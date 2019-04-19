
open Core
open Functools

module A = Ast
module F = Formula
module Heap = Virtheap

let rec collectAccesses xi phi = match phi with
| F.True | F.Cmp _ | F.Alias _ | F.NotEq _ -> xi
| F.Access (e, f) -> Map.add_multi xi f e
| F.Alpha _ -> raise @@ Failure "no abstract predicates!"
| F.Sep (phi1, phi2) ->
    let xi1 = collectAccesses xi phi1 in
    collectAccesses xi1 phi2

let rec mu xi =
  match Map.max_elt xi with
  | None -> F.True
  | Some (f, es) ->
      Listutil.foldSuffixes ~init:F.True es
      ~f:(fun acc e es' ->
        F.Sep (
          List.fold_left ~f:(fun acc x -> F.Sep (acc, F.NotEq (x, e)))
          ~init:(F.NotEq (e, F.Null)) es',
          acc
        ))

let floatAccs phi =
  let accs, classic = F.splitAccs phi in
  List.sort ~compare:F.Access.ascending accs, classic

let joinFootprint =
  List.fold_left
    ~f:(fun acc (e,f) -> F.Sep (acc, F.Access (e,f)))
    ~init:F.True

(* Splits the formula into the classical part (plus anti-aliasing information
 * from access predicates) and a sorted list of access predicates.
 *
 * XXX - rename this
 *)
let accImplies phi =
  let xi = collectAccesses String.Map.empty phi in
  let (accs, classic) = floatAccs phi in
  accs, F.Sep (mu xi, classic)

(* Constructs a list of sets corresponding to aliased heap values *)
let rec collectAliases cs = function
  | F.True | F.Cmp _ | F.NotEq _ -> cs
  | F.Alpha _ -> raise @@ Failure "abstract predicates unimplemented"
  | F.Access _ -> cs
  | F.Sep (s1, s2) ->
      let cs' = collectAliases cs s1 in
      collectAliases cs' s2
  | F.Alias (e1, e2) ->
      (* this could be cleaner *)
      match
        Listutil.updateWhen
          ~p:(Set.exists ~f:(fun e -> F.termEq e e1 || F.termEq e e2))
          ~f:(fun s -> Set.add (Set.add s e1) e2)
          cs
      with
      | None -> Set.add (F.Term.Set.singleton e1) e2::cs
      | Some cs' -> cs'

let expandAliases cs (e, f) =
  F.Access.Set.(
    match List.find ~f:(fun s -> Set.mem s e) cs with
    | None -> singleton (e,f)
    | Some s -> map ~f:(fun e' -> (e', f)) s
  )

let rec extractObjsExp = F.(function
  | Var _ | Num _ | Null | Cls -> Access.Set.empty
  | Field (e, f) -> Set.add (extractObjsExp e) (e,f)
  | Binop (e1, _, e2) -> Set.union (extractObjsExp e1) (extractObjsExp e2)
)

let rec extractObjsPhi = function
  | F.True -> F.Access.Set.empty
  | F.Cmp (e1, _, e2) | F.Alias (e1, e2) | F.NotEq (e1, e2) ->
      Set.union (extractObjsExp e1) (extractObjsExp e2)
  | F.Alpha _ -> raise F.abspred
  | F.Access (e, f) -> Set.add (extractObjsExp e) (e,f)
  | F.Sep (s1, s2) -> Set.union (extractObjsPhi s1) (extractObjsPhi s2)

(* Compute a minimal formula phi_acc such that phi_acc * phi is self-framed. *)
let minFramePhi = function phi ->
  let objs = Set.to_list @@ extractObjsPhi phi in
  let aliases = collectAliases [] phi in
  let aliasedObjs = List.map ~f:(expandAliases aliases) objs in
  let frame, _ = F.splitAccs phi in
  let unframed =
    List.fold_right aliasedObjs ~init:[] ~f:(fun cls acc ->
      if Set.exists cls
        ~f:(fun (e,f) ->
          List.exists frame
          ~f:(fun (e', f') -> F.termEq e e' && String.equal f f')
        )
      then acc
      else Set.min_elt_exn cls :: acc
    )
  in
  List.fold unframed
    ~f:(fun acc (e,f) -> F.Sep (acc, F.Access (e,f)))
    ~init:F.True

let selfFramed = function phi ->
  let aliases = collectAliases [] phi in
  let frame, _ = F.splitAccs phi in
  let owned =
    List.fold ~init:F.Access.Set.empty ~f:Set.union
    @@ List.map ~f:(expandAliases aliases) frame
  in
  let objs = extractObjsPhi phi in
  Set.is_subset objs ~of_:owned

module type IMPLIES = sig
  type 'a t1
  type t2
  val (=>) : 'a t1 Formula.t -> t2 Formula.t -> bool
end

module MakeIDF(S : Sat.S) = struct
  module Precise = struct
    type 'a t1 = Formula.precise
    type t2 = Formula.precise

    let (=>) (F.Static phi1) (F.Static phi2) =
      let (acc1, cls1) = accImplies phi1 in
      let (acc2, cls2) = accImplies phi2 in
      let cs = collectAliases [] cls1 in
      let acc1s = List.map ~f:(expandAliases cs) acc1 in
      List.fold_left ~init:true acc2
        ~f:(fun rslt acc ->
          rslt && List.exists ~f:(fun s -> Set.mem s acc) acc1s)
      &&
      S.valid cls1 cls2
  end

  module Imprecise = struct
    type 'a t1 = 'a
    type t2 = Formula.imprecise

    let (=>) (type a) (p1 : a t1 Formula.t) (F.Gradual phi2) = match p1 with
    | (F.Static phi1) -> raise @@ Failure "TODO"
    | (F.Gradual phi1) ->
        Precise.(
          let (acc1, cls1) = accImplies phi1 in
          let (acc2, cls2) = accImplies phi2 in
          let cls = F.Sep (cls1, cls2) in
          let phi_acc = minFramePhi cls in
          (F.Static (F.Sep (phi_acc, cls)) => F.Static phi1)
          &&
          (F.Static (F.Sep (phi_acc, cls)) => F.Static phi2)
        )
  end
end

