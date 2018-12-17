(* Contains all the logic for access predicates, etc.
 *
 * TODO: Refactoring -- The organization of this stuff is just generally a mess
 *
 *   The heap stuff should go in its own module, but it's mutually dependent on
 *   this module. We can fix this by separating the logic itself from the basic
 *   type definitions.
 *
 *   Simiilarly, the only reason we don't have the Idf.satisfiable function in
 *   this file is that we need it to rely on both Formula and SAT.
 *)

open Core
open Functools

module A = Ast

module LM = List.Assoc

let abspred = Failure "Abstract predicates unimplemented!"

exception Unsat

type expop = A.expop =
  | Plus
  | Minus
  | Times
  | Div

type cmpop = A.cmpop =
  | Neq
  | Eq
  | Lt
  | Gt
  | Le
  | Ge

(* TODO: method calls *)
type term =
  | Var of string
  | Num of int
  | Null
  | Cls
  | Field of term * string
  | Binop of term * expop * term

let rec termEq t1 t2 =
  match t1, t2 with
  | Var s1, Var s2 -> String.equal s1 s2
  | Num n1, Num n2 -> Int.equal n1 n2
  | Null, Null -> true
  | Field (b1, f1), Field (b2, f2) -> termEq b1 b2 && String.equal f1 f2
  | Binop (l1, op1, r1), Binop (l2, op2, r2) ->
      termEq l1 l2 && phys_equal op1 op2 && termEq r1 r2
  | Cls, Cls -> false
  | _ -> false

type t = True
       | Cmp of term * cmpop * term
         (* a1 = a2 *)
       | Alias of term * term
       | NotEq of term * term
       | Alpha of term list
       | Access of term * string
       | Sep of t * t

(* Heap objects are tracked using unique IDs.
 *
 * We can ensure that the ID of any given heap object is unique by hiding a
 * global counter behind the module interface and incrementing it only when
 * we know we're allocating more.
 *
 * The heap itself is tracked via an association list of (t, c) where t is the
 * term (either `Var s` or `Field (e, f)`) and c is the associated object on
 * the heap.
 *
 * This should be in its own module, but we would need to move all the
 * heap-dependent functions into a separate module which is kind of annoying so
 * they'll all live here for now.
 *)
module type HEAP = sig
  exception Unknown of term
  type cell
  type t

  module Access : Comparable.S with type t = cell * string
  module Cell : Comparable.S with type t = cell

  val cellId : cell -> int
  val cellEq : cell -> cell -> bool

  val empty : t

  val add : t -> term -> t
  val add_new : t -> term -> t
  val alias : t -> term -> term -> t

  val lookup : t -> term -> cell
end

module Heap : HEAP = struct
  exception Unknown of term
  let counter = ref 0

  type cell = int
  type t = (term * cell) list

  module Access = struct
    type t = cell * string
    include Comparable.Make(
      struct type t = int * string [@@deriving compare, sexp] end
    )
  end

  module Cell = Int

  let fresh () =
    let x = !counter in
    counter := !counter + 1; x

  let cellId i = i
  let cellEq = Int.equal

  let empty = []

  let add ctx s = LM.add ctx ~equal:termEq s @@ fresh ()

  let lmlookup = LM.find ~equal:termEq

  let add_new ctx s =
    match lmlookup ctx s with
    | None -> add ctx s
    | Some _ -> ctx

  let alias ctx s1 s2 =
    match lmlookup ctx s2 with
    | Some i -> LM.add ctx ~equal:termEq s1 i
    | None ->
        let i = fresh () in
        LM.add (LM.add ctx ~equal:termEq s1 i) ~equal:termEq s2 i

  let lookup ctx s =
    match lmlookup ctx s with
    | Some i -> i
    | None -> raise @@ Unknown s

  let same ctx s1 s2 =
    match lmlookup ctx s1, lmlookup ctx s2 with
    | Some a, Some b -> cellEq a b
    | None, _ -> raise @@ Unknown s1
    | _, None -> raise @@ Unknown s2
end

module H = Heap.Access.Set

(* substVar e v e = e[e'/v] *)
let rec substVar e v e' = match e with
| Var v' -> if String.equal v v' then e' else e
| Num _ -> e
| Null -> e
| Cls -> e
| Field (e, f) -> Field (substVar e v e', f)
| Binop (e1, op, e2) -> Binop (substVar e1 v e', op, substVar e2 v e')

(* substTerm s v e = s[e/v] *)
let rec substTerm s v e = match s with
| True -> s
| Cmp (e1, op, e2) -> Cmp (substVar e1 v e, op, substVar e2 v e)
| Alias (e1, e2) -> Alias (substVar e1 v e, substVar e2 v e)
| NotEq (e1, e2) -> NotEq (substVar e1 v e, substVar e2 v e)
| Alpha _ -> raise abspred
| Access (e', f) -> Access (substVar e' v e, f)
| Sep (s1, s2) -> Sep (substTerm s1 v e, substTerm s2 v e)

let rec substAcc e x f y = match e with
| Var _ -> e
| Num _ -> e
| Null -> e
| Cls -> e
| Field (Var v, f') ->
    if String.equal x v && String.equal f f' then Var y else e
| Field (e, f') -> Field (substAcc e x f y, f')
| Binop (e1, op, e2) -> Binop (substAcc e1 x f y, op, substAcc e2 x f y)

let rec substTerm' s x f y = match s with
| True -> s
| Cmp (e1, op, e2) -> Cmp (substAcc e1 x f y, op, substAcc e2 x f y)
| Alias (e1, e2) -> Alias (substAcc e1 x f y, substAcc e2 x f y)
| NotEq (e1, e2) -> NotEq (substAcc e1 x f y, substAcc e2 x f y)
| Alpha _ -> raise abspred
| Access (Var v, f') ->
    if String.equal x v && String.equal f f' then True else s
| Access (e, f') -> Access (substAcc e x f y, f')
| Sep (s1, s2) -> Sep (substTerm' s1 x f y, substTerm' s2 x f y)

let rec acc t = match t with
| Var _ -> True
| Num _ -> True
| Null -> True
| Cls -> True
| Field (e, f) -> Access (e, f)
| Binop _ -> True

let rec dynFootprint heap s = match s with
| True -> H.empty
| Cmp _ -> H.empty
| Alias _ -> H.empty
| NotEq _ -> H.empty
| Alpha _ -> raise abspred
| Access (e, f) -> H.singleton (Heap.lookup heap e, f)
| Sep (s1, s2) -> H.union (dynFootprint heap s1) (dynFootprint heap s2)

(* XXX - Currently, the WLP rules talk about phi[e/x] => acc(e), but this
 *       doesn't obviously make sense if we have e = e1.f + e2.f (etc). For the
 *       sake of my sanity, I'm going to assume that
 *
 *       acc(e1.f OP e2.f) -- = acc(e1.f) * acc(e2.f) if e1 != e2
 *                         \_ = acc(e1.f)             otherwise
 *)
let rec accessed heap e = match e with
| Var _ -> H.empty
| Num _ -> H.empty
| Null -> H.empty
| Cls -> H.empty
| Field (e, f) -> H.singleton (Heap.lookup heap e, f)
| Binop (e1, _, e2) ->
    (* Because we dereference e1 and e2 in the Field case, this will
     * automatically join the two heap objects if e1 and e2 are aliases.
     *)
    H.union (accessed heap e1) (accessed heap e2)

let rec collectAliases heap s = match s with
| True -> heap
| Cmp _ -> heap
| Alias (e1, e2) -> Heap.alias heap e1 e2
| NotEq _ -> heap
| Alpha _ -> raise abspred
| Access (e, f) -> Heap.add_new heap e
| Sep (s1, s2) ->
    let h' = collectAliases heap s1 in
    collectAliases h' s2

let rec framed heap fp e = match e with
| Var _ -> true
| Num _ -> true
| Null -> true
| Cls -> true
| Field (e, f) -> H.mem fp (Heap.lookup heap e, f)
| Binop (e1, _, e2) -> framed heap fp e1 && framed heap fp e2

let rec dframe heap fp s = match s with
| True -> true
| Cmp (e1, _, e2) -> framed heap fp e1 && framed heap fp e2
| Alias (e1, e2) -> framed heap fp e1 && framed heap fp e2
| NotEq (e1, e2) -> framed heap fp e1 && framed heap fp e2
| Alpha _ -> raise abspred
| Access (e, f) -> framed heap fp e
| Sep (s1, s2) ->
    dframe heap fp s1 && dframe heap (H.union fp @@ dynFootprint heap s1) s2

(* This seems to be the best way to express phi => acc(e) *)
let frames s e =
  let heap = collectAliases Heap.empty s in
  let exprCells = accessed heap e in
  let phiCells = dynFootprint heap s in
  H.is_subset exprCells ~of_:phiCells

let rec transExpand s x =
  let rec rmExpr ctx e = match e with
  | Var v -> List.exists ~f:(termEq e) ctx
  | Binop (e1, _, e2) -> rmExpr ctx e1 || rmExpr ctx e2
  | Null -> false
  | Cls -> false
  | Num _ -> false
  | Field (Field(e, f) as e', _) ->
      if rmExpr ctx e then raise Unsat
                      else rmExpr ctx e'
  | Field (e, f) -> rmExpr ctx e
  in
  let rec go ctx s = match s with
  | True -> True, ctx
  | NotEq (e1, e2)
  | Cmp (e1, Neq, e2) ->
      if rmExpr ctx e1 || rmExpr ctx e2 then True, ctx
                                        else s, ctx
  | Cmp (e1, _, e2) ->
      if rmExpr ctx e1 || rmExpr ctx e2 then raise Unsat
                                        else s, ctx
  | Alias (e1, e2) ->
      if List.exists ~f:(termEq e2) ctx
        then True, e1::ctx
        else if List.exists ~f:(termEq e1) ctx then raise Unsat
                                               else s, ctx
  | Alpha _ -> raise abspred
  | Access (Field (e, f') as e', f) ->
      if rmExpr ctx e then raise Unsat
                      else if rmExpr ctx e' then True, ctx
                                            else s, ctx
  | Access (e, f) ->
      if rmExpr ctx e then True, ctx
                      else s, ctx
  | Sep (s1, s2) ->
      let (s1', ctx') = go ctx s1 in
      let (s2', ctx'') = go ctx' s2 in
      Sep (s1', s2'), ctx''
  in
  fst @@ go [Var x] s

let rec splitAccs s = match s with
| True -> [], True
| Cmp _ -> [], s
| Alias _ -> [], s
| NotEq _ -> [], s
| Alpha _ -> raise abspred
| Access (e,f) -> [s], True
| Sep (s1, s2) ->
    let (a1, r1) = splitAccs s1 in
    let (a2, r2) = splitAccs s2 in
    a1 @ a2, Sep (r1, r2)

let fuseAccs s1 s2 =
  let (a1, r1) = splitAccs s1 in
  let (a2, r2) = splitAccs s2 in
  let heap = collectAliases Heap.empty (Sep (r1, r2)) in
  let go s (ctx, result) =
    match s with
    | Access (e, f) ->
        begin
          match Map.find ctx (Heap.lookup heap e) with
          | Some _ -> (ctx, result)
          | None -> (Map.set ctx (Heap.lookup heap e) s, Sep (s, result))
        end
    | _ -> assert false (* can't happen *)
  in
  snd @@
  List.fold_right ~f:go ~init:(Heap.Cell.Map.empty, Sep (r1, r2)) (a1 @ a2)

let rec checkAcc s =
  let rec go fp heap s = match s with
  | True -> true, heap, fp
  | Cmp (e1, _, e2) ->
      H.is_subset (H.union (accessed heap e1) (accessed heap e2)) ~of_:fp,
      heap, fp
  | Alias (e1, e2) ->
      H.is_subset (H.union (accessed heap e1) (accessed heap e2)) ~of_:fp,
      Heap.alias heap e1 e2, fp
  | NotEq (e1, e2) ->
      H.is_subset (H.union (accessed heap e1) (accessed heap e2)) ~of_:fp,
      heap, fp
  | Alpha _ -> raise abspred
  | Access (e, f) ->
      let o = Heap.lookup heap e in
      not (H.mem fp @@ (o, f)),
      heap, H.add fp (o, f)
  | Sep (s1, s2) ->
      let lsat, lh, lfp = go fp heap s1 in
      let rsat, rh, rfp = go lfp lh s2 in
      lsat && rsat, rh, rfp
  in
  let res, _, _ = go H.empty Heap.empty s in
  res

