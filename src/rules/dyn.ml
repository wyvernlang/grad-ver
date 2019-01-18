
open Core
open Functools

module F = Formula
module Heap = Virtheap
module H = Heap.Access.Set

let abspred = Failure "Abstract predicates unimplemented!"

type expop = F.expop =
  | Plus
  | Minus
  | Times
  | Div

type cmpop = F.cmpop =
  | Neq
  | Eq
  | Lt
  | Gt
  | Le
  | Ge

type term = F.term =
  | Var of string
  | Num of int
  | Null
  | Cls
  | Field of term * string
  | Binop of term * expop * term

type formula = F.t =
  | True
  | Cmp of term * cmpop * term
    (* a1 = a2 *)
  | Alias of term * term
  | NotEq of term * term
  | Alpha of term list
  | Access of term * string
  | Sep of formula * formula

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
  try
    let heap = collectAliases Heap.empty s in
    let exprCells = accessed heap e in
    let phiCells = dynFootprint heap s in
    H.is_subset exprCells ~of_:phiCells
  with Heap.Unknown t ->
    false

let fuseAccs s1 s2 =
  let (a1, r1) = F.splitAccs s1 in
  let (a2, r2) = F.splitAccs s2 in
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
      begin try
        let o = Heap.lookup heap e in
        not (H.mem fp @@ (o, f)),
        heap, H.add fp (o, f)
      with Heap.Unknown _ ->
        let heap' = Heap.add heap e in
        true, heap', H.add fp (Heap.lookup heap' e, f)
      end
  | Sep (s1, s2) ->
      let lsat, lh, lfp = go fp heap s1 in
      let rsat, rh, rfp = go lfp lh s2 in
      lsat && rsat, rh, rfp
  in
  let res, _, _ = go H.empty Heap.empty s in
  res

