(* Contains all the logic for access predicates, etc.
 *
 * TODO: Conversion to Z3, checking of access predicates
 *)

open Core
open Functools

module A = Ast

let abspred = Failure "Abstract predicates unimplemented!"

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

type access = term * string

let rec accessEq (t1, s1) (t2, s2) = termEq t1 t2 && String.equal s1 s2

type t = True
       | Cmp of term * cmpop * term
         (* a1 = a2 *)
       | Alias of access * access
       | NotEq of term * term
       | Alpha of term list
       | Access of access
       | Sep of t * t

let rec acc t = match t with
| Var _ -> True
| Num _ -> True
| Null -> True
| Cls -> True
| Field (e, f) -> Access (e, f)
| Binop _ -> True

let rec staticFootprint s = match s with
| True | Cmp _ | NotEq _ | Alias _ -> []
| Alpha _ -> raise abspred
| Access a -> [a]
| Sep (t1, t2) ->
    Listutil.union ~eq:accessEq (staticFootprint t1) (staticFootprint t2)

(* XXX - name is misleading, we should be cleaner than just returning the
 *       alias association list
 *)
let rec dynFootprint ctx s = match s with
| True | Cmp _ | NotEq _ -> ctx, []
| Alpha _ -> raise abspred
| Alias (a1, a2) ->
    Listutil.changeWhen ~p:(List.exists ~f:(accessEq a2) @< snd)
    ~f:(fun (a,als) -> (a, Listutil.union ~eq:accessEq [a1] als)) ctx, []
| Sep (s1, s2) ->
    let lctx, r1 = dynFootprint ctx s1 in
    let rctx, r2 = dynFootprint lctx s2 in
    rctx, Listutil.union ~eq:accessEq r1 r2
| Access a ->
    match List.find ~f:(List.exists ~f:(accessEq a) @< snd) ctx with
    | Some (a', ls) -> ctx, [a']
    | None -> (a,[a])::ctx, [a]

let aliases heap ft = match List.find ~f:(accessEq ft @< fst) heap with
| Some (a, als) -> als
  (* misformed footprint/heap pair *)
| None -> assert false

let rec substVar' v t t' = match t with
| Var v' -> if String.equal v' @@ A.name v then t' else t
| Binop (l, oper, r) -> Binop (substVar' v l t', oper, substVar' v r t')
| Num _ -> t
  (* TODO: field substitution *)
| Field (e, f) -> Field (substVar' v e t', f)
| Null -> t
| Cls -> t

let rec substVar s v t = match s with
| True -> True
| Cmp (l, oper, r) -> Cmp (substVar' v l t, oper, substVar' v r t)
| Alpha _ -> raise abspred
| Access (e,f) -> Access (substVar' v e t, f)
| Alias ((e1, f1),(e2, f2)) ->
    Alias ((substVar' v e1 t, f1),(substVar' v e2 t, f2))
| NotEq (e1, e2) -> NotEq (substVar' v e1 t, substVar' v e2 t)
| Sep (s1, s2) -> Sep (substVar s1 v t, substVar s2 v t)

let rec substAcc' v t t' = match t with
| Var v -> t
| Binop (l, oper, r) -> Binop (substAcc' v l t', oper, substAcc' v r t')
| Num _ -> t
| Field (e, f) -> if accessEq v (e,f) then t' else Field (substAcc' v e t', f)
| Null -> t
| Cls -> t

let rec substAcc s v t = match s with
| True -> True
| Cmp (l, oper, r) -> Cmp (substAcc' v l t, oper, substAcc' v r t)
| Alpha _ -> raise abspred
| Alias ((e1, f1),(e2, f2)) ->
    Alias ((substAcc' v e1 t, f1),(substAcc' v e2 t, f2))
| NotEq (e1, e2) -> NotEq (substAcc' v e1 t, substAcc' v e2 t)
| Access (e,f) -> Access (substAcc' v e t, f)
| Sep (s1, s2) -> Sep (substAcc s1 v t, substAcc s2 v t)

let accesses phi (e : term) =
  let heap, footprint = dynFootprint [] phi in
  match e with
  | Num _ | Binop _
  | Var _ | Null | Cls -> true
  | Field (e, f) ->
      let acc = (e,f) in
      List.exists ~f:(accessEq acc) @@
      List.concat_map ~f:(aliases heap) footprint

let rec sat _ = raise @@ Failure "TODO"


