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

let pp_binop = A.pp_binop
let pp_cmpop = A.pp_cmpop

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

let rec pp_term = function
  | Var s -> s
  | Num n -> Int.to_string n
  | Null -> "NULL"
  | Cls -> "C"
  | Field (e, f) -> pp_term e ^ "." ^ f
  | Binop (e1, op, e2) -> pp_term e1 ^ pp_binop op ^ pp_term e2

let rec pp_formula = function
  | True -> "true"
  | Alias (t1, t2) -> pp_term t1 ^ "==" ^ pp_term t2
  | NotEq (t1, t2) -> pp_term t1 ^ "<>" ^ pp_term t2
  | Cmp (t1, op, t2) -> pp_term t1 ^ pp_cmpop op ^ pp_term t2
  | Alpha _ -> raise abspred
  | Access (e, f) -> "acc(" ^ pp_term e ^ "." ^ f ^ ")"
  | Sep (s1, s2) -> pp_formula s1 ^ " * " ^ pp_formula s2

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

