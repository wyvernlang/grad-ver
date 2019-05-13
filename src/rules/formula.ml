(* Contains all the logic for access predicates, etc.
 *
 * TODO: Refactoring -- The organization of this stuff is just generally a mess
 *
 *   Simiilarly, the only reason we don't have the Idf.satisfiable function in
 *   this file is that we need it to rely on both Formula and SAT.
 *
 * Formulas are values of type 'a t, where the 'a can be `precise' or
 * `imprecise'. This done at the type level so we can ensure that we don't
 * call static WLP on a gradual formula (etc). This does lead to complications
 * elsewhere (such as needing to write gradualWLP in continuation passing
 * style), as well as generally more complex code.
 *)

open Core
open Functools

module A = Ast

module LM = List.Assoc

let abspred = Failure "Abstract predicates unimplemented!"

exception Unsat
exception Malformed

type expop = A.expop =
  | Plus
  | Minus
  | Times
  | Div [@@deriving sexp, compare]

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
  | Old of string
  | Result
  | Binop of term * expop * term [@@deriving sexp, compare]

let rec termEq t1 t2 =
  match t1, t2 with
  | Var s1, Var s2 -> String.equal s1 s2
  | Num n1, Num n2 -> Int.equal n1 n2
  | Null, Null -> true
  | Field (b1, f1), Field (b2, f2) -> termEq b1 b2 && String.equal f1 f2
  | Binop (l1, op1, r1), Binop (l2, op2, r2) ->
      termEq l1 l2 && phys_equal op1 op2 && termEq r1 r2
  | Cls, Cls -> false
  | Result, Result -> true
  | Old s1, Old s2 -> String.equal s1 s2
  | _ -> false

type formula = True
             | Cmp of term * cmpop * term
               (* a1 = a2 *)
             | Alias of term * term
             | NotEq of term * term
             | Alpha of term list
             | Access of term * string
             | Sep of formula * formula

type precise
type imprecise

type _ t =
  | Gradual : formula -> imprecise t
  | Static : formula -> precise t

let rec pp_term = function
  | Var s -> s
  | Num n -> Int.to_string n
  | Null -> "NULL"
  | Cls -> "C"
  | Field (e, f) -> pp_term e ^ "." ^ f
  | Binop (e1, op, e2) -> pp_term e1 ^ pp_binop op ^ pp_term e2
  | Result -> "__result"
  | Old s -> "old(" ^ s ^ ")"

let rec pp_formula = function
  | True -> "true"
  | Alias (t1, t2) -> pp_term t1 ^ "==" ^ pp_term t2
  | NotEq (t1, t2) -> pp_term t1 ^ "<>" ^ pp_term t2
  | Cmp (t1, op, t2) -> pp_term t1 ^ pp_cmpop op ^ pp_term t2
  | Alpha _ -> raise abspred
  | Access (e, f) -> "acc(" ^ pp_term e ^ "." ^ f ^ ")"
  | Sep (s1, s2) -> pp_formula s1 ^ " * " ^ pp_formula s2

let pp_phi (type a) : a t -> string = function
  | Static f -> pp_formula f
  | Gradual f -> "? * (" ^ pp_formula f ^ ")"

let staticPart ((Gradual phi) : imprecise t) = Static phi

let rec subst e v e' =
  if termEq e v then e' else
  match e with
  | Var _ | Num _ | Null | Cls | Result | Old _ -> e
  | Field (e'', f) -> Field (subst e'' v e', f)
  | Binop (e1, op, e2) -> Binop (subst e1 v e', op, subst e2 v e')

(* substVar e v e' = [e'/v]e *)
let rec substVar e v e' = subst e (Var v) e'

(* substTerm s v e = [e/v]s *)
let rec substTerm s v e = match s with
| True -> s
| Cmp (e1, op, e2) -> Cmp (substVar e1 v e, op, substVar e2 v e)
| Alias (e1, e2) -> Alias (substVar e1 v e, substVar e2 v e)
| NotEq (e1, e2) -> NotEq (substVar e1 v e, substVar e2 v e)
| Alpha _ -> raise abspred
| Access (e', f) -> Access (substVar e' v e, f)
| Sep (s1, s2) -> Sep (substTerm s1 v e, substTerm s2 v e)

let rec substAcc e x f y = subst e (Field (Var x, f)) (Var y)

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

let rec substRes s e = match s with
| True -> s
| Cmp (e1, op, e2) -> Cmp (subst e1 Result e, op, subst e2 Result e)
| Alias (e1, e2) -> Alias (subst e1 Result e, subst e2 Result e)
| NotEq (e1, e2) -> NotEq (subst e1 Result e, subst e2 Result e)
| Alpha _ -> raise abspred
| Access (e', f') -> Access (subst e' Result e, f')
| Sep (s1, s2) -> Sep (substRes s1 e, substRes s2 e)

let rec acc t = match t with
| Var _ -> True
| Num _ -> True
| Null -> True
| Cls -> True
| Field (e, f) -> Access (e, f)
| Binop _ -> True
| Result -> True
| Old _ -> True

let rec fv_exp e = match e with
| Var s -> String.Set.singleton s
| Null | Cls | Num _ | Result -> String.Set.empty
| Old s -> String.Set.singleton s
| Field (e, f) -> fv_exp e
| Binop (e1, _, e2) -> Set.union (fv_exp e1) (fv_exp e2)

let rec freeVars f = match f with
| True -> String.Set.empty
| Cmp (e1, _, e2) | NotEq (e1, e2)
| Alias (e1, e2) -> Set.union (fv_exp e1) (fv_exp e2)
| Access (e, f) -> fv_exp e
| Sep (s1, s2) -> Set.union (freeVars s1) (freeVars s2)
| Alpha _ -> raise abspred

let rec accContains e' (e,f) =
  termEq e' (Field (e,f)) ||
  match e with
  | Field (e'', f') -> accContains e'' (e,f)
  | _ -> false

let rec transExpand s x =
  let rec rmExpr ctx e = match e with
  | Var _ -> List.exists ~f:(termEq e) ctx
  | Binop (e1, _, e2) -> rmExpr ctx e1 || rmExpr ctx e2
  | Null -> false
  | Cls -> false
  | Num _ -> false
  | Old _ -> false
  | Result -> false
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
| Access (e,f) -> [(e,f)], True
| Sep (s1, s2) ->
    let (a1, r1) = splitAccs s1 in
    let (a2, r2) = splitAccs s2 in
    a1 @ a2, Sep (r1, r2)

module Access = struct
  include Comparable.Make
    (struct
      type t = term * string [@@deriving sexp]

      let compare (e1, f1) (e2, f2) =
        (* if terms are literally equal  *)
        if termEq (Field (e1, f1)) (Field (e2, f2)) then 0 else
        (* if e1,f1 is e1'.f2,f1 *)
        if accContains e1 (e2, f2) then 1 else
        (* if e2,f2 is e2'.f1,f2 *)
        if accContains e2 (e1, f1) then -1 else
          (* otherwise, who cares *)
          compare_term e1 e2
    end)
end

module Term = Comparable.Make(
  struct
    type t = term [@@deriving compare, sexp]
  end)

