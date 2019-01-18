
open Core
open Functools

module A = Ast
module F = Formula
module D = Dyn
module W = Wellformed

let rec convertExpr e = match e with
| A.Var s -> F.Var (A.name s)
| A.Val (A.Num n) -> F.Num n
| A.Val A.Nil -> F.Null
| A.Val A.C -> F.Cls
| A.Binop (e1, oper, e2) ->
    F.Binop (convertExpr e1, oper, convertExpr e2)
| A.FieldAccess (e,f) -> F.Field (convertExpr e, A.name f)

let rec convertFormula s = match s with
| A.Cmpf (e1, A.Eq, e2) ->
    begin
      match W.synthtype e1 with
      | A.Int -> F.Cmp (convertExpr e1, F.Eq, convertExpr e2)
      | _ -> F.Alias (convertExpr e1, convertExpr e2)
    end
| A.Cmpf (e1, A.Neq, e2) ->
    begin
      match W.synthtype e1 with
      | A.Int -> F.Cmp (convertExpr e1, F.Neq, convertExpr e2)
      | _ -> F.NotEq (convertExpr e1, convertExpr e2)
    end
| A.Cmpf (e1, op, e2) -> F.Cmp (convertExpr e1, op, convertExpr e2)
| A.Access (e, f) -> F.Access (convertExpr e, A.name f)
| A.Sep (s1, s2) -> F.Sep (convertFormula s1, convertFormula s2)
| A.Alpha _ -> raise @@ Failure "abstract predicates not implemented"

(* wlp : Ast.formula -> Formula.t -> Formula.t
 *
 * This is not very efficient. We reconstruct a virtual heap for alias checking
 * at pretty much every step, which requires us to traverse the entire formula
 * every time, giving us an (at minimum) runtime complexity of O(l * s) where
 * l is the number of lines and s is the number of conjunctive terms in phi.
 * In practice, there's also a factor of H, the number of static heap elements
 * due to how the heap is represented internally.
 *
 * I'm not sure we can actually be much smarter, however. One thought is to do
 * a forward pass before WLP that annotates each statement with the
 * currently-live aliases.
 *)
let rec wlp s phi = match s with
| A.Skip -> phi
| A.Seq (s1, s2) -> wlp s1 (wlp s2 phi)
  (* Expressions don't have side effects, so I think we should have referential
   * transparency. If that's the case, then
   *   phi[e/x] * x = e => phi
   * should be a tautology.
   *)
| A.Assign (t, v, e) ->
    let e' = convertExpr e in
    let phi' = F.substTerm phi (A.name v) e' in
    if D.frames phi' e' then phi' else F.Sep (F.acc e', phi')
| A.Fieldasgn (x,f,y) ->
    let phi' = F.substTerm' phi (A.name x) (A.name f) (A.name y) in
    let e = F.Field (F.Var (A.name x), A.name f) in
    if D.frames phi' e then phi' else F.Sep (F.acc e, phi')
| A.NewObj (x, c) -> F.transExpand phi (A.name x)
| A.Assert phi_a -> D.fuseAccs (convertFormula phi_a) phi

