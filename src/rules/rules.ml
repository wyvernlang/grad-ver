
open Core
open Functools

module A = Ast
module F = Formula
module W = Wellformed

exception Unsat

type 'a cont = {k: 'b . 'b F.t -> 'a}

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

let mkEq e1 e2 = convertFormula (A.Cmpf (e1, A.Eq, e2))

module MakeWLP(S : Sat.S) = struct
  module I = Idf.MakeIDF(S)
  open I

  let sat = function
    | F.Static phi -> Idf.selfFramed phi && S.sat phi
    | F.Gradual phi -> S.sat phi

  (* wlp : Ast.formula -> Formula.t -> Formula.t
   *
   * See design/wlp-defs.pdf. In most cases, we elide checking whether the
   * result is self-framed (they certainly are so by construction).
   *
   * This is not very efficient. We need to, at minimum, reconstruct a virtual
   * heap for alias checking at pretty much every step, which requires us to
   * traverse the entire formula every time, giving us an (at minimum) runtime
   * complexity of O(l * s) where l is the number of lines and s is the number
   * of conjunctive terms in phi.
   *
   * In practice, there's also a factor of log H, the number of static heap
   * elements due to how the heap is represented internally.
   *
   * I'm not sure we can actually be much smarter, however. One thought is to do
   * a forward pass before WLP that annotates each statement with the
   * currently-live aliases.
   *
   * TODO: track line information to report where static checking fails
   *)
  let rec staticWLP s ((F.Static phi) as p) =
    let wlp = staticWLP in
    Precise.(
      match s with
      | A.Skip -> p
      | A.Seq (s1, s2) -> wlp s1 (wlp s2 p)
      | A.Declare (t, s) ->
          if Set.mem (F.freeVars phi) (A.name s) then p
          else raise F.Unsat
      | A.Assign (v, e) ->
          let e' = convertExpr e in
          let ee = F.Cmp (e', F.Eq, e') in
          let phi' = F.substTerm phi (A.name v) e' in
          let frame = Idf.minFramePhi ee in
          let result =
            (* The need to annotate "F.Static" is boilerplate, signifying that
             * phi' and frame are fully precise formulas. I thought to write a
             * wrapper for readability, but we wouldn't be able to do the same
             * for gradual implication so I decided not to for consistency.
             *)
            if F.Static phi' => F.Static frame then phi'
            else
              let phi_acc = Idf.minFramePhi (F.Sep (phi', ee)) in
              F.Sep (phi_acc, phi')
          in
          let c1 = F.Static (F.Sep (result, mkEq (A.Var v) e)) in
          if S.sat result && c1 => p then F.Static result else raise Unsat
      | A.Fieldasgn (x,f,y) ->
          let phi' = F.substTerm' phi (A.name x) (A.name f) (A.name y) in
          let e = F.Field (F.Var (A.name x), A.name f) in
          let result =
            if F.Static phi' => F.Static (F.acc e)
              then phi'
              else F.Sep (F.acc e, phi')
          in
          let c1 =
            F.Static (F.Sep(result, mkEq (A.FieldAccess(A.Var x,f)) (A.Var y)))
          in
          if S.sat result && c1 => p then F.Static result else raise Unsat
      | A.NewObj (x, c) ->
          let result = F.transExpand phi (A.name x) in
          if S.sat result then F.Static result else raise Unsat
      | A.Assert phi_a ->
          let phi_a' = convertFormula phi_a in
          let _, cls = F.splitAccs phi in
          let _, cls_a = F.splitAccs phi_a' in
          let result =
            F.Sep (
              Idf.minFramePhi (F.Sep (cls_a, cls)),
              F.Sep (cls_a, cls)
            )
          in
          if F.Static result => F.Static phi_a'
          && F.Static result => p
          && S.sat result
            then F.Static result
            else raise Unsat
    )

  (* Because of our fancy type-level precision/imprecision stuff, we
   * unfortunately are required to write this (stylistically) a little weirdly.
   *
   * TODO: gradual function calls
   *)
  let rec gradualWLP : type a . A.stmt -> a F.t -> 'b cont -> 'b =
    fun s p {k} -> match p with
    | F.Static phi -> k (staticWLP s p)
    | F.Gradual phi ->
        match s with
        | A.Skip -> k p
        | A.Seq (s1, s2) ->
            gradualWLP s2 p
            {k=fun p' -> gradualWLP s1 p' {k=k}}
        | A.Declare _ | A.Assign _ | A.Fieldasgn _ | A.NewObj _ | A.Assert _ ->
            (* The compiler will give a warning on this line, but it's actually
             * exhaustive, because staticWLP can only return a precise formula
             *)
            let (F.Static p') = staticWLP s (F.Static phi) in
            k @@ F.Gradual p'
end

