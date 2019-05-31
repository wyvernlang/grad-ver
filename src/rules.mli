
val convertExpr : Ast.expr -> Formula.term
val convertFormula : Ast.formula -> Formula.formula

type 'a cont = {k: 'b . 'b Formula.t -> 'a}

module MakeWLP(S : Sat.S) : sig
  val sat : 'a Formula.t -> bool

  val staticWLP :
    bool -> Ast.stmt -> Formula.precise Formula.t -> Formula.precise Formula.t

  val gradualWLP : Ast.stmt -> 'a Formula.t -> 'b cont -> 'b
end

