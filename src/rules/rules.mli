
val convertExpr : Ast.expr -> Formula.term
val convertFormula : Ast.formula -> Formula.t

module MakeWLP(S : Sat.S) : sig
  val wlp : Ast.stmt -> Formula.t -> Formula.t

  val verify : Formula.t -> Formula.t -> bool
end

