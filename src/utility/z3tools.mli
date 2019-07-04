val symbol_of_field_reference : Ast.expression_field_reference -> string
module Z3Context :
sig
  type t = {
    context : Z3.context;
    solver : Z3.Solver.solver;
    bool_sort : Z3.Sort.sort;
    int_sort : Z3.Sort.sort;
    object_sort : Z3.Sort.sort;
  }
  val create : unit -> t
  val isSatisfiable : t -> bool
  val addExpr : t -> Z3.Expr.expr -> unit
  val sort_of_type : t -> Ast.type_ -> Z3.Sort.sort
  val makeBoolVal : t -> bool -> Z3.Expr.expr
  val makeBoolConst : t -> Ast.id -> Z3.Expr.expr
  val makeAnd : t -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
  val makeOr : t -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
  val makeNeq : t -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
  val makeEq : t -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
  val makeIntVal : t -> int -> Z3.Expr.expr
  val makeIntConst : t -> Ast.id -> Z3.Expr.expr
  val makeAdd : t -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
  val makeSub : t -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
  val makeMul : t -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
  val makeDiv : t -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
  val makeLt : t -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
  val makeGt : t -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
  val makeLe : t -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
  val makeGe : t -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
  val makeObjectConst : t -> string -> Z3.Expr.expr
  val makePredicateFunc : t -> Ast.predicate -> Z3.FuncDecl.func_decl
  val makePredicateAppl : t -> Z3.FuncDecl.func_decl -> Z3.Expr.expr list -> Z3.Expr.expr
end
