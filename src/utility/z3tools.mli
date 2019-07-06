type z3expr = Z3.Expr.expr

module Z3Context :
sig
  type t = {
    context : Z3.context;
    solver : Z3.Solver.solver;
    bool_sort : Z3.Sort.sort;
    int_sort : Z3.Sort.sort;
    object_sort : Z3.Sort.sort;
    field_sort : Z3.Sort.sort;
    make_fieldref_funcdecl : Z3.Sort.sort -> Z3.FuncDecl.func_decl;
    acc_funcdecl : Z3.FuncDecl.func_decl;
  }
  val create : unit -> t
  val evaluateSolverStatus : Z3.Solver.status -> bool
  val isSatisfiable : t -> bool
  val isSatisfiableWith : t -> z3expr list -> bool
  val isUnsatisfiableWith : t -> z3expr list -> bool
  val addExpr : t -> z3expr -> unit
  val addExprs : t -> z3expr list -> unit
  val sort_of_type : t -> Ast.type_ -> Z3.Sort.sort
  val makeBoolVal : t -> bool -> z3expr
  val makeBoolConst : t -> Ast.id -> z3expr
  val makeAnd : t -> z3expr -> z3expr -> z3expr
  val makeOr : t -> z3expr -> z3expr -> z3expr
  val makeNeq : t -> z3expr -> z3expr -> z3expr
  val makeEq : t -> z3expr -> z3expr -> z3expr
  val makeNot : t -> z3expr -> z3expr
  val makeIntVal : t -> int -> z3expr
  val makeIntConst : t -> Ast.id -> z3expr
  val makeAdd : t -> z3expr -> z3expr -> z3expr
  val makeSub : t -> z3expr -> z3expr -> z3expr
  val makeMul : t -> z3expr -> z3expr -> z3expr
  val makeDiv : t -> z3expr -> z3expr -> z3expr
  val makeLt : t -> z3expr -> z3expr -> z3expr
  val makeGt : t -> z3expr -> z3expr -> z3expr
  val makeLe : t -> z3expr -> z3expr -> z3expr
  val makeGe : t -> z3expr -> z3expr -> z3expr
  val makeObjectConst : t -> string -> z3expr
  val makeFieldConst :
    t -> z3expr -> Ast.id -> Ast.type_ -> z3expr
  val makePredicateFuncDecl : t -> Ast.predicate -> Z3.FuncDecl.func_decl
  val makePredicateCheck :
    'a -> Z3.FuncDecl.func_decl -> z3expr list -> z3expr
  val makeAccessCheck : t -> z3expr -> z3expr
end
