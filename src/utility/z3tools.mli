type z3ex = Z3.Expr.expr

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
  val isSatisfiableWith : t -> z3ex list -> bool
  val isUnsatisfiableWith : t -> z3ex list -> bool
  val addExpr : t -> z3ex -> unit
  val addExprs : t -> z3ex list -> unit
  val sort_of_type : t -> Ast.type_ -> Z3.Sort.sort
  val makeBoolVal : t -> bool -> z3ex
  val makeBoolConst : t -> Ast.id -> z3ex
  val makeAnd : t -> z3ex -> z3ex -> z3ex
  val makeOr : t -> z3ex -> z3ex -> z3ex
  val makeNeq : t -> z3ex -> z3ex -> z3ex
  val makeEq : t -> z3ex -> z3ex -> z3ex
  val makeNot : t -> z3ex -> z3ex
  val makeIntVal : t -> int -> z3ex
  val makeIntConst : t -> Ast.id -> z3ex
  val makeAdd : t -> z3ex -> z3ex -> z3ex
  val makeSub : t -> z3ex -> z3ex -> z3ex
  val makeMul : t -> z3ex -> z3ex -> z3ex
  val makeDiv : t -> z3ex -> z3ex -> z3ex
  val makeLt : t -> z3ex -> z3ex -> z3ex
  val makeGt : t -> z3ex -> z3ex -> z3ex
  val makeLe : t -> z3ex -> z3ex -> z3ex
  val makeGe : t -> z3ex -> z3ex -> z3ex
  val makeObjectConst : t -> string -> z3ex
  val makeFieldConst :
    t -> z3ex -> Ast.id -> Ast.type_ -> z3ex
  val makePredicateFuncDecl : t -> Ast.predicate -> Z3.FuncDecl.func_decl
  val makePredicateCheck :
    'a -> Z3.FuncDecl.func_decl -> z3ex list -> z3ex
  val makeAccessCheck : t -> z3ex -> z3ex
end
