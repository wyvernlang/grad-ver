open Core
open Z3
open Functools
open Utility

open Ast
open Wellformed
open Aliasing

type z3expr = Expr.expr

module Z3Context =
struct

  type t = {
    context                : context;
    solver                 : Solver.solver;
    bool_sort              : Sort.sort;
    int_sort               : Sort.sort;
    object_sort            : Sort.sort;
    field_sort             : Sort.sort;
    make_fieldref_funcdecl : Sort.sort -> FuncDecl.func_decl;
    acc_funcdecl           : FuncDecl.func_decl;
  }

  let create () : t =
    let cfg         = [] in
    let ctx         = mk_context cfg in
    let bool_sort   = Boolean.mk_sort ctx in
    let obj_sort    = Sort.mk_uninterpreted_s ctx "object" in
    let fld_sort    = Sort.mk_uninterpreted_s ctx "field" in
    {
      context                = ctx;
      solver                 = Solver.mk_simple_solver ctx;
      bool_sort              = bool_sort;
      int_sort               = Arithmetic.Integer.mk_sort ctx;
      object_sort            = obj_sort;
      field_sort             = fld_sort;
      make_fieldref_funcdecl = FuncDecl.mk_func_decl_s ctx "fieldref" [obj_sort; fld_sort];
      acc_funcdecl           = FuncDecl.mk_func_decl_s ctx "acc" [obj_sort; fld_sort] bool_sort;
    }

  let evaluateSolverStatus : Solver.status -> bool = function
    | SATISFIABLE   -> true
    | UNSATISFIABLE -> false
    | UNKNOWN       -> false (* TODO: not sure when this happens... *)

  let isSatisfiable z3ctx               : bool = evaluateSolverStatus @@ Solver.check z3ctx.solver []
  let isSatisfiableWith z3ctx z3exprs   : bool = evaluateSolverStatus @@ Solver.check z3ctx.solver z3exprs
  let isUnsatisfiableWith z3ctx z3exprs : bool = not @@ isSatisfiableWith z3ctx z3exprs

  let addExpr z3ctx z3expr : unit = Solver.add z3ctx.solver [ z3expr ]
  let addExprs z3ctx z3exprs : unit = Solver.add z3ctx.solver z3exprs

  (* sorts *)

  let sort_of_type z3ctx : type_ -> Sort.sort =
    function
    | Int     -> z3ctx.int_sort
    | Bool    -> z3ctx.bool_sort
    | Class _ -> z3ctx.object_sort
    | Top     -> failwith "TODO: sort for Top"

  (* booleans *)

  let makeBoolVal   z3ctx b   : z3expr = Boolean.mk_val      z3ctx.context b
  let makeBoolConst z3ctx x   : z3expr = Boolean.mk_const_s  z3ctx.context x
  let makeAnd       z3ctx x y : z3expr = Boolean.mk_and      z3ctx.context[x;y]
  let makeOr        z3ctx x y : z3expr = Boolean.mk_or       z3ctx.context[x;y]
  let makeNeq       z3ctx x y : z3expr = Boolean.mk_distinct z3ctx.context[x;y]
  let makeEq        z3ctx x y : z3expr = Boolean.mk_eq       z3ctx.context x y
  let makeNot       z3ctx x   : z3expr = Boolean.mk_not      z3ctx.context x

  (* integers *)

  let makeIntVal   z3ctx : int -> z3expr = Arithmetic.Integer.mk_numeral_i z3ctx.context
  let makeIntConst z3ctx : id -> z3expr  = Arithmetic.Integer.mk_const_s z3ctx.context
  let makeAdd      z3ctx x y : z3expr    = Arithmetic.mk_add z3ctx.context [ x ; y ]
  let makeSub      z3ctx x y : z3expr    = Arithmetic.mk_sub z3ctx.context [ x ; y ]
  let makeMul      z3ctx x y : z3expr    = Arithmetic.mk_mul z3ctx.context [ x ; y ]
  let makeDiv      z3ctx x y : z3expr    = Arithmetic.mk_div z3ctx.context   x   y
  let makeLt       z3ctx x y : z3expr    = Arithmetic.mk_lt  z3ctx.context   x   y
  let makeGt       z3ctx x y : z3expr    = Arithmetic.mk_gt  z3ctx.context   x   y
  let makeLe       z3ctx x y : z3expr    = Arithmetic.mk_le  z3ctx.context   x   y
  let makeGe       z3ctx x y : z3expr    = Arithmetic.mk_ge  z3ctx.context   x   y

  (* objects *)

  let makeObjectConst z3ctx id : z3expr =
    Expr.mk_const_s z3ctx.context id z3ctx.object_sort

  (* fields *)

  let makeFieldConst z3ctx (base_z3expr:z3expr) (fld:id) (typ:type_) : z3expr =
    let fld_z3expr  = Expr.mk_const_s z3ctx.context fld z3ctx.field_sort in
    let fldref_sort = sort_of_type z3ctx typ in
    let fldref_fndl = z3ctx.make_fieldref_funcdecl fldref_sort in
    FuncDecl.apply fldref_fndl [base_z3expr; fld_z3expr]

  (* predicates *)

  let makePredicateFuncDecl z3ctx (pred:predicate) : FuncDecl.func_decl =
    let arg_sorts = List.map pred.arguments ~f:(fun arg -> sort_of_type z3ctx arg.type_) in
    FuncDecl.mk_func_decl_s z3ctx.context pred.id arg_sorts z3ctx.bool_sort

  let makePredicateCheck z3ctx (pred_fndl:FuncDecl.func_decl) (args:z3expr list) : z3expr =
    FuncDecl.apply pred_fndl args

  (* accesses *)

  let makeAccessCheck z3ctx (fldref_z3expr:z3expr) : z3expr =
    FuncDecl.apply z3ctx.acc_funcdecl [fldref_z3expr]


end
