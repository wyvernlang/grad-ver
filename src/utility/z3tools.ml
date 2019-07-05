open Core
open Z3
open Utility

open Ast
open Wellformed
open Aliasing

let rec symbol_of_field_reference (base:expression) (field:id) : string =
  begin
    match base with
    | Variable        var     -> getExpressionId @@ Variable var
    | Value           vlu     -> getExpressionId @@ Value vlu
    | Field_reference fldref' -> symbol_of_field_reference fldref'.base fldref'.field
    | _                       -> failwith "IMPOSSIBLE: invalid field base"
  end^"."^field

module Z3Context =
struct

  type t = {
    context      : context;
    solver       : Solver.solver;
    bool_sort    : Sort.sort;
    int_sort     : Sort.sort;
    object_sort  : Sort.sort;
    field_sort   : Sort.sort;
    acc_funcdecl : FuncDecl.func_decl
  }

  let create () : t =
    let cfg = [] in
    let ctx = mk_context cfg in
    let bool_sort = Boolean.mk_sort ctx in
    let fld_sort = failwith "TODO: need special sort for fields, because only they can be in `acc`" in
    {
      context      = ctx;
      solver       = Solver.mk_simple_solver ctx;
      bool_sort    = bool_sort;
      int_sort     = Arithmetic.Integer.mk_sort ctx;
      object_sort  = Sort.mk_uninterpreted_s ctx "object";
      field_sort   = fld_sort;
      acc_funcdecl = FuncDecl.mk_func_decl_s ctx "acc" [fld_sort] bool_sort;
    }

  let isSatisfiable z3ctx : bool =
    match Solver.check z3ctx.solver [] with
    | SATISFIABLE   -> true
    | UNSATISFIABLE -> false
    | UNKNOWN       -> false (* TODO: could treat this as true *)

  let addExpr z3ctx expr : unit = Solver.add z3ctx.solver [expr]

  (* sorts *)
  let sort_of_type z3ctx : type_ -> Sort.sort =
    function
    | Int     -> z3ctx.int_sort
    | Bool    -> z3ctx.bool_sort
    | Class _ -> z3ctx.object_sort
    | Top     -> failwith "TODO: sort for Top"


  (* booleans *)
  let makeBoolVal   z3ctx : bool -> Expr.expr = Boolean.mk_val      z3ctx.context
  let makeBoolConst z3ctx : id -> Expr.expr   = Boolean.mk_const_s  z3ctx.context
  let makeAnd       z3ctx x y : Expr.expr     = Boolean.mk_and      z3ctx.context [ x ; y ]
  let makeOr        z3ctx x y : Expr.expr     = Boolean.mk_or       z3ctx.context [ x ; y ]
  let makeNeq       z3ctx x y : Expr.expr     = Boolean.mk_distinct z3ctx.context [ x ; y ]
  let makeEq        z3ctx x y : Expr.expr     = Boolean.mk_eq       z3ctx.context   x   y

  (* integers *)
  let makeIntVal   z3ctx : int -> Expr.expr = Arithmetic.Integer.mk_numeral_i z3ctx.context
  let makeIntConst z3ctx : id -> Expr.expr  = Arithmetic.Integer.mk_const_s z3ctx.context
  let makeAdd      z3ctx x y : Expr.expr    = Arithmetic.mk_add z3ctx.context [ x ; y ]
  let makeSub      z3ctx x y : Expr.expr    = Arithmetic.mk_sub z3ctx.context [ x ; y ]
  let makeMul      z3ctx x y : Expr.expr    = Arithmetic.mk_mul z3ctx.context [ x ; y ]
  let makeDiv      z3ctx x y : Expr.expr    = Arithmetic.mk_div z3ctx.context   x   y
  let makeLt       z3ctx x y : Expr.expr    = Arithmetic.mk_lt  z3ctx.context   x   y
  let makeGt       z3ctx x y : Expr.expr    = Arithmetic.mk_gt  z3ctx.context   x   y
  let makeLe       z3ctx x y : Expr.expr    = Arithmetic.mk_le  z3ctx.context   x   y
  let makeGe       z3ctx x y : Expr.expr    = Arithmetic.mk_ge  z3ctx.context   x   y

  (* objects *)

  let makeObjectConst z3ctx id : Expr.expr =
    Expr.mk_const_s z3ctx.context id z3ctx.object_sort

  (* predicates *)

  let makePredicateFunc z3ctx (pred:predicate) : FuncDecl.func_decl =
    let arg_sorts = List.map pred.arguments ~f:(fun arg -> sort_of_type z3ctx arg.type_) in
    FuncDecl.mk_func_decl_s z3ctx.context pred.id arg_sorts z3ctx.bool_sort

  let makePredicateAppl z3ctx (pred_func:FuncDecl.func_decl) (args:Expr.expr list) : Expr.expr =
    FuncDecl.apply pred_func args

  (* accesses *)

  let makeAccess z3ctx (fldref_expr:Expr.expr) : Expr.expr =
    FuncDecl.apply z3ctx.acc_funcdecl [fldref_expr]
end
