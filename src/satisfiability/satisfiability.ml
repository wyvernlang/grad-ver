open Core
open Core.Option
open Z3
open Z3tools
open Utility
open Functools

open Ast
open Wellformed
open Aliasing
open Framing

(*--------------------------------------------------------------------------------------------------------------------------*)
(* satisfiability context *)
(*--------------------------------------------------------------------------------------------------------------------------*)

type context = {
  clsctx : ClassContext.t;
  typctx : TypeContext.t;
  scpctx : ScopingContext.t;
  z3ctx : Z3Context.t;
  z3exprs : Expr.expr list;
  acc_z3exprs : Expr.expr list; (* these are kept separate so they may be removed between '^'s *)
}

(* mutators *)

let getZ3Exprs ctx : Expr.expr list =
  ctx.z3exprs @ ctx.acc_z3exprs

let addZ3Expr ctx z3expr : context =
  { ctx with z3exprs=z3expr::ctx.z3exprs }

let addAccZ3Expr ctx acc_z3expr : context =
  { ctx with acc_z3exprs=acc_z3expr::ctx.acc_z3exprs }

let removeAccZ3Exprs ctx : context =
  { ctx with acc_z3exprs=[] }

(* satisfiability *)

let isSatisfiableContext ctx : bool =
  Z3Context.isSatisfiableWith ctx.z3ctx @@ getZ3Exprs ctx

let isSatisfiableContextWith ctx z3expr : bool =
  Z3Context.isSatisfiableWith ctx.z3ctx @@ z3expr::getZ3Exprs ctx

let checkSatisfiability ctx : context option =
  if Z3Context.isUnsatisfiableWith ctx.z3ctx @@ getZ3Exprs ctx
  then Some ctx
  else None

let addZ3ExprIfSatisfiable ctx z3expr : context option =
  if isSatisfiableContextWith ctx z3expr
  then some @@ addZ3Expr ctx z3expr
  else None

(*--------------------------------------------------------------------------------------------------------------------------*)
(* z3exprs *)
(*--------------------------------------------------------------------------------------------------------------------------*)

(* of expression *)

let rec z3expr_of_expression (ctx:context) (expr:expression) : Expr.expr =
  match expr with
  | Variable var -> Z3Context.makeBoolConst ctx.z3ctx @@ getExpressionId (Variable var)
  | Value vlu ->
    begin
      match vlu with
      | Int    i  -> Z3Context.makeIntVal      ctx.z3ctx i
      | Bool   b  -> Z3Context.makeBoolVal     ctx.z3ctx b
      | Object id -> Z3Context.makeObjectConst ctx.z3ctx id
    end
  | Operation oper ->
    let left = z3expr_of_expression ctx oper.left in
    let right = z3expr_of_expression ctx oper.right in
    begin
      match oper.operator with
      | Add -> Z3Context.makeAdd ctx.z3ctx left right
      | Sub -> Z3Context.makeSub ctx.z3ctx left right
      | Mul -> Z3Context.makeMul ctx.z3ctx left right
      | Div -> Z3Context.makeDiv ctx.z3ctx left right
      | And -> Z3Context.makeAnd ctx.z3ctx left right
      | Or  -> Z3Context.makeOr  ctx.z3ctx left right
    end
  | Comparison comp ->
    let left = z3expr_of_expression ctx comp.left in
    let right = z3expr_of_expression ctx comp.right in
    begin
      match comp.comparer with
      | Neq -> Z3Context.makeNeq ctx.z3ctx left right
      | Eq  -> Z3Context.makeEq  ctx.z3ctx left right
      | Lt  -> Z3Context.makeLt  ctx.z3ctx left right
      | Gt  -> Z3Context.makeGt  ctx.z3ctx left right
      | Le  -> Z3Context.makeLe  ctx.z3ctx left right
      | Ge  -> Z3Context.makeGe  ctx.z3ctx left right
    end
  | Field_reference fldref ->
    let base_z3expr = z3expr_of_expression ctx fldref.base in
    let fld_typ = TypeContext.getExpressionType ctx.clsctx ctx.typctx (Field_reference fldref) in
    Z3Context.makeFieldConst ctx.z3ctx base_z3expr fldref.field fld_typ

(* process formula *)

let rec processConcrete ctx phi : context option =
  match phi with

  | Expression expr ->
    addZ3ExprIfSatisfiable ctx @@ z3expr_of_expression ctx expr

  | Predicate_check predchk ->
    let pred = TypeContext.inferClassPredicate ctx.clsctx ctx.typctx predchk in
    let pred_fndl = Z3Context.makePredicateFuncDecl ctx.z3ctx pred in
    let arg_z3exprs = List.map predchk.arguments ~f:(z3expr_of_expression ctx) in
    let predchk_z3expr = Z3Context.makePredicateCheck ctx.z3ctx pred_fndl arg_z3exprs in
    addZ3ExprIfSatisfiable ctx predchk_z3expr

  | Access_check accchk ->
    let fldref_z3expr = z3expr_of_expression ctx @@ Field_reference{ base=accchk.base; field=accchk.field } in
    let accchk_z3expr = Z3Context.makeAccessCheck ctx.z3ctx fldref_z3expr in
    let neg_accchk_z3expr = Z3Context.makeNot ctx.z3ctx accchk_z3expr in
    if isSatisfiableContextWith ctx neg_accchk_z3expr (* if ~ acc(x.f) is satisfiable: *)
    then addZ3ExprIfSatisfiable ctx accchk_z3expr     (* then assert acc(x.f); *)
    else None                                         (* otherwise acc(x.f) has already been asserted. *)

  | Operation oper ->
    begin
      match oper.operator with
      | Sep -> processConcrete ctx oper.left                      >>= fun ctx -> processConcrete ctx oper.right
      | And -> processConcrete ctx oper.left >>| removeAccZ3Exprs >>= fun ctx -> processConcrete ctx oper.right
    end

  | If_then_else ite ->
    let cond_z3expr     = z3expr_of_expression ctx ite.condition in
    let neg_cond_z3expr = Z3Context.makeNot ctx.z3ctx cond_z3expr in
    (* at lxeast one branch must be satisfiable *)
    if isSatisfiableConcrete (addZ3Expr ctx cond_z3expr)     @@ termOf ite.then_ ||
       isSatisfiableConcrete (addZ3Expr ctx neg_cond_z3expr) @@ termOf ite.else_
    then Some ctx
    else None
  | Unfolding_in unfolin ->
    processConcrete ctx (termOf unfolin.formula)

(* satisfiable formula *)

and isSatisfiableConcrete ctx phi : bool =
  match processConcrete ctx phi with
  | Some ctx -> isSatisfiableContext ctx
  | None     -> false

(** Checks whether the given formula is satifiable.
    The ClassContext is of the enclosing program.
    The TypeContext is of the enclosing statement.
    If the formula appears in a method's contract, the type context includes the method's arguments.
    The ScopingContext is of the formula. *)
let isSatisfiable clsctx typctx scpctx frm : bool =
  let ctx = { clsctx=clsctx; typctx=typctx; scpctx=scpctx;
              z3ctx=Z3Context.create (); z3exprs=[]; acc_z3exprs=[] } in
  match frm with
  | Imprecise phi -> failwith "TODO: isSatisfiable of Imprecise"
  | Concrete  phi -> isSatisfiableConcrete ctx phi
