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

type context = {
  clsctx : ClassContext.t;
  typctx : TypeContext.t;
  scpctx : ScopingContext.t;
  z3ctx : Z3Context.t;
  z3exprs : Expr.expr list;
}

let addZ3Expr ctx z3expr : context = failwith "TODO"

(* normal form is: (phi * ... * phi) ^ ... ^ (phi * ... * phi)
   which traslates to norm=[ [phi;...;phi] ; ... ; [phi;...;phi] ]
   first dimension is of "clauses"
*)
let normalize (phi:concrete) : concrete list list =
  failwith "TODO"

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

let removeAccesses ctx : context =
  failwith "TODO: go through ctx.z3exprs and remove all access and predicate checks"

let checkSatisfiability ctx : context option =
  if Z3Context.areSatisfiable ctx.z3ctx ctx.z3exprs
  then Some ctx
  else None

let rec isSatisfiableConcrete ctx phi : bool =
  let clauses = normalize phi in
  let f ctx_op phis = ctx_op >>= fun ctx ->
    processClause ctx phis >>|
    removeAccesses in
  let ctx_op = List.fold_left clauses ~init:(Some ctx) ~f in
  match ctx_op with
  | Some ctx -> Z3Context.areSatisfiable ctx.z3ctx ctx.z3exprs
  | None -> false

and processClause ctx phis : context option =
  let f ctx_op phi = ctx_op >>= fun ctx -> processConcrete ctx phi in
  List.fold_left phis ~init:(Some ctx) ~f

and processConcrete ctx phi : context option =
  checkSatisfiability @@
  match phi with

  | Expression expr ->
    addZ3Expr ctx @@ z3expr_of_expression ctx expr

  | Predicate_check predchk ->
    let pred = TypeContext.inferClassPredicate ctx.clsctx ctx.typctx predchk in
    let pred_fndl = Z3Context.makePredicateFuncDecl ctx.z3ctx pred in
    let arg_z3exprs = List.map predchk.arguments ~f:(z3expr_of_expression ctx) in
    let predchk_z3expr = Z3Context.makePredicateCheck ctx.z3ctx pred_fndl arg_z3exprs in
    (* if is satisfiable with new z3expr, then good so far, so add negation of z3expr *)
    addZ3Expr ctx predchk_z3expr

  | Access_check accchk ->
    (* if is satisfiable with new z3expr, then good so far, so add negation of z3expr *)
    failwith "TODO"

  | Operation _ -> failwith "TODO"

  | If_then_else ite -> failwith "TODO"

  | Unfolding_in unfolin -> failwith "TODO"



(* let rec processConcrete ctx phi : bool =
   match phi with

   | Expression expr ->
    Z3Context.addExpr ctx.z3ctx @@ z3expr_of_expression ctx expr;
    true

   | Predicate_check predchk ->
    let pred = TypeContext.inferClassPredicate ctx.clsctx ctx.typctx predchk in
    let pred_fndl = Z3Context.makePredicateFuncDecl ctx.z3ctx pred in
    let arg_z3exprs = List.map predchk.arguments ~f:(z3expr_of_expression ctx) in
    let predchk_z3expr = Z3Context.makePredicateCheck ctx.z3ctx pred_fndl arg_z3exprs in
    (* first, check if addition is consistent *)
    if Z3Context.
         Z3Context.addExpr ctx.z3ctx @@ ;
      true

   | Access_check accchk -> some @@
    let fldref_z3expr = z3expr_of_expression ctx @@ Field_reference { base=accchk.base; field=accchk.field } in
    let acchk_z3expr = Z3Context.makeAccess ctx.z3ctx fldref_z3expr in
    (* first, check whether access is consistent with solver *)

   | Operation _ -> failwith "TODO"

   | If_then_else ite -> failwith "TODO"

   | Unfolding_in unfolin -> failwith "TODO" *)


(** Checks whether the given formula is satifiable.
    The ClassContext is of the enclosing program.
    The TypeContext is of the enclosing statement.
    If the formula appears in a method's contract, the type context includes the method's arguments.
    The ScopingContext is of the formula. *)
let isSatisfiable clsctx typctx scpctx frm : bool =
  let z3ctx = Z3Context.create () in
  let ctx = {
    clsctx = clsctx;
    typctx = typctx;
    scpctx = scpctx;
    z3ctx = z3ctx;
  } in
  match frm with
  | Imprecise phi -> failwith "TODO"
  | Concrete  phi -> isSatisfiableConcrete ctx phi
