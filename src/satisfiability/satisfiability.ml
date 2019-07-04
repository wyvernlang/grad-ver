open Core
open Z3
open Z3tools
open Utility
open Functools

open Ast
open Wellformed
open Aliasing
open Framing

let z3expr_of_predicate predid clsid : Expr.expr = failwith "TODO"

let rec z3expr_of_expression clsctx typctx scpctx z3ctx (expr:expression) : Expr.expr =
  match expr with
  | Variable var -> Z3Context.makeBoolConst z3ctx @@ getExpressionId (Variable var)
  | Value vlu ->
    begin
      match vlu with
      | Int    i  -> Z3Context.makeIntVal      z3ctx i
      | Bool   b  -> Z3Context.makeBoolVal     z3ctx b
      | Object id -> Z3Context.makeObjectConst z3ctx id
    end
  | Operation oper ->
    let left = z3expr_of_expression clsctx typctx scpctx z3ctx oper.left in
    let right = z3expr_of_expression clsctx typctx scpctx z3ctx oper.right in
    begin
      match oper.operator with
      | Add -> Z3Context.makeAdd z3ctx left right
      | Sub -> Z3Context.makeSub z3ctx left right
      | Mul -> Z3Context.makeMul z3ctx left right
      | Div -> Z3Context.makeDiv z3ctx left right
      | And -> Z3Context.makeAnd z3ctx left right
    end
  | BOr bor -> failwith "TODO"
  | Comparison comp ->
    let left = z3expr_of_expression clsctx typctx scpctx z3ctx comp.left in
    let right = z3expr_of_expression clsctx typctx scpctx z3ctx comp.right in
    begin
      match comp.comparer with
      | Neq -> Z3Context.makeNeq z3ctx left right
      | Eq  -> Z3Context.makeEq  z3ctx left right
      | Lt  -> Z3Context.makeLt  z3ctx left right
      | Gt  -> Z3Context.makeGt  z3ctx left right
      | Le  -> Z3Context.makeLe  z3ctx left right
      | Ge  -> Z3Context.makeGe  z3ctx left right
    end
  | Field_reference fldref ->
    let sym = symbol_of_field_reference fldref in
    begin
      match TypeContext.getExpressionType clsctx typctx (Field_reference fldref) with
      | Int -> Z3Context.makeIntConst z3ctx sym
      | Bool -> Z3Context.makeBoolConst z3ctx sym
      | Class id -> Z3Context.makeObjectConst z3ctx sym
      | Top -> failwith "UNIMPL: formulas with Top values"
    end

let rec z3expr_of_formula clsctx typctx scpctx z3ctx frm : Expr.expr =
  match frm with
  | Imprecise phi -> failwith "UNIMPL: toZ3Expr for imprecise formulas"
  | Concrete phi ->
    begin
      match phi with
      | Expression expr -> z3expr_of_expression clsctx typctx scpctx z3ctx expr
      | Predicate_check predchk ->
        let cls = begin
          match predchk.cls with
          | Some cls -> cls
          | None -> failwith "UNIMPL: infer predicate class"
        end in
        let pred = ClassContext.getClassPredicate clsctx cls predchk.predicate in
        let pred_func = Z3Context.makePredicateFunc z3ctx pred in
        let arg_exprs = List.map predchk.arguments ~f:(z3expr_of_expression clsctx typctx scpctx z3ctx) in
        Z3Context.makePredicateAppl z3ctx pred_func arg_exprs
      | Access_check accchk -> failwith "TODO"
      | Operation oper -> failwith "TODO"
      | If_then_else ite -> failwith "TODO"
      | Unfolding_in unfolin ->  failwith "TODO"
    end

(** Checks whether the given formula is satifiable.
    The ClassContext is of the enclosing program.
    The TypeContext is of the enclosing statement.
    If the formula appears in a method's contract, the type context includes the method's arguments.
    The ScopingContext is of the formula. *)
let isSatisfiable clsctx typctx scpctx phi : bool =
  let z3ctx = Z3Context.create () in
  Z3Context.addExpr z3ctx @@ z3expr_of_formula clsctx typctx scpctx z3ctx phi;
  Z3Context.isSatisfiable z3ctx
