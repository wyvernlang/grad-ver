open Core
open Z3
open Z3tools
open Utility
open Functools

open Wellformed
open Aliasing
open Framing
open Ast

let z3expr_of_expression scpctx expr : Expr.expr =
  match expr with
  | Variable var -> failwith "TODO"
  | Value vlu -> failwith "TODO"
  | Operation oper -> failwith "TODO"
  | BOr bor -> failwith "TODO"
  | Comparison comp -> failwith "TODO"
  | Field_reference -> failwith "TODO"

let z3expr_of_formula scpctx frm : Expr.expr =
  match frm with
  | Imprecise phi -> failwith "UNIMPL: toZ3Expr for imprecise formulas"
  | Concrete phi ->
    begin
      match phi with
      | Expression expr -> z3expr_of_expression scpctx frm
      | Predicate_check predchk -> failwith "TODO"
      | Access_check accchk -> failwith "TODO"
      | Operation oper -> failwith "TODO"
      | If_then_else ite -> failwith "TODO"
      | Unfolding_in unfolin ->  failwith "TODO"
    end

(** Checks whether the given formula is satifiable.
    The ScopingContext is required to access the aliasing contexts of nested scopes *)
let isSatisfiable scpctx phi : bool =
  let z3ctx = Z3Context.create () in
  Z3Context.addZ3Expr z3ctx @@ z3expr_of_formula scpctx phi;
  Z3Context.isSatisfiable z3ctx
