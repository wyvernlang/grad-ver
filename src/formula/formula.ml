(* Contains all the logic for access predicates, etc.
 *
 * TODO: Refactoring -- The organization of this stuff is just generally a mess
 *
 *   Simiilarly, the only reason we don't have the Idf.satisfiable function in
 *   this file is that we need it to rely on both Formula and SAT.
 *
 * Formulas are values of type 'a t, where the 'a can be `precise' or
 * `imprecise'. This done at the type level so we can ensure that we don't
 * call static WLP on a gradual formula (etc). This does lead to complications
 * elsewhere (such as needing to write gradualWLP in continuation passing
 * style), as well as generally more complex code.
*)

open Core
open Functools
open Ast_types
open Ast_pb

module A = Ast

module LM = List.Assoc

exception Unsat
exception Malformed

(* -------------------------------------------------------------------------------------------------------------------------*)
(* substitution *)

let rec eq_expression : expression -> expression -> bool =
  fun e e' -> raise (Failure "unimplemented")

(* let x = e' in e *)
let rec substitute_in_expression : expression -> expression -> expression -> expression =
  fun x e' e ->
  if eq_expression e x then
    e'
  else
    match e with
    | Variable _ | Value _      -> e
    | Binaryoperation  binop    -> Binaryoperation
                                     { binop with
                                       binaryoperationleft  = substitute_in_expression x e' binop.binaryoperationleft;
                                       binaryoperationright = substitute_in_expression x e' binop.binaryoperationright }
    | Binarycomparison binco    -> Binarycomparison
                                     { binco with
                                       binarycomparisonleft  = substitute_in_expression x e' binco.binarycomparisonleft;
                                       binarycomparisonright = substitute_in_expression x e' binco.binarycomparisonright }
    | Fieldreference   fieldref -> Fieldreference
                                     { fieldref with base = substitute_in_expression x e' fieldref.base }

let rec eq_formula f f' = raise (Failure "unimplemented")

(* let x = f' in f *)
(* let rec substitute_in_formula : formula -> formula -> formula -> formula =
  fun x f' f ->
  if eq_formula x f then
    f'
  else
    match f with
    | Concrete conc -> begin
      match conc with
      | Expression      expr    -> f
      | Predicatecheck  predchk -> f
      | Accesscheck     accchk  -> f
      | Logicaland      logand  -> f
      | Logicalseparate logsep  -> f
      | Ifthenelse      ite     -> f
      | Unfoldingin     uin     -> f
    end
  | Imprecise impr -> f *)

(* -------------------------------------------------------------------------------------------------------------------------*)
(* access *)

module Access = struct

end

let rec access : expression -> formula =
  fun e ->
  match e with
  | Variable         _
  | Value            _
  | Binaryoperation  _
  | Binarycomparison _ -> Concrete (Expression (Value Truevalue))
  | Fieldreference   fieldref -> Concrete (Accesscheck
                                             { base    = fieldref.base;
                                               fieldid = fieldref.fieldid })
