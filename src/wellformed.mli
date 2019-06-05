open Core
open Functools
open Ast_types

(* type synthesis *)
val synthesizeType  : expression -> type_

(* wellformedness checks *)
val checkFormula    : formula    -> unit
val checkExpression : expression -> unit
val checkStatement  : statement  -> unit
val checkProgram    : program    -> unit
