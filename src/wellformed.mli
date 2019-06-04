open Core
open Functools
open Ast_types

val getExpressionType : expression -> type_
val checkFormula      : formula    -> unit
val checkExpression   : expression -> unit
val checkStatement    : statement  -> unit
val checkProgram      : program    -> unit
