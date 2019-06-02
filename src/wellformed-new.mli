open Core
open Functools
open Ast_types

val synthesizeType : expression -> type_

val checkFormula : formula -> unit

val processStatement  : unit -> statement -> unit
val processStatements : statement list ->  unit

val initProgram : program -> unit
