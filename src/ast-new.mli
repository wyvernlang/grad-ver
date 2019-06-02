open Ast_types

(* e = e' *)
val eqExpression : expression -> expression -> bool

(* [e'/x]e *)
val substituteInExpression : expression -> expression -> expression -> expression

val simplifyFormula
