(** {1 Wellformedness and Type-Checking} *)

open Ast

exception Malformed of string
exception Invalid_field_reference of expression_field_reference
exception Invalid_access_check of concrete_access_check
exception Invalid_field_assignment of statement_field_assignment
exception Invalid_new_object of statement_new_object
exception Class_undefined of Ast.id
exception Field_undeclared of class_ * Ast.id
exception Predicate_undefined of class_ option * Ast.id
exception Method_undefined of class_ * Ast.id
exception Variable_undeclared of Ast.id
exception Type_mismatch of { left : type_; right : type_; }
exception Class_mismatch of { left : class_; right : class_; }
exception Method_call_arguments_length_mismatch of method_ * statement_method_call
exception Fold_arguments_length_mismatch of predicate * statement_fold
exception Unfold_arguments_length_mismatch of predicate * statement_unfold
exception Unfolding_in_arguments_length_mismatch of predicate * concrete_unfolding_in
val check : bool -> exn -> unit
val checkSome : 'a option -> exn -> unit
val getSome : 'a option -> exn -> 'a
val checkFold : ('a -> unit) -> 'a Core.List.t -> unit
val string_of_type_ : type_ -> string
val checkTypeMatch : type_ -> type_ -> unit
val checkClassMatch : class_ -> class_ -> unit
type 'a context = (Ast.id, 'a) Core.String.Table.t_
val findExn : 'a context -> Ast.id -> exn -> 'a
val createContext : unit -> 'a context
val class_context : class_ context
val setClass : Ast.id -> class_ -> unit
val getClass : Ast.id -> class_
val getField : Ast.id -> Ast.id -> class_field
val getFieldType : Ast.id -> Ast.id -> type_
val getPredicate : Ast.id -> Ast.id -> predicate
val getPredicateArguments : Ast.id -> Ast.id -> argument list
val getPredicateFormula : Ast.id -> Ast.id -> formula
val getMethod : Ast.id -> Ast.id -> method_
val variable_context : type_ context
val setVariableType : Ast.id -> type_ -> unit
val getVariableType : Ast.id -> type_
val synthesizeType : expression -> type_
val inferPredicateClass : predicate_check -> class_
val checkExpression : expression -> unit
val checkConcrete : concrete -> unit
val checkFormula : formula -> unit
val checkContract : contract -> unit
val checkStatement : statement -> unit
val checkPredicate : predicate -> unit
val checkMethod : method_ -> unit
val checkClass : class_ -> unit
val checkProgram : program -> unit
