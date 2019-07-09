open Ast

exception Unfound_id of Ast.id
exception Unfound_predicate of Ast.id * Ast.id
exception Type_mismatch_comparison of Ast.type_ * Ast.type_
exception Type_mismatch_operation of Ast.type_ * Ast.type_
exception Type_mismatch_argument of Ast.type_ * Ast.type_
exception Type_mismatch_assignment of id * type_ * type_
exception Type_mismatch_field_assignment of Ast.type_ * Ast.type_
exception Type_mismatch_new_object of Ast.type_ * Ast.type_
exception Type_mismatch_method_call of Ast.type_ * Ast.type_
exception Unfound_field of Ast.expression_field_reference
exception Nonobject_base of Ast.expression_field_reference
exception Nonobject_field_assignment_base of Ast.statement_field_assignment
exception Undefined_class of Ast.id
exception Undefined_field of Ast.id * Ast.id
exception Undefined_predicate of Ast.id * Ast.id
exception Undefined_method of Ast.id * Ast.id
exception Nonbool_if_condition of Ast.expression
exception Nonbool_while_condition of Ast.expression
exception Argument_length_mismatch_method_call of Ast.statement_method_call
val assertEq : exn -> 'a -> 'a -> unit
val assertSome : exn -> 'a option -> unit
val assertEqType : exn -> Ast.type_ -> Ast.type_ -> unit
module ClassContext :
sig
  type t = (Ast.id, Ast.class_) Core.Hashtbl.t
  val create : unit -> t
  val copy : t -> t
  val to_string : t -> string
  val sexp_of_t : t -> Sexplib.Sexp.t
  val equal : t -> t -> bool
  val global : t
  val addClass : (string, Ast.class_) Core.Hashtbl.t -> Ast.class_ -> unit
  val getClass :
    (Ast.id, Ast.class_) Core.Hashtbl.t ->
    Ast.id Core.Hashtbl.key -> Ast.class_
  val getClassFieldType :
    (Ast.id, Ast.class_) Core.Hashtbl.t ->
    Ast.id Core.Hashtbl.key -> Ast.id -> Ast.type_
  val getClassPredicate :
    (Ast.id, Ast.class_) Core.Hashtbl.t ->
    Ast.id Core.Hashtbl.key -> Ast.id -> Ast.predicate
  val getClassMethod :
    (Ast.id, Ast.class_) Core.Hashtbl.t ->
    Ast.id Core.Hashtbl.key -> Ast.id -> Ast.method_
  val constructClass :
    (string, Ast.class_) Core.Hashtbl.t -> Ast.class_ -> unit
  val construct : Ast.program -> t
end
module TypeContext :
sig
  type t = (Ast.id, Ast.type_) Core.Hashtbl.t
  val create : unit -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val to_string : t -> string
  val equal : t -> t -> bool
  val copy : t -> t
  val setIdType :
    ('a, 'b) Core.Hashtbl.t -> 'a Core.Hashtbl.key -> 'b -> unit
  val getIdType :
    (Ast.id, Ast.type_) Core.Hashtbl.t ->
    Ast.id Core.Hashtbl.key -> Ast.type_
  val getExpressionType :
    (Ast.id, Ast.class_) Core.Hashtbl.t ->
    (Ast.id, Ast.type_) Core.Hashtbl.t -> Ast.expression -> Ast.type_
  val inferClassPredicate : ClassContext.t -> t -> predicate_check -> predicate
  val constructArguments :
    (string, Ast.type_) Core.Hashtbl.t -> Ast.argument list -> unit
  val constructStatement :
    ClassContext.t -> (string, Ast.type_) Core.Hashtbl.t -> Ast.statement -> unit
end
module TypeCheck :
sig
  type class_context = (Ast.id, Ast.class_) Core.Hashtbl.t
  type type_context = (Ast.id, Ast.type_) Core.Hashtbl.t
  val checkExpression :
    (Ast.id, Ast.class_) Core.Hashtbl.t ->
    (Ast.id, Ast.type_) Core.Hashtbl.t -> Ast.expression -> unit
  val checkPredicateCheck :
    (Ast.id, Ast.class_) Core.Hashtbl.t ->
    (Ast.id, Ast.type_) Core.Hashtbl.t -> Ast.predicate_check -> unit
  val checkFormula :
    (Ast.id, Ast.class_) Core.Hashtbl.t ->
    (Ast.id, Ast.type_) Core.Hashtbl.t -> Ast.formula -> unit
  val checkStatement :
    (Ast.id, Ast.class_) Core.Hashtbl.t ->
    (string, Ast.type_) Core.Hashtbl.t -> Ast.statement -> unit
  val checkPredicate :
    (Ast.id, Ast.class_) Core.Hashtbl.t -> 'a -> Ast.predicate -> unit
  val checkContract :
    (Ast.id, Ast.class_) Core.Hashtbl.t ->
    (Ast.id, Ast.type_) Core.Hashtbl.t -> Ast.contract -> unit
  val checkMethod :
    (Ast.id, Ast.class_) Core.Hashtbl.t -> string -> Ast.method_ -> unit
  val checkClass :
    (Ast.id, Ast.class_) Core.Hashtbl.t -> Ast.class_ -> unit
  val check : TypeContext.t -> Ast.program -> unit
end
