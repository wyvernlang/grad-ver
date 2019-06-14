open Ast_types

(** An object value is either a variable or value [v : C], a field reference [e.f : C], or [null : C] where [C] is a class *)
type object_variable =
  | Value            of value
  | Variable         of variable
  | Field_referencce of expression_field_reference
  | Null

(** An aliasing proposition [ovs : aliased] is an assertions that a set [ovs] of object variables is such that each [o] in [ovs] aliases with each other [o'] in [ovs]. *)
type aliased = object_variable list

(** An aliasing context [A] is a set of aliasing propositions and a set of labeled child contexts. This forms a tree structure. *)
type aliasing_context = Context of aliased list * (aliasing_context_label * aliasing_context) list

(** Each branch off of a parent aliasing context is labeled by either the condition or the "unfolding ... in ..." formula needed to go down the branch. *)
and aliasing_context_label =
  | Condition of expression
  | Unfolding of predicate_check

(** Constructs the aliasing context of a given formula *)
val constructAliasingContext : formula -> aliasing_context

(** Evaluates the judgement that a given aliasing context entails that an aliasing proposition is true *)
val entailsAliased : aliasing_context -> aliased -> bool