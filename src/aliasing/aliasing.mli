open Ast

(** An object value is either a variable or value [v : C], a field reference [e.f : C], or [null : C] where [C] is a class *)
type objectvalue =
  | OV_Value            of value
  | OV_Variable         of variable
  | OV_Field_referencce of expression_field_reference
  | OV_Null

(** An aliasing proposition [ovs : aliased] is an assertions that a set [ovs] of object variables is such that each [o] in
    [ovs] aliases with each other [o'] in [ovs]. *)
type aliased_prop = objectvalue list

(** An aliasing context [A] is a set of aliasing propositions and a set of labeled child contexts. This forms a tree structure. *)
type aliasing_context = {
  scope         : scope_id;
  aliased_props : aliased_prop list;
  children      : (aliasing_context_label * aliasing_context) list;
}

(** Each branch off of a parent aliasing context is labeled by either the condition or the "unfolding ... in ..." formula
    needed to go down the branch. *)
and aliasing_context_label =
  | ACL_Condition of expression
  | ACL_Unfolding of predicate_check

(** Combine aliasing contexts *)
val contextUnion : aliasing_context -> aliasing_context -> aliasing_context
val contextIntersection : aliasing_context -> aliasing_context -> aliasing_context
(** Useful notations for [contextUnion] and [contextIntersection] respectively  *)
val (+++) : aliasing_context -> aliasing_context -> aliasing_context
val (&&&) : aliasing_context -> aliasing_context -> aliasing_context

(** Constructs the aliasing context of a given formula *)
val constructAliasingContext : formula -> aliasing_context

(** Combines a sub-contexts aliasing proposition set with all ancestors *)
val getTotalAliasedProps : aliasing_context -> aliased_prop list

(** Evaluates the judgement that a given aliasing context entails that an aliasing proposition is true. In other words,
    finds an element (object variable set) of the total aliasing proposition set of the context that is a superset of the
    given aliasing proposition. *)
val entailsAliased : aliasing_context -> aliased_prop -> bool
