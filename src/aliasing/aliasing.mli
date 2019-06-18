open Core
open Sexplib.Std
open Ast

module Sexp = Sexplib.Sexp

(** An object value is either a variable or value [v : C], a field reference [e.f : C], or [null : C] where [C] is a class *)
type objectvalue =
  | OV_Value           of value
  | OV_Variable        of variable
  | OV_Field_reference of expression_field_reference
  | OV_Null
[@@deriving sexp]

module ObjectValueSet : Set.S

(** An aliasing proposition [ovs : aliased] is an assertions that a set [ovs] of object variables is such that each [o] in
    [ovs] aliases with every [o'] in [ovs]. *)

type aliasprop = ObjectValueSet.t
[@@deriving sexp]

module AliasPropSet : Set.S
type aliasprop_set = AliasPropSet.t

(** An aliasing context [A] is a set of aliasing propositions and a set of labeled child contexts. This forms a tree structure. *)
type aliasing_context = {
  parent   : aliasing_context option;
  scope_id : scope_id;
  props    : aliasprop_set;
  children : (aliasing_context_label * aliasing_context) list;
}

(** Each branch off of a parent aliasing context is labeled by either the condition or the "unfolding ... in ..." formula
    needed to go down the branch. *)
and aliasing_context_label =
  | ACL_Condition of expression
  | ACL_Unfolding of predicate_check

(** Judge whether a set [ps] of aliased propositions entail a given aliased proposition [p]. This is calculated by finding
    the existence or non-existence of a member [p'] of [ps] such that [p] is a subset of [p']. *)
val propsEntailsAliased : aliasprop_set -> aliasprop -> bool

(** Combine aliasing contexts. In each, inherit the parent and scope_id of the first argument. *)
val contextUnion : aliasing_context -> aliasing_context -> aliasing_context
val contextIntersection : aliasing_context -> aliasing_context -> aliasing_context
(** Useful notations for [contextUnion] and [contextIntersection] respectively  *)
val (+++) : aliasing_context -> aliasing_context -> aliasing_context
val (&&&) : aliasing_context -> aliasing_context -> aliasing_context

(** Constructs the aliasing context of a given formula *)
val constructAliasingContext : formula -> aliasing_context

(** Combines a sub-contexts aliasing proposition set with all ancestors *)
val totalAliasProps : aliasing_context -> aliasprop_set

(** Evaluates the judgement that a given aliasing context entails that an aliasing proposition is true. In other words,
    finds an element (object variable set) of the total aliasing proposition set of the context that is a superset of the
    given aliasing proposition. *)
val entailsAliasProp : aliasing_context -> aliasprop -> bool
