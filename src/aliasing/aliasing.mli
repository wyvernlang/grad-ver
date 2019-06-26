(** {1 Aliasing Context Construction and Utilities} *)

open Core
open Sexplib.Std

open Ast
open Utility
open Wellformed

(** {1 Aliasing Context Structures} *)

(** {2 Object Value} *)

(** TODO: explain the *SetElt modules necessity and all that; they are necessary *)

(** An object value is one of the folloing:
    - a variable or value [v:C],
    - a field reference [e.f:C],
    - [null:C] where [C] is a class *)
type objectvalue =
    Value of value
  | Variable of variable
  | Field_reference of expression_field_reference
[@@deriving sexp]

module ObjectValueSet : Set.S

module ObjectValue :
sig
  type t = ObjectValueSet.Elt.t
  val to_string : t -> string
  val of_objectvalue : objectvalue -> t
  val ofExpression : expression -> t option
  val toExpression : t -> expression
end

(** {2 Alias Proposition} *)

type aliasprop = ObjectValueSet.t
[@@deriving sexp]

module AliasPropSet : Set.S

module AliasProp :
sig
  (* type t = ObjectValueSet.t *)
  type t = AliasPropSet.Elt.t

  val of_aliasprop : aliasprop -> t

  val to_string : t -> string

  val of_list : ObjectValueSet.Elt.t list -> t

  (** Judge whether a set [ps] of aliased propositions entail a given aliased proposition [p]. This is calculated by finding
      the existence or non-existence of a member [p'] of [ps] such that [p] is a subset of [p']. *)
  val entails : AliasPropSet.t -> ObjectValueSet.t -> bool
end

module AliasPropSetElt :
sig
  (** An alias proposition [p] is an assertions that a set [ovs] of object variables is such that each [o] in
      [ovs] aliases with every [o'] in [ovs]. *)
  type t = ObjectValueSet.t
  [@@deriving sexp]
end

(* {3 Utilities} *)

(* a common conversion *)
val aliaspropset_of_objectvalue_list_list : objectvalue list list -> AliasPropSet.t

(** {2 Aliasing Context} *)

(** An aliasing-context [A] is a set of aliasing propositions and a set of labeled child contexts. This forms a tree
    structure. *)
type aliasingcontext =
  { parent   : aliasingcontext option;
    scope    : scope;
    props    : AliasPropSet.t;
    children : aliasingcontext_child list; }
[@@deriving sexp]

(** Each branch off of a parent aliasing-context is labeled by either:
    - the condition expressions [e] and [negateExpression e] for the two branches of a [if e then f1 else f2] formula
    - the predicate check [a(es)] for the single branch of a [unfolding a(es) in f] formula *)
and aliasingcontext_child_label =
  | Condition of expression
  | Unfolding of predicate_check
[@@deriving sexp]

and aliasingcontext_child = (aliasingcontext_child_label * aliasingcontext)
[@@deriving sexp]

module AliasingContext :
sig
  type t = aliasingcontext
  type child = aliasingcontext_child
  type label = aliasingcontext_child_label

  val to_string : t -> string

  (* Get scope of parent context; if not parent, then [root_scope] *)
  val parentScopeOf : t -> scope

  (** Collects the set of object values that appear at the top level of the given context (not including children). *)
  val objectvaluesOf : t -> ObjectValueSet.t

  (** Equality; requires special Set.equal for AliasPropSet *)
  val equal : t -> t -> bool

  (** Combine aliasing-contexts. In each, inherit the parent and scope of the first argument. *)
  val union : t -> t -> t
  val inter : t -> t -> t

  (** Combines a sub-context's aliasing proposition set with all ancestors *)
  val getTotal : t -> t
  val totalAliasProps : t -> AliasPropSet.t

  (** Evaluates the judgement that a given aliasing-context entails that an aliasing proposition is true. In other words,
      finds an element (object variable set) of the total aliasing proposition set of the context that is a superset of the
      given aliasing proposition. *)
  val entails : t -> ObjectValueSet.t -> bool

  (** Constructs the aliasing-context of a given formula *)
  val construct : formula -> t

  (** Get the sub-aliasing-context nested in a given root context that has the scope. *)
  val ofScope : t -> scope -> t
end
