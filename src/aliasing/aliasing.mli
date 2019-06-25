(** {1 Aliasing Context Construction and Utilities} *)

open Core
open Sexplib.Std

open Ast
open Utility
open Wellformed

(** {1 Aliasing Context Structures} *)

(** {2 Object Value} *)

module ObjectValueSetElt :
sig
  (** An object value is one of the folloing:
      - a variable or value [v:C],
      - a field reference [e.f:C],
      - [null:C] where [C] is a class *)
  type t =
      Value of value
    | Variable of variable
    | Field_reference of expression_field_reference
  [@@deriving sexp]
end

module ObjectValueSet : Set.S

module ObjectValue :
sig
  type t = ObjectValueSet.Elt.t
  [@@deriving sexp]

  val ofObjectValueSetElt : ObjectValueSetElt.t -> t
  val ofExpression : expression -> t option
  val toExpression : t -> expression
end

(** {2 Alias Proposition} *)

module AliasPropSet : Set.S

module AliasProp :
sig
  type t = ObjectValueSet.t
  [@@deriving sexp]

  val ofList : ObjectValue.t list -> t

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

val toAliasPropSetElt : AliasProp.t -> AliasPropSet.Elt.t

(** {2 Aliasing Context} *)

module AliasingContext :
sig
  (** An aliasing-context [A] is a set of aliasing propositions and a set of labeled child contexts. This forms a tree
      structure. *)
  type t =
    { parent : t option;
      scope : scope;
      props : AliasPropSet.t;
      children : child list; }
  [@@deriving sexp]

(** Each branch off of a parent aliasing-context is labeled by either:
      - the condition expressions [e] and [negateExpression e] for the two branches of a [if e then f1 else f2] formula
      - the predicate check [a(es)] for the single branch of a [unfolding a(es) in f] formula *)
  and label =
      Condition of expression
    | Unfolding of predicate_check
  [@@deriving sexp]

  and child = label * t
  [@@deriving sexp]

  val parentScopeOf : t -> scope

  (** Collects the set of object values that appear at the top level of the given context (not including children). *)
  val objectvaluesOf : t -> ObjectValueSet.t

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
