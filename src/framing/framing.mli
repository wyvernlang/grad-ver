(** {1 Implicit Dynamic Frames} *)

open Core
open Sexplib.Std

open Ast
open Utility
open Aliasing
open Wellformed

(** {2 Permissions} *)

(** A {b permission} is either to access a field to assume a predicate. Note that the permission to assume a predicate is
    derived permission - it expands into the permissions granted by the singly-unrolled body of the predicate. *)
type permission =
  | Accessed of expression_field_reference
  | Assumed  of predicate_check
[@@deriving sexp]

module PermissionSetElt :
sig
  type t = permission
  val compare : t -> t -> int
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end

module PermissionSet : Set.S

module Permissions :
sig
  type set = PermissionSet.t
  type elt = PermissionSet.Elt.t

  (** {2 Granted Permissions} *)

  (** [granted phi] is the set of permissions granted by [phi]. *)
  val granted : Ast.formula -> set

  (** A set of permissions [perms] within an aliasing context [ctx] may entail a permission [p] without [p] explicitly being
      a member of [perms] because of aliasing considerations. *)
  val entails : ClassContext.t -> TypeContext.t -> ScopingContext.t -> AliasingContext.t -> PermissionSet.t -> elt -> bool
end

(** {2 Framing} *)

(** [frames perms phi = true] if and only if [perms] frames [phi] i.e. each permission required to frame [phi] is an element
    of either [perms] or [granted phi] *)
val frames : ClassContext.t -> TypeContext.t -> ScopingContext.t -> AliasingContext.t -> PermissionSet.t ->
  Ast.formula -> bool

(** [selfFrames phi = true] if and only if [phi] grants all the permissions it requires i.e. the empty set frames [phi]. *)
val selfFrames : ClassContext.t -> TypeContext.t -> Ast.formula -> bool
