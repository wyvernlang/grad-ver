(** {1 Implicit Dynamic Frames} *)

open Core
open Sexplib.Std

open Ast
open Utility
open Wellformed

(** {2 Permissions} *)

module PermissionSetElt :
sig
  (** A {b permission} is either to access a field to assume a predicate. Note that the permission to assume a predicate is
      derived permission - it expands into the permissions granted by the singly-unrolled body of the predicate. *)
  type t =
      Accessed of Ast.expression_field_reference
    | Assumed of Ast.predicate_check
  [@@deriving sexp]
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
  val entails : Aliasing.AliasingContext.t -> PermissionSet.t -> elt -> bool
end

(** {2 Framing} *)

(** [frames perms phi = true] if and only if [perms] frames [phi] i.e. each permission required to frame [phi] is an element
    of either [perms] or [granted phi] *)
val frames : Aliasing.AliasingContext.t -> PermissionSet.t -> Ast.formula -> bool

(** [selfFrames phi = true] if and only if [phi] grants all the permissions it requires i.e. the empty set frames [phi]. *)
val selfFrames : Ast.formula -> bool
