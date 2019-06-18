(** {1 Implicit Dynamic Frames} *)

open Core
open Sexplib.Std

open Ast
open Utility
open Wellformed

(** {2 Permissions} *)

(** A {b permission} is either to access a field to assume a predicate. Note that the permission to assume a predicate is
    derived permission - it expands into the permissions granted by the singly-unrolled body of the predicate. *)

(* TODO: pretty sure that the class_ field is not optional, must be inferred? *)

type permission =
  | Accessed of { base: expression; field: id }
  | Assumed  of { predicate: id; arguments: expression list; class_: id option }
[@@deriving sexp]

module PermissionSet : Set.S

(** {2 Granted Permissions} *)

(** [granted phi] is the set of permissions granted by [phi]. *)
val granted : formula -> PermissionSet.t

(** {2 Framing} *)

(** [frames perms phi = true] if and only if [perms] frames [phi] i.e. each permission required to frame [phi] is an element of either [perms] or [granted phi] *)
val frames : PermissionSet.t -> formula -> bool

(** {2 Self-Framing} *)

(** [selfFrames phi = true] if and only if [phi] grants all the permissions it requires i.e. the empty set frames [phi]. *)
val selfFrames : formula -> bool
