open Ast_types
open Ast
open Wellformed
open Utility

(** {0 Implicit Dynamic Frames} *)

(** {1 Permissions} *)

(** A {b permission} is either to access a field to assume a predicate *)
(* TODO: pretty sure that the class_ field is not optional, must be inferred? *)

type permission =
  | Access of { base: expression; field: id }
  | Assume of { predicate: id; arguments: expression list; class_: id option }

module type PERMISSION = sig
  type t = permission
  val compare : t -> t -> int
end
module Permission : PERMISSION = struct
  type t = permission
  let compare = compare
end

module PermissionSet = Set.Make(Permission)
module PS = PermissionSet

(** {1 Granted Permissions} *)

(** [granted phi] is the set of permissions granted by [phi]. *)
let rec granted : concrete -> PS.t =
  fun phi ->
  match phi with
  | Expression _ ->
    PS.empty
  | Predicate_check predchk ->
    PS.empty
  | Access_check accchk ->
    PS.singleton (Access{ base=accchk.base; field=accchk.field })
  | Operation oper ->
    PS.union (granted oper.left) (granted oper.right)
  | If_then_else ite ->
    PS.empty (* statically, you don't know which path will be taken *)
  | Unfolding_in unfolin ->
    let predchk = unfolin.predicate_check in
    PS.singleton (Assume{ predicate=predchk.predicate; arguments=predchk.arguments; class_=predchk.class_ })

(** {1 Framing} *)

(** [framesConcrete perms phi = true] if and only if [perms] frames [phi] i.e. each permission required to frame [phi] is an element of either [perms] or [granted phi] *)
let rec framesConcrete : PS.t -> concrete -> bool =
  fun perms phi ->
  match phi with
  | Expression expr ->
    framesExpression perms expr
  | Predicate_check predchk ->
    List.for_all (framesExpression perms) predchk.arguments
  | Access_check accchk ->
    framesExpression perms accchk.base
  | Operation oper ->
    let child_perms = PS.union (granted oper.left) (granted oper.right) in
    let perms' = PS.union perms child_perms in
    framesConcrete perms' oper.left && framesConcrete perms' oper.right
  | If_then_else ite ->
    framesExpression perms ite.condition &&
    framesConcrete perms ite.then_ && framesConcrete perms ite.else_
  | Unfolding_in unfolin ->
    let predchk = unfolin.predicate_check in
    List.for_all (framesExpression perms) predchk.arguments &&
    PS.mem (Assume { predicate=predchk.predicate; arguments=predchk.arguments; class_=predchk.class_ }) perms

and framesExpression : PS.t -> expression -> bool =
  fun perms expr ->
  match expr with
  | Variable _ -> true
  | Value _ -> true
  | Operation oper -> framesExpression perms oper.left && framesExpression perms oper.right
  | Comparison comp -> framesExpression perms comp.left && framesExpression perms comp.right
  | Field_reference fldref -> PS.mem (Access{ base=fldref.base; field=fldref.field }) perms

(** {1 Self-Framing} *)

(** [selfFramesConcrete phi = true] if and only if [phi] grants all the permissions it requires i.e. the empty set frames [phi]. *)
let rec selfFramesConcrete : concrete -> bool =
  framesConcrete PS.empty
