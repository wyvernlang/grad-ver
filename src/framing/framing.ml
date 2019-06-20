open Core
open Sexplib.Std

open Ast
open Utility
open Wellformed
open Aliasing

(*--------------------------------------------------------------------------------------------------------------------------*)
(* permissions *)
(*--------------------------------------------------------------------------------------------------------------------------*)

type permission =
  | Accessed of { base: expression; field: id }
  | Assumed  of { predicate: id; arguments: expression list; class_: id option }
[@@deriving sexp]

module PermissionSet = Set.Make(
  struct
    type t = permission
    let compare = compare
    let sexp_of_t = sexp_of_permission
    let t_of_sexp = permission_of_sexp
  end)

(*--------------------------------------------------------------------------------------------------------------------------*)
(* granted permissions *)
(*--------------------------------------------------------------------------------------------------------------------------*)

let rec granted : formula -> PermissionSet.t =
  function
  | Imprecise _ -> failwith "unimplemented: granted permissions for imprecise formulae"
  | Concrete phi -> grantedConcrete phi

and grantedConcrete (phi, scp) : PermissionSet.t =
  match phi with
  | Expression _ ->
    PermissionSet.empty
  | Predicate_check predchk ->
    PermissionSet.singleton @@ Assumed { predicate=predchk.predicate; arguments=predchk.arguments; class_=predchk.class_ }
  | Access_check accchk ->
    PermissionSet.singleton @@ Accessed { base=accchk.base; field=accchk.field }
  | Operation oper ->
    PermissionSet.union (grantedConcrete oper.left) (grantedConcrete oper.right)
  | If_then_else ite ->
    PermissionSet.inter (grantedConcrete ite.then_) (grantedConcrete ite.else_)
  | Unfolding_in unfolin ->
    grantedConcrete unfolin.formula

(*--------------------------------------------------------------------------------------------------------------------------*)
(* permission entailment *)
(*--------------------------------------------------------------------------------------------------------------------------*)

let rec permissionsEntail prgm perms scp : permission -> bool =
  let ctx = aliasingContextOfScope prgm scp in
  let ovs = objectValuesOfContext ctx in
  function
  | Accessed acd ->
    (* base of field access as an object value *)
    let o : ObjectValueSet.Elt.t =
      match objectvalue_of_expression acd.base with
      | Some o -> objectvaluesetelt_of_objectvalue o
      | None -> failwith "malformed e.f" in
    (* the aliasing context entails that  *)
    let f o' =
      aliasingContextEntailsAliasProp ctx (ObjectValueSet.of_list [o;o']) &&
      PermissionSet.mem perms (Accessed { base=expression_of_objectvalue o'; field=acd.field }) in
    ObjectValueSet.exists ovs ~f
  | Assumed asm ->
    let args = asm.arguments in
    (*  *)
    let f o : bool = () in
    List.for_all args ~f

(*--------------------------------------------------------------------------------------------------------------------------*)
(* framing *)
(*--------------------------------------------------------------------------------------------------------------------------*)

let rec frames perms =
  function
  | Imprecise _ -> failwith "imprecise formulae are always framed"
  | Concrete phi -> framesConcrete perms phi

and framesConcrete perms (phi, scp) : bool =
  match phi with
  | Expression expr ->
    framesExpression perms expr
  | Predicate_check predchk ->
    List.for_all predchk.arguments ~f:(framesExpression perms)
  | Access_check accchk ->
    framesExpression perms accchk.base
  | Operation oper ->
    framesConcrete (PermissionSet.union perms (grantedConcrete oper.left)) oper.right &&
    framesConcrete (PermissionSet.union perms (grantedConcrete oper.right)) oper.left
  | If_then_else ite ->
    framesExpression perms ite.condition &&
    framesConcrete perms ite.then_ &&
    framesConcrete perms ite.else_
  | Unfolding_in unfolin ->
    let predchk = unfolin.predicate_check in
    framesConcrete perms unfolin.formula &&
    permissionsEntail perms scp @@ Assumed{ predicate=predchk.predicate; class_=predchk.class_; arguments=predchk.arguments }

and framesExpression perms (expr, scp) : bool =
  match expr with
  | Variable var ->
    true
  | Value vlu ->
    true
  | Operation oper ->
    framesExpression perms oper.left &&
    framesExpression perms oper.right
  | Comparison comp ->
    framesExpression perms comp.left &&
    framesExpression perms comp.right
  | Field_reference fldref ->
    framesExpression perms fldref.base &&
    permissionsEntail perms scp @@ Accessed{ base=fldref.base; field=fldref.field }

(*--------------------------------------------------------------------------------------------------------------------------*)
(* self framing *)
(*--------------------------------------------------------------------------------------------------------------------------*)

let rec selfFrames : formula -> bool =
  function
  | Imprecise _ -> failwith "unimplemented: self-framing for imprecise formulas"
  | Concrete phi -> selfFramesConcrete phi

and selfFramesConcrete : concrete -> bool = framesConcrete PermissionSet.empty
