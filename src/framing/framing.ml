open Core
open Sexplib.Std

open Ast
open Utility
open Wellformed

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

and grantedConcrete (phi, sid) : PermissionSet.t =
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
(* framing *)
(*--------------------------------------------------------------------------------------------------------------------------*)

let rec frames perms =
  function
  | Imprecise _ -> failwith "unimplemented: framing for imprecise formulae"
  | Concrete phi -> framesConcrete perms phi

and framesConcrete perms phi : bool =
  failwith "TODO"

and framesExpression perms expr : bool =
  failwith "TODO"

(*--------------------------------------------------------------------------------------------------------------------------*)
(* self framing *)
(*--------------------------------------------------------------------------------------------------------------------------*)

let rec selfFrames : formula -> bool =
  function
  | Imprecise _ -> failwith "unimplemented: self-framing for imprecise formulas"
  | Concrete phi -> selfFramesConcrete phi

and selfFramesConcrete : concrete -> bool = framesConcrete PermissionSet.empty
