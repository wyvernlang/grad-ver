open Core
open Sexplib.Std

open Ast
open Utility
open Wellformed

(*--------------------------------------------------------------------------------------------------------------------------*)
(* permissions *)
(*--------------------------------------------------------------------------------------------------------------------------*)

type permission =
  | Access of { base: expression; field: id }
  | Assume of { predicate: id; arguments: expression list; class_: id option }
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

and grantedConcrete phi : PermissionSet.t =
  failwith "TODO"
  (* function
  | Expression _ ->
    PermissionSet.empty
  | Access_check
  | Predicate_check predchk -> *)

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
