open Core
open Sexplib.Std

open Ast
open Utility
open Wellformed
open Aliasing

(*--------------------------------------------------------------------------------------------------------------------------*)
(* permissions *)
(*--------------------------------------------------------------------------------------------------------------------------*)

module PermissionSetElt =
struct
  type t =
    | Accessed of { base: expression; field: id }
    | Assumed  of { predicate: id; arguments: expression list; class_: id option }
  [@@deriving sexp]

  let compare = compare
  let sexp_of_t = sexp_of_t
  let t_of_sexp = t_of_sexp
end

module PermissionSet = Set.Make(PermissionSetElt)

module Permissions =
struct
  open PermissionSetElt
  type set = PermissionSet.t
  type elt = PermissionSet.Elt.t

  (*------------------------------------------------------------------------------------------------------------------------*)
  (* granted permissions *)
  (*------------------------------------------------------------------------------------------------------------------------*)

  let rec granted : formula -> set =
    function
    | Imprecise _ -> failwith "unimplemented: granted permissions for imprecise formulae"
    | Concrete phi -> grantedConcrete phi

  and grantedConcrete : concrete -> set =
    function
    | Expression _ ->
      PermissionSet.empty
    | Predicate_check predchk ->
      PermissionSet.singleton @@ Assumed { predicate=predchk.predicate; arguments=predchk.arguments; class_=predchk.class_ }
    | Access_check accchk ->
      PermissionSet.singleton @@ Accessed { base=accchk.base; field=accchk.field }
    | Operation oper ->
      PermissionSet.union (grantedConcrete oper.left) (grantedConcrete oper.right)
    | If_then_else ite ->
      PermissionSet.inter (grantedConcrete @@ termOf ite.then_) (grantedConcrete @@ termOf ite.else_)
    | Unfolding_in unfolin ->
      grantedConcrete @@ termOf unfolin.formula

  (*------------------------------------------------------------------------------------------------------------------------*)
  (* permission entailment *)
  (*------------------------------------------------------------------------------------------------------------------------*)

  (* TODO: impl *)
  (* ps |- p *)
  let rec entails root_ctx scp ps : elt -> bool =
    let ctx = AliasingContext.ofScope root_ctx scp in
    let os = AliasingContext.objectvaluesOf ctx in
    function
    (* p = accessed( o.f ) *)
    | Accessed acd ->
      (* base of field access as an object value *)
      let o : ObjectValueSet.Elt.t =
        match ObjectValue.ofExpression acd.base with
        | Some o -> o
        | None -> failwith "malformed e.f" in
      (* the aliasing context entails that  *)
      let f o' =
        AliasingContext.entails ctx (AliasProp.of_list [o;o']) &&
        PermissionSet.mem ps (Accessed { base=ObjectValue.toExpression o'; field=acd.field }) in
      ObjectValueSet.exists os ~f
    (* p = assumed( a(es) ) *)
    | Assumed ass ->
      let f =
        function
        | Accessed _  -> false
        | Assumed ass' ->
          ass'.predicate = ass.predicate &&
          ass'.class_    = ass.class_ &&
          let matchArg e' e = true in
          (match List.for_all2 ass'.arguments ass.arguments ~f:matchArg with Ok true -> true | _ -> false)
      in
      PermissionSet.exists ps ~f
  end

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
