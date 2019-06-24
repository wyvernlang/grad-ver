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
    | Accessed of expression_field_reference
    | Assumed  of predicate_check
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

  (* in [ctx], [ps] |- [p] *)
  let rec entails ctx ps : elt -> bool =
    function
    (* accessed( e.f ) *)
    | Accessed predchk ->
      let f =
        function
        | Assumed _ -> false
        | Accessed predchk' ->
          AliasingContext.entails ctx @@ AliasProp.ofList @@
          (* casting from ObjectValueSet.Elt.t *)
          List.map ~f:ObjectValue.ofObjectValueSetElt [ ObjectValueSetElt.Field_reference predchk;
                                                        ObjectValueSetElt.Field_reference predchk' ]
      in
      PermissionSet.exists ps ~f
    (* assumed( a(es) ) *)
    | Assumed ass ->
      let f =
        function
        | Accessed _   -> false
        | Assumed ass' ->
          ass.predicate = ass'.predicate &&
          ass.class_    = ass'.class_ &&
          (* for each argument, either aliases or syntaxeq *)
          let f e e' =
            match ObjectValue.ofExpression e, ObjectValue.ofExpression e' with
            | Some o, Some o' -> AliasingContext.entails ctx @@ AliasProp.ofList [o;o']
            | None,   None    -> syneqExpression e e'
            | _               -> failwith "TODO: non-syntactical equality not supported."
          in
          match List.for_all2 ass.arguments ass'.arguments ~f with
          | Ok b -> b
          | Unequal_lengths -> false
      in
      PermissionSet.exists ps ~f
  end

(*--------------------------------------------------------------------------------------------------------------------------*)
(* framing *)
(*--------------------------------------------------------------------------------------------------------------------------*)

let rec frames (ctx:AliasingContext.t) (perms:PermissionSet.t) =
  function
  | Imprecise _ -> failwith "imprecise formulae are always framed"
  | Concrete phi -> framesConcrete ctx perms phi

and framesConcrete (ctx:AliasingContext.t) (perms:PermissionSet.t) (phi:concrete) : bool =
  match phi with
  | Expression expr ->
    framesExpression ctx perms expr
  | Predicate_check predchk ->
    List.for_all predchk.arguments ~f:(framesExpression ctx perms)
  | Access_check accchk ->
    framesExpression ctx perms accchk.base
  | Operation oper ->
    framesConcrete ctx (PermissionSet.union perms (Permissions.grantedConcrete oper.left)) oper.right &&
    framesConcrete ctx (PermissionSet.union perms (Permissions.grantedConcrete oper.right)) oper.left
  | If_then_else ite ->
    framesExpression ctx perms ite.condition &&
    (let then_, scp = ite.then_ in framesConcrete (AliasingContext.ofScope ctx scp) perms then_) &&
    (let else_, scp = ite.else_ in framesConcrete (AliasingContext.ofScope ctx scp) perms else_)
  | Unfolding_in unfolin ->
    (framesConcrete ctx perms @@ Predicate_check unfolin.predicate_check) &&
    (let predchk = unfolin.predicate_check in Permissions.entails ctx perms @@ Assumed predchk) &&
    (let phi', scp = unfolin.formula in framesConcrete (AliasingContext.ofScope ctx scp) perms phi')

and framesExpression ctx perms expr : bool =
  match expr with
  | Variable var ->
    true
  | Value vlu ->
    true
  | Operation oper ->
    framesExpression ctx perms oper.left &&
    framesExpression ctx perms oper.right
  | Comparison comp ->
    framesExpression ctx perms comp.left &&
    framesExpression ctx perms comp.right
  | Field_reference fldref ->
    framesExpression ctx perms fldref.base &&
    Permissions.entails ctx perms @@ Accessed fldref

(*--------------------------------------------------------------------------------------------------------------------------*)
(* self framing *)
(*--------------------------------------------------------------------------------------------------------------------------*)

let rec selfFrames phi_root : bool =
  match phi_root with
  | Imprecise phi_root -> failwith "unimplemented: self-framing for imprecise formulas"
  | Concrete  phi_root ->
    let ctx_root = AliasingContext.construct (Imprecise phi_root) in
    framesConcrete ctx_root PermissionSet.empty phi_root
