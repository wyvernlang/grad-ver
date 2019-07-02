open Core
open Sexplib.Std

open Ast
open Utility
open Functools
open Wellformed
open Aliasing

(*--------------------------------------------------------------------------------------------------------------------------*)
(* permissions *)
(*--------------------------------------------------------------------------------------------------------------------------*)

type permission =
  | Accessed of expression_field_reference
  | Assumed  of predicate_check
[@@deriving sexp]

module PermissionSetElt =
struct
  type t = permission
  let compare = compare
  let sexp_of_t = sexp_of_permission
  let t_of_sexp = permission_of_sexp
end

module PermissionSet = Set.Make(PermissionSetElt)

module Permissions =
struct
  type set = PermissionSet.t
  type elt = PermissionSet.Elt.t

  let to_string : set -> string = Sexp.to_string @< PermissionSet.sexp_of_t

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
  let rec entails clsctx typctx ctx ps : elt -> bool =
    function
    (* accessed( e.f ) *)
    | Accessed predchk ->
      let f =
        function
        | Assumed _ -> false
        | Accessed predchk' ->
          (* aliased bases *)
          let os : ObjectValue.t list = List.map ~f:(ObjectValue.ofExpression_exn clsctx typctx)
              [ predchk.base; predchk'.base ] in
          AliasingContext.entails ctx ctx @@ AliasProp.of_list os &&
          (* and syntactically same field *)
          eqId predchk.field predchk'.field
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
            match ObjectValue.ofExpression clsctx typctx e, ObjectValue.ofExpression clsctx typctx e' with
            | Some o, Some o' -> AliasingContext.entails ctx ctx @@ AliasProp.of_list [o;o']
            | None,   None    -> syneqExpression e e'
            | _               -> failwith "UNIMPL: non-syntactical equality"
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

let rec frames clsctx typctx (ctx:AliasingContext.t) (perms:PermissionSet.t) =
  function
  | Imprecise _ -> failwith "imprecise formulae are always framed"
  | Concrete phi -> framesConcrete clsctx typctx ctx perms phi

and framesConcrete clsctx typctx (ctx:AliasingContext.t) (perms:PermissionSet.t) (phi:concrete) : bool =
  match phi with
  | Expression expr ->
    framesExpression clsctx typctx ctx perms expr
  | Predicate_check predchk ->
    List.for_all predchk.arguments ~f:(framesExpression clsctx typctx ctx perms)
  | Access_check accchk ->
    framesExpression clsctx typctx ctx perms accchk.base
  | Operation oper ->
    framesConcrete clsctx typctx ctx (PermissionSet.union perms (Permissions.grantedConcrete oper.left)) oper.right &&
    framesConcrete clsctx typctx ctx (PermissionSet.union perms (Permissions.grantedConcrete oper.right)) oper.left
  | If_then_else ite ->
    framesExpression clsctx typctx ctx perms ite.condition &&
    (let then_, scp = ite.then_ in framesConcrete clsctx typctx (AliasingContext.ofScope ctx scp) perms then_) &&
    (let else_, scp = ite.else_ in framesConcrete clsctx typctx (AliasingContext.ofScope ctx scp) perms else_)
  | Unfolding_in unfolin ->
    (framesConcrete clsctx typctx ctx perms @@ Predicate_check unfolin.predicate_check) &&
    (let predchk = unfolin.predicate_check in Permissions.entails clsctx typctx ctx perms @@ Assumed predchk) &&
    (let phi', scp = unfolin.formula in framesConcrete clsctx typctx (AliasingContext.ofScope ctx scp) perms phi')

and framesExpression clsctx typctx ctx perms expr : bool =
  let res = match expr with
  | Variable var ->
    true
  | Value vlu ->
    true
  | Operation oper ->
    framesExpression clsctx typctx ctx perms oper.left &&
    framesExpression clsctx typctx ctx perms oper.right
  | Comparison comp ->
    framesExpression clsctx typctx ctx perms comp.left &&
    framesExpression clsctx typctx ctx perms comp.right
  | Field_reference fldref ->
    framesExpression clsctx typctx ctx perms fldref.base &&
    Permissions.entails clsctx typctx ctx perms @@ Accessed fldref
  in
  debugList ~hide:true [
    "framesExpression:";
    "ctx    = "^AliasingContext.to_string ctx;
    "perms  = "^Permissions.to_string perms;
    "expr   = "^Sexp.to_string @@ sexp_of_expression expr;
    "result = "^string_of_bool res;
  ];
  res

(*--------------------------------------------------------------------------------------------------------------------------*)
(* self framing *)
(*--------------------------------------------------------------------------------------------------------------------------*)

let rec selfFrames clsctx typctx phi_root : bool =
  match phi_root with
  | Imprecise phi_root -> failwith "UNIMPL: self-framing for imprecise formulas"
  | Concrete  phi_root ->
    let ctx_root = AliasingContext.construct clsctx typctx (Concrete phi_root) in
    debugList ~hide:true [
      "selfFrames:";
      "phi_root = "^Sexp.to_string @@ sexp_of_concrete phi_root;
      "ctx_root = "^AliasingContext.to_string ctx_root;
    ];
    framesConcrete clsctx typctx ctx_root PermissionSet.empty phi_root
