open Core
open Sexplib.Std
open Ast
open Wellformed

(* ------------------------------------------------------------------------------------------------------------------------ *)
(* definitions *)
(* ------------------------------------------------------------------------------------------------------------------------ *)

type objectvalue =
  | OV_Value            of value
  | OV_Variable         of variable
  | OV_Field_reference  of expression_field_reference
  | OV_Null
[@@deriving sexp]

module OBJECTVALUE = struct
  type t = objectvalue
  let compare ov ov' = failwith "unimplemented"
  let sexp_of_t = sexp_of_objectvalue
  let t_of_sexp = objectvalue_of_sexp
end

module ObjectValueSet = Set.Make(OBJECTVALUE)
type objectvalue_set = ObjectValueSet.t

let extract_objectvalue : expression -> objectvalue option =
  function (expr, scope) as expression ->
  match synthesizeType expression with
  | Class id ->
    begin
      match expr with
      | Variable var -> Some (OV_Variable var)
      | Value vlu -> Some (OV_Value vlu)
      | Field_reference fldref -> Some (OV_Field_reference fldref)
      (* impossible case: other expressions cannot have type Class id *)
      | _ -> failwith "Class instance has invalid type "
    end
  | _ -> None

type aliased_prop = objectvalue_set
type aliased_props = aliased_prop list

type aliasing_context = {
  parent        : aliasing_context option;
  aliased_props : aliased_prop list;
  children      : (aliasing_context_label * aliasing_context) list;
  scope_id      : scope_id
}

and aliasing_context_label =
  | ACL_Condition of expression
  | ACL_Unfolding of predicate_check

(* ------------------------------------------------------------------------------------------------------------------------ *)
(* utilities *)
(* ------------------------------------------------------------------------------------------------------------------------ *)

let empty_context parent sid =
  { parent=parent; aliased_props=[]; children=[]; scope_id=sid }

let collectObjectVariables ctx : objectvalue_set =
  failwith "TODO"

let propsEntailsAliased props prop =
  failwith "TODO"

let rec contextUnion ctx ctx' =
  assert (ctx.parent = ctx'.parent);
  assert (ctx.scope_id = ctx'.scope_id);
  let ovs = collectObjectVariables ctx
  let propsUnion ps ps' =
  in
  let new_props = propsUnion ctx.aliased_props ctx'.aliased_props in
  { parent        = ctx.parent;
    aliased_props = new_props;
    children      = ctx.children @ ctx'.children;
    scope_id      = ctx.scope_id }
and (+++) ctx ctx' = contextUnion ctx ctx'

let rec contextIntersection : aliasing_context -> aliasing_context -> aliasing_context =
  fun ctx ctx' ->
  failwith "TODO"
and (&&&) ctx ctx' = contextIntersection ctx ctx'

let rec negate : expression -> expression =
  function
  | (Comparison comp, sid) -> (Comparison { comp with comparer=negateComparer comp.comparer }, sid)
  | expression -> expression
and negateComparer =
  function
  | Neq -> Eq
  | Eq -> Neq
  | Lt -> Ge
  | Gt -> Le
  | Ge -> Lt
  | Le -> Gt

(* ----------------------------------------------------------------------------------------------------------------------- *)
(* entailment from aliasing context *)
(* ----------------------------------------------------------------------------------------------------------------------- *)

let getTotalAliasedProps ctx =
  let props = failwith "TODO" in
  match ctx.parent with
  | parent_ctx -> contextUnion props @@ getTotalAliasedProps parent_ctx
  | None -> props

let entailsAliased ctx prop =
  propsEntailsAliased (getTotalAliasedProps ctx) prop

(* ----------------------------------------------------------------------------------------------------------------------- *)
(* constructing aliasing context *)
(* ----------------------------------------------------------------------------------------------------------------------- *)

let rec constructAliasingContext : formula -> aliasing_context =
  function
  | Imprecise _ -> failwith "[!] unimplemented: construct_aliasing_context of imprecise formulas"
  | Concrete (phi, sid) -> helper None (phi, sid)

and helper parent (conc, sid) =
  match conc with
  | Expression (expr, sid) -> begin
      match expr with
      | Variable var ->
        empty_context parent sid
      | Value vlu ->
        empty_context parent sid
      | Operation oper ->
        begin
          match oper.operator with
          | And -> (helper parent @@ (Expression oper.left, sid)) +++ (helper parent @@ (Expression oper.right, sid))
          | Or  -> helper parent @@ (If_then_else
                                { condition=oper.left;
                                  (* its fine for this scope never to be considered because
                                     it can only ever just `true` as its contents *)
                                  then_=(Expression (Value (Bool true), sid), sid);
                                  else_=(Expression oper.right, sid) },
                              sid)
          | _ -> empty_context parent sid
        end
      | Comparison comp ->
        begin
          match comp.comparer with
          | Eq ->
            begin
              match extract_objectvalue comp.left, extract_objectvalue comp.right with
              | (Some ov1, Some ov2) ->
                { parent = parent;
                  aliased_props = [ [ov1;ov2] ];
                  children = [];
                  scope_id = sid }
              | _ -> empty_context parent sid
            end
          | _ -> empty_context parent sid
        end
      | Field_reference fldref ->
        empty_context parent sid
  end
  | Predicate_check predchk ->
    empty_context parent sid
  | Access_check accchk ->
    empty_context parent sid
  | Operation oper ->
    (helper parent oper.left) +++ (helper parent oper.right)
  | If_then_else ite ->
    let children =
      [ ACL_Condition ite.condition, helper parent ite.then_;
        ACL_Condition (negate ite.condition), helper parent ite.else_ ] in
    { parent        = parent;
      aliased_props = [];
      children      = children;
      scope_id      = sid }
  | Unfolding_in unfolin ->
    let children = [ ACL_Unfolding unfolin.predicate_check, helper parent unfolin.formula ] in
    { parent        = parent;
      aliased_props = [];
      children      = children;
      scope_id      = sid }
