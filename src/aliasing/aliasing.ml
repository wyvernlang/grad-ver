open Core
open Sexplib.Std
open Ast
open Wellformed

module Sexp = Sexplib.Sexp

(* ------------------------------------------------------------------------------------------------------------------------ *)
(* definitions *)
(* ------------------------------------------------------------------------------------------------------------------------ *)

(* object value *)

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

(* alias proposition *)

type aliasprop = ObjectValueSet.t
[@@deriving sexp]

module ALIASPROP = struct
  type t = aliasprop
  let compare = compare
  let sexp_of_t = sexp_of_aliasprop
  let t_of_sexp = aliasprop_of_sexp
end

module AliasPropSet = Set.Make(ALIASPROP)
type aliasprop_set = AliasPropSet.t
[@@deriving sexp]

type aliasing_context = {
  parent   : aliasing_context option;
  scope_id : scope_id;
  props    : aliasprop_set;
  children : (aliasing_context_label * aliasing_context) list;
} [@@deriving sexp]

and aliasing_context_label =
  | ACL_Condition of expression
  | ACL_Unfolding of predicate_check
[@@deriving sexp]

(* ------------------------------------------------------------------------------------------------------------------------ *)
(* utilities *)
(* ------------------------------------------------------------------------------------------------------------------------ *)

let empty_context parent sid =
  { parent=parent; props=AliasPropSet.empty; children=[]; scope_id=sid }

let collectObjectVariables ctx : ObjectValueSet.t =
  AliasPropSet.fold ctx.props ~init:ObjectValueSet.empty ~f:ObjectValueSet.union

(* find whether an element of ps is a superset of p *)
let propsEntailsAliased ps p : bool =
  AliasPropSet.exists ps ~f:(fun p' -> ObjectValueSet.is_subset p ~of_:p')

(* TODO: get [contextUnion] to work, need to somehow iterate/fold though set of object values
   to create a list of props (set)  *)

(* inherit the parent and scope_id of the first argument *)
let rec contextUnion ctx ctx' : aliasing_context =
  let os = collectObjectVariables ctx in
  let propsUnion ps ps' : aliasprop_set =
    let ps_new = ref AliasPropSet.empty in
    let addFullAliasProp o : unit =
      let f o' = propsEntailsAliased ps  (ObjectValueSet.of_list [o;o']) ||
                 propsEntailsAliased ps' (ObjectValueSet.of_list [o;o']) in
      ps_new := AliasPropSet.add !ps_new (ObjectValueSet.filter os ~f) in
    ObjectValueSet.iter os ~f:addFullAliasProp;
    !ps_new
  in
  { parent   = ctx.parent;
    props    = propsUnion ctx.props ctx'.props;
    children = ctx.children @ ctx'.children;
    scope_id = ctx.scope_id }
and (+++) ctx ctx' = contextUnion ctx ctx'

let rec contextIntersection ctx ctx' =
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

let rec getTotalAliasingContext ctx : aliasing_context =
  match ctx.parent with
  | Some parent_ctx -> contextUnion (getTotalAliasingContext parent_ctx) ctx
  | None -> ctx

let rec totalAliasProps ctx : aliasprop_set =
  (getTotalAliasingContext ctx).props

let entailsAliasProp ctx prop : bool =
  propsEntailsAliased (totalAliasProps ctx) prop

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
                                { condition = oper.left;
                                  (* its fine for this scope never to be considered because
                                     it can only ever just `true` as its contents *)
                                  then_ = (Expression (Value (Bool true), sid), sid);
                                  else_ = (Expression oper.right, sid) },
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
                { parent   = parent;
                  props    = AliasPropSet.singleton (ObjectValueSet.of_list [ov1;ov2]);
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
    { parent   = parent;
      props    = AliasPropSet.empty;
      children = children;
      scope_id = sid }
  | Unfolding_in unfolin ->
    let children = [ ACL_Unfolding unfolin.predicate_check, helper parent unfolin.formula ] in
    { parent   = parent;
      props    = AliasPropSet.empty;
      children = children;
      scope_id = sid }
