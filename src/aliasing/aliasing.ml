open Core
open Sexplib.Std

open Ast
open Utility
open Wellformed

(* ------------------------------------------------------------------------------------------------------------------------ *)
(* definitions *)
(* ------------------------------------------------------------------------------------------------------------------------ *)

(* object value *)

type objectvalue =
  | OV_Value            of value
  | OV_Variable         of variable
  | OV_Field_reference  of expression_field_reference
[@@deriving sexp]

module ObjectValueSet = Set.Make(
  struct
    type t = objectvalue
    let compare ov ov' = failwith "unimplemented"
    let sexp_of_t = sexp_of_objectvalue
    let t_of_sexp = objectvalue_of_sexp
  end)

let objectvaluesetelt_of_objectvalue (ov:objectvalue) : ObjectValueSet.Elt.t = ov

let objectvalue_of_expression expr : objectvalue option =
  match synthesizeType expr with
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

let expression_of_objectvalue (ov:objectvalue) : expression =
  match ov with
  | OV_Value    vlu -> Value vlu
  | OV_Variable var -> Variable var
  | OV_Field_reference fldref -> Field_reference fldref

(* alias proposition *)

type aliasprop = ObjectValueSet.t
[@@deriving sexp]

module AliasPropSet = Set.Make(
  struct
    type t = aliasprop
    let compare = compare
    let sexp_of_t = sexp_of_aliasprop
    let t_of_sexp = aliasprop_of_sexp
  end)
type aliasprop_set = AliasPropSet.t
[@@deriving sexp]

type aliasing_context = {
  parent   : aliasing_context option;
  scope : scope;
  props    : aliasprop_set;
  children : (aliasing_context_label * aliasing_context) list;
} [@@deriving sexp]

and aliasing_context_label =
  | ACL_Condition of expression
  | ACL_Unfolding of predicate_check
[@@deriving sexp]

let getScope : aliasing_context option -> scope =
  function
  | None -> root_scope
  | Some ctx -> ctx.scope

let getParentScope ctx : scope =
  getScope ctx.parent

(* ------------------------------------------------------------------------------------------------------------------------ *)
(* utilities *)
(* ------------------------------------------------------------------------------------------------------------------------ *)

let empty_context parent scp =
  { parent=parent; scope=scp; props=AliasPropSet.empty; children=[];  }

let objectValuesOfContext ctx : ObjectValueSet.t =
  AliasPropSet.fold ctx.props ~init:ObjectValueSet.empty ~f:ObjectValueSet.union

(* proposition entailment *)

(* find whether an element of ps is a superset of p *)
let propsEntailsAliased ps p : bool =
  AliasPropSet.exists ps ~f:(fun p' -> ObjectValueSet.is_subset p ~of_:p')

(* context merging *)

(* generically merge contexts with boolean operation filtering entailment *)
(* inherit the parent and scope of the first argument *)
let contextMergeWith boolop ctx ctx' : aliasing_context =
  let os = objectValuesOfContext ctx in
  let propsUnion ps ps' : aliasprop_set =
    let ps_new = ref AliasPropSet.empty in
    let addFullAliasProp o : unit =
      let f o' = boolop
          (propsEntailsAliased ps  (ObjectValueSet.of_list [o;o']))
          (propsEntailsAliased ps' (ObjectValueSet.of_list [o;o'])) in
      ps_new := AliasPropSet.add !ps_new (ObjectValueSet.filter os ~f) in
    ObjectValueSet.iter os ~f:addFullAliasProp;
    !ps_new in
  { parent    = ctx.parent;
    scope     = ctx.scope;
    props     = propsUnion ctx.props ctx'.props;
    children  = ctx.children @ ctx'.children; }

let contextUnion        = contextMergeWith (||)
let contextIntersection = contextMergeWith (&&)

let (+++) = contextUnion
let (&&&) = contextIntersection

(* expressions *)

let rec negate : expression -> expression =
  function
  | Comparison comp -> Comparison { comp with comparer=negateComparer comp.comparer }
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

let aliasingContextEntailsAliasProp ctx prop : bool =
  propsEntailsAliased (totalAliasProps ctx) prop

(* ----------------------------------------------------------------------------------------------------------------------- *)
(* constructing aliasing context *)
(* ----------------------------------------------------------------------------------------------------------------------- *)

let rec constructAliasingContext : formula -> aliasing_context =
  function
  | Imprecise _ -> failwith "[!] unimplemented: construct_aliasing_context of imprecise formulas"
  | Concrete phi -> helper (empty_context None root_scope) phi

(* [ctx] is like the "current context". it is used for referencing [ctx.parent] and [ctx.scope] in the making of new empty
   contexts at the same level as [ctx] (sibling contexts) as well as new child contexts of [ctx]. *)
and helper ctx phi =
  let empty_sibling_context = empty_context ctx.parent ctx.scope in
  match phi with
  | Expression expr -> begin
      match expr with
      | Variable var ->
        empty_sibling_context
      | Value vlu ->
        empty_sibling_context
      | Operation oper ->
        begin
          match oper.operator with
          | And -> (helper ctx @@ Expression oper.left) +++ (helper ctx @@ Expression oper.right)
          (* TODO: this won't work exactly because the scopes it makes won't correspond to scopes in the Ast *)
          | Or  -> helper ctx @@ If_then_else
              { condition = oper.left;
                then_     = (Expression (Value (Bool true)), makeScope ());
                else_     = (Expression oper.right, makeScope ()); }
          | _ -> empty_sibling_context
        end
      | Comparison comp ->
        begin
          match comp.comparer with
          | Eq ->
            begin
              match objectvalue_of_expression comp.left, objectvalue_of_expression comp.right with
              | (Some ov1, Some ov2) ->
                let ctx' = {
                  parent   = Some ctx;
                  props    = AliasPropSet.singleton (ObjectValueSet.of_list [ov1;ov2]);
                  children = [];
                  scope    = ctx.scope
                } in
                contextUnion ctx ctx'
              | _ -> empty_sibling_context
            end
          | _ -> empty_sibling_context
        end
      | Field_reference fldref ->
        empty_sibling_context
  end
  | Predicate_check predchk ->
    empty_sibling_context
  | Access_check accchk ->
    empty_sibling_context
  | Operation oper ->
    (helper ctx oper.left) +++ (helper ctx oper.right)
  | If_then_else ite ->
    let child_then =
      let (phi', scp') = ite.then_ in
      ACL_Condition ite.condition, helper (empty_context (Some ctx) scp') phi' in
    let child_else =
      let (phi', scp') = ite.else_ in
      ACL_Condition (negate ite.condition), helper (empty_context (Some ctx) scp') phi' in
    { parent   = Some ctx;
      props    = AliasPropSet.empty;
      children = [child_then; child_else];
      scope    = ctx.scope }
  | Unfolding_in unfolin ->
    let child =
      let (phi', scp') = unfolin.formula in
      ACL_Unfolding unfolin.predicate_check, helper (empty_context (Some ctx) scp') phi' in
    { parent   = ctx.parent;
      props    = AliasPropSet.empty;
      children = [child];
      scope    = ctx.scope }

let aliasingContextOfScope : formula -> scope -> aliasing_context = failwith "TODO"
