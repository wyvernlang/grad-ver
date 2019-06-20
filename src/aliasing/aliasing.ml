open Core
open Sexplib.Std

open Ast
open Utility
open Wellformed

(* ------------------------------------------------------------------------------------------------------------------------ *)
(* definitions *)
(* ------------------------------------------------------------------------------------------------------------------------ *)

(* object value *)

(* TODO: fix aliasing.mli interface to match all the changes below, moving things into modules *)
(* TODO: fix all references after the changes below, moving things into modules *)

type objectvalue =
  | Value           of value
  | Variable        of variable
  | Field_reference of expression_field_reference
[@@deriving sexp]

module ObjectValueSet = Set.Make(
  struct
    type t = objectvalue
    let compare ov ov' = failwith "unimplemented"
    let sexp_of_t = sexp_of_objectvalue
    let t_of_sexp = objectvalue_of_sexp
  end)

module ObjectValue =
struct
  type t = objectvalue

  let to_objectvaluesetelt (ov:t) : ObjectValueSet.Elt.t = ov

  let of_expression expr : t option =
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

  let to_expression (ov:t) : expression =
    match ov with
    | Value    vlu -> Value vlu
    | Variable var -> Variable var
    | Field_reference fldref -> Field_reference fldref
end

(* alias proposition *)

type aliasprop = ObjectValueSet.t
[@@deriving sexp] (* not sure if I need this @@deriving here, but why not *)

module AliasPropSet = Set.Make(
  struct
    type t = aliasprop
    let compare = compare
    let sexp_of_t = sexp_of_aliasprop
    let t_of_sexp = aliasprop_of_sexp
  end)
type aliasprop_set = AliasPropSet.t
[@@deriving sexp]

(* TODO: finish modularizing things *)

module AliasProp =
struct
  type t = aliasprop

  let of_list : ObjectValue.t list -> t = ObjectValueSet.of_list

  (* proposition entailment *)
  (* find whether an element of ps is a superset of p *)
  let entailsAliased ps p : bool =
    AliasPropSet.exists ps ~f:(fun p' -> ObjectValueSet.is_subset p ~of_:p')
end

module AliasingContext =
struct
  type t =
    { parent   : t option;
      scope     : scope;
      props    : aliasprop_set;
      children : (label * t) list; }
  [@@deriving sexp]

  and label =
    | Condition of expression
    | Unfolding of predicate_check
  [@@deriving sexp]

  let getScope : t option -> scope =
    function
    | None -> root_scope
    | Some ctx -> ctx.scope

  let getParentScope ctx : scope = getScope ctx.parent

  let empty parent scp = { parent=parent; scope=scp; props=AliasPropSet.empty; children=[]; }

end

let objectValuesOfContext ctx : ObjectValueSet.t =
  AliasPropSet.fold ctx.props ~init:ObjectValueSet.empty ~f:ObjectValueSet.union

(* ------------------------------------------------------------------------------------------------------------------------ *)
(* utilities *)
(* ------------------------------------------------------------------------------------------------------------------------ *)

(* context merging *)

(* generically merge contexts with boolean operation filtering entailment *)
(* inherit the parent and scope of the first argument *)
let contextMergeWith boolop ctx ctx' : aliasing_context =
  let os = objectValuesOfContext ctx in
  let propsUnion ps ps' : aliasprop_set =
    let ps_new = ref AliasPropSet.empty in
    let addFullAliasProp o : unit =
      let f o' = boolop
          (propsEntailsAliased ps  (aliasprop_of_list [o;o']))
          (propsEntailsAliased ps' (aliasprop_of_list [o;o'])) in
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
  (* [ctx] is like the "current context". it is used for referencing [ctx.parent] and [ctx.scope] in the making of new empty
     contexts at the same level as [ctx] (sibling contexts) as well as new child contexts of [ctx]. *)
  let rec helper ctx phi =
    let empty_sibling_context = empty_context ctx.parent ctx.scope in
    let singleton_sibling_context prop =
      { parent    = ctx.parent;
        scope     = ctx.scope;
        props     = AliasPropSet.singleton prop;
        children  = []; } in
    match phi with
    | Expression expr ->
      begin
        match expr with
        | Variable var ->
          empty_sibling_context
        | Value vlu ->
          empty_sibling_context
        | Operation oper ->
          begin
            match oper.operator with
            (* form: e && e *)
            | And -> (helper ctx @@ Expression oper.left) +++ (helper ctx @@ Expression oper.right)
            (* form: e || e *)
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
                (* form: ov = ov' *)
                | (Some ov, Some ov') -> contextUnion ctx (singleton_sibling_context @@ aliasprop_of_list [ov;ov'])
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
    (* conditional formula branches into two aliasing-context children:
       - in one the condition is assumed true
       - in the other the condition is assumed false *)
    | If_then_else ite ->
      let child_then =
        let (phi', scp') = ite.then_ in
        let ctx_affirm = helper empty_sibling_context (Expression ite.condition) in
        ACL_Condition ite.condition, helper ctx_affirm phi' in
      let child_else =
        let (phi', scp') = ite.then_ in
        let ctx_negate = helper empty_sibling_context (Expression (negate ite.condition)) in
        ACL_Condition ite.condition, helper ctx_negate phi' in
      { parent   = Some ctx;
        props    = AliasPropSet.empty;
        children = [child_then; child_else];
        scope    = ctx.scope }
    | Unfolding_in unfolin ->
      let child =
        let (phi', scp') = unfolin.formula in
        (* TODO: unfold the predicate once with substituted arguments and then get its constructed aliasing-context *)
        let ctx_unfolded = failwith "TODO" in
        ACL_Unfolding unfolin.predicate_check, helper ctx_unfolded phi' in
      { parent   = ctx.parent;
        props    = AliasPropSet.empty;
        children = [child];
        scope    = ctx.scope }
  in
  function
  | Imprecise _ -> failwith "[!] unimplemented: construct_aliasing_context of imprecise formulas"
  | Concrete phi -> helper (empty_context None root_scope) phi

let rec subAliasingContextOfScope root_ctx scp =
  let rec helper ctx : aliasing_context option =
    if ctx.scope = scp
    (* at the target scope *)
    then Some ctx
    (* recursively search children for target scope *)
    else
      let f =
        function
        | None      -> fun (lab, ctx') -> helper ctx'
        | Some ctx' -> fun _           -> Some ctx'
      in
      List.fold_left root_ctx.children ~init:None ~f
  in
  match helper root_ctx with
  | Some ctx -> ctx
  | None     -> failwith "sub-aliasing-context of scope not found"
