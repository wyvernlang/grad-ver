open Core

open Ast
open Wellformed

open Utility
open Functools

(* ------------------------------------------------------------------------------------------------------------------------ *)
(* definitions *)
(* ------------------------------------------------------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------------------------------------------------------ *)
(* object value *)

type objectvalue =
    Value of value
  | Variable of variable
  | Field_reference of expression_field_reference
[@@deriving sexp]

module type OBJECTVALUESETELT =
sig
  type t = objectvalue [@@deriving sexp]
  val compare : t -> t -> int
  val sexp_of_t : t -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t
end

module ObjectValueSetElt : OBJECTVALUESETELT =
struct
  type t = objectvalue
  let compare = compare
  let sexp_of_t = sexp_of_objectvalue
  let t_of_sexp = objectvalue_of_sexp
end

module ObjectValueSet = Set.Make(ObjectValueSetElt)

module ObjectValue =
struct
  type t = ObjectValueSet.Elt.t

  let ofExpression expr : t option =
    match synthesizeType expr with
    | Class id ->
      begin
        match expr with
        | Variable var -> Some (Variable var)
        | Value vlu -> Some (Value vlu)
        | Field_reference fldref -> Some (Field_reference fldref)
        (* impossible case: other expressions cannot have type Class id *)
        | _ -> failwith "Class instance id declared as wrong type."
      end
    | _ -> None

  let toExpression (ov:t) : expression =
    match ov with
    | Value    vlu -> Value vlu
    | Variable var -> Variable var
    | Field_reference fldref -> Field_reference fldref
end

let ofObjectValueSetElt o : ObjectValue.t = o

(* ------------------------------------------------------------------------------------------------------------------------ *)
(* aliasing proposition *)

type aliasprop = ObjectValueSet.t [@@deriving sexp]

module AliasPropSetElt =
struct
  type t = aliasprop
  let compare = compare
  let sexp_of_t = sexp_of_aliasprop
  let t_of_sexp = aliasprop_of_sexp
end
module AliasPropSet = Set.Make(AliasPropSetElt)

module AliasProp =
struct
  type t = AliasPropSet.Elt.t

  let of_list : objectvalue list -> t = ObjectValueSet.of_list

  (* proposition entailment *)

  (* [ps |- p] <=> an element of ps is a superset of p *)
  let entails ps p : bool =
    print_endline @@
    (Sexp.to_string @@ AliasPropSet.sexp_of_t ps)^
    " |- aliased"^(Sexp.to_string @@ sexp_of_aliasprop p)^
    " => "^string_of_bool
      begin
        if ObjectValueSet.length p = 1 then true
        else AliasPropSet.exists ps ~f:(fun p' -> ObjectValueSet.is_subset p ~of_:p')
      end;
    if ObjectValueSet.length p = 1
    (* if p is a singleton set, then it is the proposition that an [o] is an alias of itself, which is always true *)
    then true
    else AliasPropSet.exists ps ~f:(fun p' -> ObjectValueSet.is_subset p ~of_:p')
end

(* utilities *)

let aliaspropset_of_objectvalue_list_list (oss:objectvalue list list) : AliasPropSet.t =
  AliasPropSet.of_list @@ List.map ~f:AliasProp.of_list oss

(* ------------------------------------------------------------------------------------------------------------------------ *)
(* aliasing context *)

type aliasingcontext =
  { parent   : aliasingcontext option;
    scope    : scope;
    props    : AliasPropSet.t;
    children : aliasingcontext_child list; }
[@@deriving sexp]

and aliasingcontext_child_label =
  | Condition of expression
  | Unfolding of predicate_check
[@@deriving sexp]

and aliasingcontext_child = (aliasingcontext_child_label * aliasingcontext)
[@@deriving sexp]

module AliasingContext =
struct
  type t = aliasingcontext
  type child = aliasingcontext_child
  type label = aliasingcontext_child_label

  (* equality *)

  let equal ctx ctx' : bool =
    ctx.parent    = ctx'.parent   &&
    ctx.scope     = ctx'.scope    &&
    ctx.children  = ctx'.children &&
    AliasPropSet.equal ctx.props ctx'.props (* use special set equality *)

  (* accessed *)

  let parentScopeOf  ctx : scope            = match ctx.parent with None -> root_scope | Some parent -> parent.scope
  let objectvaluesOf ctx : ObjectValueSet.t = AliasPropSet.fold ctx.props ~init:ObjectValueSet.empty ~f:ObjectValueSet.union

  (* context merging *)

  (* generically merge contexts with boolean operation filtering entailment *)
  (* inherit the parent and scope of the first argument *)
  let rec mergeWith boolop ctx ctx' : t =
    let os, os' = objectvaluesOf ctx, objectvaluesOf ctx' in
    let os_all  = ObjectValueSet.union os os' in
    let mergeProps ps ps' : AliasPropSet.t =
      let divider      = "-----------------------------------------------------\n" in
      let half_divider = "---------------------------\n" in
      debug @@
      divider^divider^
      "merging props:\n"^
      " ps  : "^Sexp.to_string (AliasPropSet.sexp_of_t ps)^"\n"^
      " ps' : "^Sexp.to_string (AliasPropSet.sexp_of_t ps');
      let ps_new = ref AliasPropSet.empty in
      let addFullAliasProp o : unit =
        let f o' =
          debug @@
          divider^
          " o   : "^Sexp.to_string (ObjectValueSet.Elt.sexp_of_t o')^"\n"^
          " o'  : "^Sexp.to_string (ObjectValueSet.Elt.sexp_of_t o')^"\n"^
          " ps  : "^Sexp.to_string (AliasPropSet.sexp_of_t ps)^"\n"^
          " ps' : "^Sexp.to_string (AliasPropSet.sexp_of_t ps')^"\n"^
          half_divider^
          " ps  |- aliased(o,o') : "^string_of_bool (AliasProp.entails ps  (AliasProp.of_list [o;o']))^"\n"^
          " ps' |- aliased(o,o') : "^string_of_bool (AliasProp.entails ps' (AliasProp.of_list [o;o']));
          boolop
            (AliasProp.entails ps  (AliasProp.of_list [o;o']))
            (AliasProp.entails ps' (AliasProp.of_list [o;o'])) in
        ps_new := AliasPropSet.add !ps_new (ObjectValueSet.filter os_all ~f) in
      ObjectValueSet.iter os_all ~f:addFullAliasProp;
      !ps_new in
    { parent    = ctx.parent;
      scope     = ctx.scope;
      props     = mergeProps ctx.props ctx'.props;
      children  = mergeChildrenWith boolop ctx.children ctx'.children; }

  (* concatentate children, merging children with shared labels *)
  and mergeChildrenWith boolop cs cs' : child list =
    let f cs' = function (l, ctx) as c ->
        (* if any child in cs' has same label l, merge with it *)
        (* otherwise, append c to cs' *)
        let res = List.fold_left cs' ~init:None
          ~f:
            begin
              fun wrk (l',ctx') ->
              match wrk with
              | Some _ -> wrk
              | None ->
                if l = l'
                then Some [l, mergeWith boolop ctx ctx'] (* match, so merge child contexts *)
                else None                                (* no match yet *)
            end in
        match res with
        | Some cs'' -> cs''      (* some child had matching label, so merged *)
        | None      -> cs' @ [c] (* just append c to cs' *)
    in
    (* start with cs, the iterate over cs' merging any children with a label matching a label in cs *)
    List.fold_left cs' ~init:cs ~f

  let union = mergeWith (||)
  let inter = mergeWith (&&)

  (*------------------------------------------------------------------------------------------------------------------------*)
  (* entailment from aliasing context *)
  (*------------------------------------------------------------------------------------------------------------------------*)

  let rec getTotal ctx : t =
    match ctx.parent with
    | None -> ctx
    | Some parent_ctx -> union (getTotal parent_ctx) ctx
  let totalAliasProps ctx : AliasPropSet.t = (getTotal ctx).props

  let entails ctx prop : bool = AliasProp.entails (totalAliasProps ctx) prop

  let construct : formula -> t =
    (* [ctx] is like the "current context". it is used for referencing [ctx.parent] and [ctx.scope] in the making of new empty
       contexts at the same level as [ctx] (sibling contexts) as well as new child contexts of [ctx]. *)
    let rec helper ctx phi =
      let empty_sibling       = { parent=ctx.parent; scope=ctx.scope; props=AliasPropSet.empty;       children=[] } in
      let singleton_sibling p = { parent=ctx.parent; scope=ctx.scope; props=AliasPropSet.singleton p; children=[] } in
      begin
        match phi with
        | Expression expr ->
          begin
            match expr with
            | Variable var ->
              empty_sibling
            | Value vlu ->
              empty_sibling
            | Operation oper ->
              begin
                match oper.operator with
                (* form: e && e *)
                | And -> union (helper ctx @@ Expression oper.left) (helper ctx @@ Expression oper.right)
                (* form: e || e *)
                (* TODO: this won't work exactly because the scopes it makes won't correspond to scopes in the Ast *)
                | Or  -> helper ctx @@ If_then_else
                    { condition = oper.left;
                      then_     = (Expression (Value (Bool true)), makeScope ());
                      else_     = (Expression oper.right, makeScope ()); }
                | _ -> empty_sibling
              end
            | Comparison comp ->
              begin
                match comp.comparer with
                | Eq ->
                  begin
                    match ObjectValue.ofExpression comp.left, ObjectValue.ofExpression comp.right with
                    (* form: o = o' *)
                    | (Some o, Some o') -> union ctx (singleton_sibling @@ AliasProp.of_list [o;o'])
                    (* non-objectvalues cannot be aliases *)
                    | _ -> empty_sibling
                  end
                (* only keep track of positive aliasing propositions *)
                | _ -> empty_sibling
              end
            | Field_reference fldref ->
              empty_sibling
          end
        | Predicate_check predchk ->
          empty_sibling
        | Access_check accchk ->
          empty_sibling
        | Operation oper ->
          union (helper ctx oper.left) (helper ctx oper.right)
        (* conditional formula branches into two aliasing-context children:
           - in one the condition is assumed true
           - in the other the condition is assumed false *)
        | If_then_else ite ->
          let child_then =
            let (phi', scp') = ite.then_ in
            let ctx_affirmed = helper empty_sibling (Expression ite.condition) in
            Condition ite.condition, helper ctx_affirmed phi' in
          let child_else =
            let (phi', scp') = ite.then_ in
            let ctx_negated = helper empty_sibling (Expression (negateExpression ite.condition)) in
            Condition ite.condition, helper ctx_negated phi'
          in
          { parent   = Some ctx;
            props    = AliasPropSet.empty;
            children = [child_then; child_else];
            scope    = ctx.scope }
        | Unfolding_in unfolin ->
          let child =
            let (phi', scp') = unfolin.formula in
            (* TODO: unfold the predicate once with substituted arguments and then get its constructed aliasing-context *)
            let ctx_unfolded = failwith "TODO" in
            Unfolding unfolin.predicate_check, helper ctx_unfolded phi'
          in
          { parent   = ctx.parent;
            props    = AliasPropSet.empty;
            children = [child];
            scope    = ctx.scope }
      end
    in
    function
    | Imprecise _   -> failwith "[!] unimplemented: construct_aliasing_context of imprecise formulas"
    | Concrete  phi -> helper { parent=None; scope=root_scope; props=AliasPropSet.empty; children=[] } phi

  (* Get the sub-aliasing-context nested in [root_ctx] that has the scope [tgt_scp]. *)
  let rec ofScope (root_ctx:t) (tgt_scp:scope) : t =
    let rec helper : t option -> t -> t option =
      function
      | Some ctx -> fun _    -> Some ctx
      | None     -> fun ctx' ->
        if ctx'.scope = tgt_scp
        then Some ctx'
        else List.fold_left ctx'.children ~init:None ~f:(fun ctx'_op (_, child) -> helper ctx'_op child)
    in
    match helper None root_ctx with
    | Some ctx -> ctx
    | None     -> failwith "sub-aliasing-context of scope not found"
end
