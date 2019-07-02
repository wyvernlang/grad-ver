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

  let of_objectvalue o = o

  let to_string : t -> string = Sexp.to_string @< sexp_of_objectvalue
  let to_t : objectvalue -> t = fun o -> o

  let ofExpression clsctx typctx expr : t option =
    match Wellformed.TypeContext.getExpressionType clsctx typctx expr with
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

  let ofExpression_exn clsctx typctx expr : t =
    match ofExpression clsctx typctx expr with
    | Some o -> o
    | None   -> failwith @@ "ofExpression_exn( "^Sexp.to_string (sexp_of_expression expr)^" )"

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
  let compare = ObjectValueSet.compare (* top-level [compare] doesn't work *)
  let sexp_of_t = sexp_of_aliasprop
  let t_of_sexp = aliasprop_of_sexp
end
module AliasPropSet = Set.Make(AliasPropSetElt)

module AliasProp =
struct
  type t = AliasPropSet.Elt.t

  let of_aliasprop p = p

  let to_string : t -> string = Sexp.to_string @< sexp_of_aliasprop

  let of_list : objectvalue list -> t = ObjectValueSet.of_list

  (* proposition entailment *)

  (* [ps |- p] <=> an element of ps is a superset of p *)
  let entails ps p : bool =
    debugList ~hide:true [
      "# checking entailment";
        (Sexp.to_string @@ AliasPropSet.sexp_of_t ps)^" |- aliased"^(Sexp.to_string @@ sexp_of_aliasprop p);
        " => "^string_of_bool
          begin
            if ObjectValueSet.length p <= 0 then true
            else AliasPropSet.exists ps ~f:(fun p' -> ObjectValueSet.is_subset p ~of_:p')
          end;
    ];
    (* the trivial aliasprop or the empty aliasprop are always entailed *)
    if ObjectValueSet.length p <= 1 then true
    else AliasPropSet.exists ps ~f:(fun p' -> ObjectValueSet.is_subset p ~of_:p')
end

(* utilities *)

let aliaspropset_of_objectvalue_list_list (oss:objectvalue list list) : AliasPropSet.t =
  AliasPropSet.of_list @@ List.map ~f:AliasProp.of_list oss

(* ------------------------------------------------------------------------------------------------------------------------ *)
(* aliasing context *)

type aliasingcontext =
  { parent   : scope option;
    scope    : scope;
    props    : AliasPropSet.t;
    children : aliasingcontext_child list; }
[@@deriving sexp]

and aliasingcontext_child = aliasingcontext_child_label * scope

and aliasingcontext_child_label =
  | Condition of expression
  | Unfolding of predicate_check
[@@deriving sexp]

module ScopingContext =
struct
  type t = (scope * aliasingcontext) list

  let add scpctx scp alictx : t = (scp, alictx) :: scpctx

  (* collects the union of all the aliasing-contexts with the same scope, concatenating their children *)
  let get scpctx scp : aliasingcontext =
    let alictxs = List.filter_map scpctx ~f:(fun (scp', alictx') -> if scp = scp' then Some alictx' else None) in
    match alictxs with
    | [] -> failwith "no aliasing contexts with scope found"
    | alictx_init::alictxs' ->
      List.fold_left alictxs' ~init:alictx_init ~f:begin
        fun alictx_accum alictx -> { alictx_accum with
                                     props=AliasPropSet.union alictx_accum.props alictx.props;
                                     children=alictx_accum.children @ alictx.children }
      end
end

module AliasingContext =
struct
  type t = aliasingcontext
  type child = aliasingcontext_child
  type label = aliasingcontext_child_label

  let to_string : t -> string = Sexp.to_string_hum ~indent:4 @< sexp_of_aliasingcontext

  (* equality *)

  let rec equal scpctx alictx alictx' : bool =
    let check : bool =
      (* equal parents *)
      alictx.parent = alictx'.parent &&
      (* equal scope *)
      alictx.scope  = alictx'.scope &&
      (* equal children *)
      List.equal
        begin
          fun (lab, child_scp) (lab', child_scp') ->
            lab = lab' &&
            let child, child' = ScopingContext.get scpctx lab, ScopingContext.get scpctx lab' in
            equal scpctx child child'
        end
        alictx.children alictx'.children &&
      AliasPropSet.equal alictx.props alictx'.props in
    debugList ~hide:true
      [ "# checking context equality";
        "ctx   : "^Sexp.to_string (sexp_of_aliasingcontext alictx);
        "ctx'  : "^Sexp.to_string (sexp_of_aliasingcontext alictx');
        "equal : "^string_of_bool check;
      ];
    check

  (* accessed *)

  let objectvaluesOf alictx : ObjectValueSet.t = AliasPropSet.fold alictx.props ~init:ObjectValueSet.empty ~f:ObjectValueSet.union

  (* context merging *)

  (* generically merge contexts with boolean operation filtering entailment *)
  (* inherit the parent and scope of the first argument *)
  let rec mergeWith boolop alictx alictx' : t =
    debugList ~hide:true
      [ "merging contexts:";
        "ctx  : "^to_string alictx;
        "ctx' : "^to_string alictx'; ];
    (* all the objectvalues avaliable to alias *)
    let os, os'     = objectvaluesOf alictx, objectvaluesOf alictx' in
    let os_all      = ObjectValueSet.union os os' in
    let os_all_list = ObjectValueSet.to_list os_all in
    (* the AliasPropSet entailed by the merging of [ctx] and [ctx'], with respect to the boolop *)
    let mergeProps ps ps' : AliasPropSet.t =
      (* the merged alias class of an objectvalue [o] is the aliasprop that contains [o] that
         the merging of [ctx] and [ctx'] entail *)
      let generateAliasClass o : AliasProp.t =
        AliasProp.of_list @@ List.filter os_all_list
          ~f:
            begin
              fun o' ->
                if o = o'
                then false (* don't include trivial aliases *)
                else
                  begin
                    let do_include = boolop
                        (AliasProp.entails ps  @@ AliasProp.of_list[ o;o' ])
                        (AliasProp.entails ps' @@ AliasProp.of_list[ o;o' ]) in
                    debugList ~hide:true [
                      "# include in merge: "^string_of_bool do_include;
                      "o, o' : "^ObjectValue.to_string o^", "^ObjectValue.to_string o';
                    ];
                    do_include
                  end
            end
      in
      (* there will be some duplicates, but these will be removed when converting to a set *)
      let ps_new'_list : AliasProp.t list =
        List.filter_map os_all_list
          ~f:
            begin
              fun o ->
                let cls_raw = generateAliasClass o in
                if ObjectValueSet.length cls_raw = 0
                (* ignore if trivial *)
                then
                  begin
                    debugList ~hide:true [
                      "o             : "^ObjectValue.to_string o;
                      "aliasClass(o) : trivial";
                    ];
                    None
                  end
                (* if not trivial, then add in [o] *)
                else
                  begin
                    let cls = ObjectValueSet.add cls_raw o in
                    debugList ~hide:true [
                      "o             : "^ObjectValue.to_string o;
                      "aliasClass(o) : "^AliasProp.to_string cls;
                    ];
                    Some cls
                  end
            end
      in
      debug ~hide:true @@ "ps_new'_list : "^Sexp.to_string @@ sexp_of_list sexp_of_aliasprop ps_new'_list;
      AliasPropSet.of_list ps_new'_list
    in
    { parent    = alictx.parent;
      scope     = alictx.scope;
      props     = mergeProps alictx.props alictx'.props;
      children  = mergeChildrenWith boolop alictx.children alictx'.children; }

  (* concatentate children, merging children with shared labels *)
  and mergeChildrenWith boolop (cs:aliasingcontext_child list) (cs':aliasingcontext_child list) : child list =
    let f (cs':aliasingcontext_child list) = function (lab, alictx) as c ->
        (* if any child in cs' has same label l, merge with it *)
        (* otherwise, append c to cs' *)
        let result = List.fold_left cs' ~init:None
          ~f:
            begin
              fun wrk (lab', alictx') ->
              match wrk with
              | Some _ -> wrk
              | None ->
                if lab = lab'
                then Some [lab, mergeWith boolop alictx alictx'] (* match, so merge child contexts *)
                else None                                   (* no match yet *)
            end
        in
        match result with
        | Some cs'' -> cs''      (* some child had matching label, so merged *)
        | None      -> c::cs' (* just append c to cs' *)
    in
    (* start with cs, the iterate over cs' merging any children with a label matching a label in cs *)
    List.fold_left cs' ~init:cs ~f

  let union = mergeWith (||)
  let inter = mergeWith (&&)

  (*------------------------------------------------------------------------------------------------------------------------*)
  (* entailment from aliasing context *)
  (*------------------------------------------------------------------------------------------------------------------------*)

  (* search through the root aliasing context's tree for the given scope *)
  let rec ofScope (root_ctx:t) (scp:scope) : t =
    let rec helper (ctx':t) : t option =
      debugList ~hide:true [ "looking for scope="^string_of_scope scp^" in:";
                             to_string alictx' ];
      if alictx'.scope = scp
      then Some ctx'
      else List.fold_left alictx'.children ~init:None
          ~f:begin
            fun found_opt (_, child) ->
              match found_opt with
              | Some found -> Some found (* already found match *)
              | None -> helper child (* haven't found match yet, so keep looking *)
          end
    in
    match helper root_ctx with
    | Some alictx -> ctx
    | None     -> failwith @@ "context of scope not found: scp="^(Sexp.to_string @@ sexp_of_scope scp)

  (* gather all the props of a context and its ancestry *)
  let rec totalAliasProps ctx_root alictx : AliasPropSet.t =
    let rec helper alictx' : AliasPropSet.t =
      match alictx'.parent with
      | Some scp -> AliasPropSet.union alictx'.props @@ helper (ofScope ctx_root scp)
      | None     -> alictx'.props
    in
    helper ctx

  let entails ctx_root alictx prop : bool = AliasProp.entails (totalAliasProps ctx_root ctx) prop

  let construct clsctx typctx : formula -> t =
    (* [ctx] is like the "current context". it is used for referencing [ctx.parent] and [ctx.scope] in the making of new empty
       contexts at the same level as [ctx] (sibling contexts) as well as new child contexts of [ctx]. *)
    let rec helper alictx phi =
      let empty_sibling = { parent=ctx.parent;
                            scope=ctx.scope;
                            props=AliasPropSet.empty;
                            children=[] } in
      let singleton_sibling p = { parent=ctx.parent;
                                  scope=ctx.scope;
                                  props=AliasPropSet.singleton p;
                                  children=[] } in
      let empty_child () = { parent=Some alictx.scope;
                             scope=makeScope();
                             props=AliasPropSet.empty;
                             children=[] } in
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
                | And -> union (helper alictx @@ Expression oper.left) (helper alictx @@ Expression oper.right)
                (* form: e || e *)
                | Or  -> helper alictx @@ If_then_else
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
                    match ObjectValue.ofExpression clsctx typctx comp.left, ObjectValue.ofExpression clsctx typctx comp.right with
                    (* form: o = o' *)
                    | (Some o, Some o') -> union alictx (singleton_sibling @@ AliasProp.of_list [o;o'])
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
          union (helper alictx oper.left) (helper alictx oper.right)
        (* conditional formula branches into two aliasing-context children:
           - in one the condition is assumed true
           - in the other the condition is assumed false *)
        | If_then_else ite ->
          let child_then =
            let (phi', scp') = ite.then_ in
            let ctx_affirmed = helper (empty_child ()) (Expression ite.condition) in
            Condition ite.condition, helper ctx_affirmed phi' in
          let child_else =
            let (phi', scp') = ite.then_ in
            let ctx_negated = helper (empty_child ()) (Expression (negateExpression ite.condition)) in
            Condition ite.condition, helper ctx_negated phi'
          in
          { parent   = alictx.parent;
            props    = AliasPropSet.empty;
            children = [child_then; child_else];
            scope    = alictx.scope }
        | Unfolding_in unfolin ->
          let child =
            let (phi', scp') = unfolin.formula in
            (* TODO: unfold the predicate once with substituted arguments and then get its constructed aliasing-context *)
            let ctx_unfolded = failwith "UNIMPL: constructing aliasing context for Unfolding_in formula" in
            Unfolding unfolin.predicate_check, helper ctx_unfolded phi'
          in
          { parent   = alictx.parent;
            props    = AliasPropSet.empty;
            children = [child];
            scope    = alictx.scope }
      end
    in
    function
    | Imprecise _   -> failwith "[!] unimplemented: construct_aliasing_context of imprecise formulas"
    | Concrete phi ->
      helper { parent=None; scope=root_scope; props=AliasPropSet.empty; children=[] } phi

end
