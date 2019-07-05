open Core

open Ast
open Wellformed

open Utility
open Functools

(** TODO: I need to fix up the merging of AliasingContext children. Right now, it is implemented naively as just
    concatentation. But there are cases in which this is too weak. For now its fine though - it makes things pretty
    complicated... *)

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
    debugList ~hide:true
      [ "# checking entailment";
        (Sexp.to_string @@ AliasPropSet.sexp_of_t ps)^" |- aliased"^(Sexp.to_string @@ sexp_of_aliasprop p);
        " => "^string_of_bool
          begin
            if ObjectValueSet.length p <= 0 then true
            else AliasPropSet.exists ps ~f:(fun p' -> ObjectValueSet.is_subset p ~of_:p')
          end; ];
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

(* basically a Hashtbl of scope => aliasingcontext *)
module ScopingContext =
struct
  (* scope => aliasingcontext *)
  type v = (scope * aliasingcontext) list
  type t = v ref

  let create () : t = ref []

  let get (scpctx:t) (scp:scope) : aliasingcontext =
    debug ~hide:true @@ "looking for scope: "^string_of_scope scp;
    List.find_map_exn !scpctx
      ~f:(fun (scp', alictx') ->
          if scp = scp'
          then Some alictx'
          else None)

  let add (scpctx:t) (scp:scope) (alictx:aliasingcontext) : unit =
    let merge (alictx:aliasingcontext) (alictx':aliasingcontext) : aliasingcontext =
      { parent   = alictx.parent;
        scope    = alictx.scope;
        props    = AliasPropSet.union alictx.props alictx'.props;
        children = alictx.children @ alictx'.children }
    in
    let f (scpctx_accum, was_merged) (scp', alictx') =
      (* merge contexts with the same scope *)
      if not was_merged && scp = scp'
      then (scp, merge alictx alictx')::scpctx_accum, true
      else (scp', alictx')::scpctx_accum, was_merged
    in
    (* only append to end if wasn't merged with another context *)
    let (scpctx_new, was_merged) = List.fold_left !scpctx ~init:([], false) ~f in
    scpctx := if was_merged then scpctx_new else (scp, alictx)::scpctx_new
end

module AliasingContext =
struct
  type t = aliasingcontext
  type child = aliasingcontext_child
  type label = aliasingcontext_child_label

  let to_string : t -> string = Sexp.to_string_hum ~indent:4 @< sexp_of_aliasingcontext

  (** Collects the set of object values that appear at the top level of the given context (not including children). *)
  let objectvaluesOf alictx : ObjectValueSet.t =
    AliasPropSet.fold alictx.props ~init:ObjectValueSet.empty ~f:ObjectValueSet.union

  (** Equality; requires special Set.equal for AliasPropSet *)
  let rec equal (scpctx:ScopingContext.t) (alictx:t) (alictx':t) : bool =
    let check : bool =
      (* equal parents *)
      alictx.parent = alictx'.parent &&
      (* equal scope *)
      alictx.scope  = alictx'.scope &&
      (* equal children *)
      List.equal
        begin
          fun ((lab, child_scp):label * scope) ((lab', child_scp'):label * scope) ->
            lab = lab' &&
            let child  = ScopingContext.get scpctx child_scp  in
            let child' = ScopingContext.get scpctx child_scp' in
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

  (** Merging aliasing-contexts. *)

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
      children  = alictx.children @ alictx'.children; }

  let union = mergeWith (||)
  let inter = mergeWith (&&)

  (** Combines a sub-context's aliasing proposition set with all ancestors *)
  let rec totalAliasProps scpctx alictx  : AliasPropSet.t =
    match alictx.parent with
    | None -> alictx.props
    | Some parent_scp ->
      let parent_alictx = ScopingContext.get scpctx parent_scp in
      let parent_props = totalAliasProps scpctx parent_alictx in
      AliasPropSet.union alictx.props parent_props

  (** Evaluates the judgement that a given aliasing-context entails that an aliasing proposition is true. In other words,
      finds an element (object variable set) of the total aliasing proposition set of the context that is a superset of the
      given aliasing proposition. *)
  let entails scpctx alictx prop : bool = AliasProp.entails (totalAliasProps scpctx alictx) prop

  (** Constructs the aliasing-context of a given formula *)
  let construct clsctx typctx scpctx phi : t =
    (* [ctx] is like the "current context". it is used for referencing [ctx.parent] and [ctx.scope] in the making of new empty
       contexts at the same level as [ctx] (sibling contexts) as well as new child contexts of [ctx]. *)
    let rec helper alictx phi =
      let empty_sibling = { parent    = alictx.parent;
                            scope     = alictx.scope;
                            props     = AliasPropSet.empty;
                            children  = [] }
      in
      let singleton_sibling p = { parent    = alictx.parent;
                                  scope     = alictx.scope;
                                  props     = AliasPropSet.singleton p;
                                  children  = [] }
      in
      let empty_child scp = { parent   = Some alictx.scope;
                              scope    = scp;
                              props    = AliasPropSet.empty;
                              children = [] }
      in
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
                | Or -> inter (helper alictx @@ Expression oper.left) (helper alictx @@ Expression oper.right)
                (* not other operations can yield aliasing propositions *)
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
          (* for each child:
             - create child context
             - add to scoping context
             - fill in children as the child scopes *)
          let child_then =
            let (phi', scp') = ite.then_ in
            (* assume condition is true *)
            let ctx_affirmed = helper (empty_child scp') (Expression ite.condition) in
            (* process phi' *)
            let ctx_affirmed = helper ctx_affirmed phi' in
            (* add child to scoping context *)
            ScopingContext.add scpctx scp' ctx_affirmed;
            (* child is labeled reference to scope *)
            Condition ite.condition, scp'
          in
          let child_else =
            let neg_condition = negateExpression ite.condition in
            let (phi', scp') = ite.else_ in
            (* assume condition is false *)
            let ctx_negated = helper (empty_child scp') (Expression neg_condition) in
            (* process phi' *)
            let ctx_negated = helper ctx_negated phi' in
            (* add child to scoping context *)
            ScopingContext.add scpctx scp' ctx_negated;
            (* child is labeled reference to scope *)
            Condition neg_condition, scp'
          in
          { parent   = alictx.parent;
            props    = AliasPropSet.empty;
            children = [child_then; child_else];
            scope    = alictx.scope }
        | Unfolding_in unfolin ->
          let child =
            let (phi', scp') = unfolin.formula in
            let ctx_unfolded = helper (empty_child scp') phi' in
            ScopingContext.add scpctx scp' ctx_unfolded;
            Unfolding unfolin.predicate_check, scp'
          in
          { parent   = alictx.parent;
            props    = AliasPropSet.empty;
            children = [child];
            scope    = alictx.scope }
      end
    in
    let alictx =
      match phi with
      | Imprecise phi ->
        helper
          { parent    = None;
            scope     = ScopeGenerator.root;
            props     = AliasPropSet.empty;
            children  = [] }
          phi
      | Concrete phi ->
        helper
          { parent    = None;
            scope     = ScopeGenerator.root;
            props     = AliasPropSet.empty;
            children  = [] }
          phi
    in
    ScopingContext.add scpctx ScopeGenerator.root alictx;
    alictx
end
