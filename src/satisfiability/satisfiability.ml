open Core
open Core.Option
open Z3
open Z3tools
open Utility
open Functools

open Ast
open Wellformed
open Aliasing
open Framing

(*--------------------------------------------------------------------------------------------------------------------------*)
(* satisfiability SatisfiabilityContext.t *)
(*--------------------------------------------------------------------------------------------------------------------------*)

module SatisfiabilityContext = struct
  type t = {
    clsctx : ClassContext.t;
    typctx : TypeContext.t;
    scpctx : ScopingContext.t;
    z3ctx : Z3Context.t;
    z3exs : Expr.expr list;
    acc_z3exs : Expr.expr list; (* these are kept separate so they may be removed between '^'s *)
  }

  (* accessors *)

  let getZ3Exs satctx : Expr.expr list = satctx.z3exs @ satctx.acc_z3exs

  (* mutators *)

  let addZ3Ex         satctx z3ex     : t = { satctx with z3exs=z3ex::satctx.z3exs }
  let addAccZ3Ex      satctx acc_z3ex : t = { satctx with acc_z3exs=acc_z3ex::satctx.acc_z3exs }
  let removeAccZ3Exs  satctx          : t = { satctx with acc_z3exs=[] }

  (* satisfiability *)

  let isSatisfiable satctx : bool =
    Z3Context.isSatisfiableWith satctx.z3ctx @@ getZ3Exs satctx

  let isSatisfiableWith satctx z3ex : bool =
    Z3Context.isSatisfiableWith satctx.z3ctx @@ z3ex::getZ3Exs satctx

  let checkSatisfiability satctx : t option =
    if Z3Context.isUnsatisfiableWith satctx.z3ctx @@ getZ3Exs satctx
    then Some satctx
    else None

  let addZ3ExIfSatisfiable satctx z3ex : t option =
    if isSatisfiableWith satctx z3ex
    then some @@ addZ3Ex satctx z3ex
    else None
end

(*--------------------------------------------------------------------------------------------------------------------------*)
(* z3exs *)
(*--------------------------------------------------------------------------------------------------------------------------*)

(* of expression *)

let rec z3ex_of_expression (satctx:SatisfiabilityContext.t) (expr:expression) : Expr.expr =
  match expr with
  | Variable var -> Z3Context.makeBoolConst satctx.z3ctx @@ getExpressionId (Variable var)
  | Value vlu ->
    begin
      match vlu with
      | Int    i  -> Z3Context.makeIntVal      satctx.z3ctx i
      | Bool   b  -> Z3Context.makeBoolVal     satctx.z3ctx b
      | Object id -> Z3Context.makeObjectConst satctx.z3ctx id
    end
  | Operation oper ->
    let left = z3ex_of_expression satctx oper.left in
    let right = z3ex_of_expression satctx oper.right in
    begin
      match oper.operator with
      | Add -> Z3Context.makeAdd satctx.z3ctx left right
      | Sub -> Z3Context.makeSub satctx.z3ctx left right
      | Mul -> Z3Context.makeMul satctx.z3ctx left right
      | Div -> Z3Context.makeDiv satctx.z3ctx left right
      | And -> Z3Context.makeAnd satctx.z3ctx left right
      | Or  -> Z3Context.makeOr  satctx.z3ctx left right
    end
  | Comparison comp ->
    let left = z3ex_of_expression satctx comp.left in
    let right = z3ex_of_expression satctx comp.right in
    begin
      match comp.comparer with
      | Neq -> Z3Context.makeNeq satctx.z3ctx left right
      | Eq  -> Z3Context.makeEq  satctx.z3ctx left right
      | Lt  -> Z3Context.makeLt  satctx.z3ctx left right
      | Gt  -> Z3Context.makeGt  satctx.z3ctx left right
      | Le  -> Z3Context.makeLe  satctx.z3ctx left right
      | Ge  -> Z3Context.makeGe  satctx.z3ctx left right
    end
  | Field_reference fldref ->
    let base_z3ex = z3ex_of_expression satctx fldref.base in
    let fld_typ = TypeContext.getExpressionType satctx.clsctx satctx.typctx (Field_reference fldref) in
    Z3Context.makeFieldConst satctx.z3ctx base_z3ex fldref.field fld_typ

(* process formula *)

let rec processConcrete satctx phi : SatisfiabilityContext.t option =
  match phi with

  | Expression expr ->
    SatisfiabilityContext.addZ3ExIfSatisfiable satctx @@ z3ex_of_expression satctx expr

  | Predicate_check predchk ->
    let pred = TypeContext.inferClassPredicate satctx.clsctx satctx.typctx predchk in
    let pred_fndl = Z3Context.makePredicateFuncDecl satctx.z3ctx pred in
    let arg_z3exs = List.map predchk.arguments ~f:(z3ex_of_expression satctx) in
    let predchk_z3ex = Z3Context.makePredicateCheck satctx.z3ctx pred_fndl arg_z3exs in
    SatisfiabilityContext.addZ3ExIfSatisfiable satctx predchk_z3ex

  | Access_check accchk ->
    let base_z3ex = z3ex_of_expression satctx accchk.base in
    let accchk_z3ex = Z3Context.makeAccessCheck satctx.z3ctx base_z3ex accchk.field in
    let neg_accchk_z3ex = Z3Context.makeNot satctx.z3ctx accchk_z3ex in
    if SatisfiabilityContext.isSatisfiableWith satctx neg_accchk_z3ex (* if ~ acc(x.f) is satisfiable: *)
    then SatisfiabilityContext.addZ3ExIfSatisfiable satctx accchk_z3ex  (* then assert acc(x.f); *)
    else None (* otherwise acc(x.f) has already been asserted. *)

  | Operation oper ->
    begin
      match oper.operator with
      | Sep -> processConcrete satctx oper.left                      >>= fun satctx -> processConcrete satctx oper.right
      | And -> processConcrete satctx oper.left >>| SatisfiabilityContext.removeAccZ3Exs >>= fun satctx -> processConcrete satctx oper.right
    end

  | If_then_else ite ->
    let cond_z3ex     = z3ex_of_expression satctx ite.condition in
    let neg_cond_z3ex = Z3Context.makeNot satctx.z3ctx cond_z3ex in
    (* at lxeast one branch must be satisfiable *)
    if isSatisfiableConcrete (SatisfiabilityContext.addZ3Ex satctx cond_z3ex)     @@ termOf ite.then_ ||
       isSatisfiableConcrete (SatisfiabilityContext.addZ3Ex satctx neg_cond_z3ex) @@ termOf ite.else_
    then Some satctx
    else None
  | Unfolding_in unfolin ->
    processConcrete satctx (termOf unfolin.formula)

(* satisfiable formula *)

and isSatisfiableConcrete satctx phi : bool =
  match processConcrete satctx phi with
  | Some satctx -> SatisfiabilityContext.isSatisfiable satctx
  | None     -> false

(** Checks whether the given formula is satifiable.
    The ClassContext is of the enclosing program.
    The TypeContext is of the enclosing statement.
    If the formula appears in a method's contract, the type SatisfiabilityContext.t includes the method's arguments.
    The ScopingContext is of the formula. *)
let isSatisfiable clsctx typctx scpctx frm : bool =
  let satctx : SatisfiabilityContext.t = { clsctx=clsctx; typctx=typctx; scpctx=scpctx;
                                z3ctx=Z3Context.create (); z3exs=[]; acc_z3exs=[] } in
  match frm with
  | Imprecise phi -> failwith "TODO: isSatisfiable of Imprecise"
  | Concrete  phi -> isSatisfiableConcrete satctx phi
