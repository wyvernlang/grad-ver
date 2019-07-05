open Core

open Ast

open Utility
open Functools

exception Unfound_id of id
exception Unfound_predicate of id * id
exception Type_mismatch_comparison of type_ * type_
exception Type_mismatch_operation of type_ * type_
exception Type_mismatch_argument of type_ * type_
exception Type_mismatch_assignment of id * type_ * type_
exception Type_mismatch_field_assignment of type_ * type_
exception Type_mismatch_new_object of type_ * type_
exception Type_mismatch_method_call of type_ * type_
exception Unfound_field of expression_field_reference
exception Nonobject_base of expression_field_reference
exception Nonobject_field_assignment_base of statement_field_assignment
exception Undefined_class of id
exception Undefined_field of id * id
exception Undefined_predicate of id * id
exception Undefined_method of id * id
exception Nonbool_if_condition of expression
exception Nonbool_while_condition of expression
exception Argument_length_mismatch_method_call of statement_method_call

let assertEq     e a a' : unit = if a = a' then () else raise e
let assertSome   e o    : unit = match o with Some _ -> () | None -> raise e
let assertEqType e t t' : unit = if eqType t t' then () else raise e

(* for all the classes *)
module ClassContext =
struct
  (* [id] is name of class, [class_] is class itself *)
  type t = (id, class_) Hashtbl.t

  let create : unit -> t = String.Table.create
  let copy : t -> t = Hashtbl.copy
  let sexp_of_t : t -> Sexp.t = Hashtbl.sexp_of_t sexp_of_id sexp_of_class_
  let to_string : t -> string = Sexp.to_string @< sexp_of_t

  let equal clsctx clsctx' : bool = Hashtbl.equal clsctx clsctx' eqClass

  let top_ctx : t = String.Table.create ()

  let addClass clsctx cls = Hashtbl.set clsctx ~key:cls.id ~data:cls

  let getClass clsctx clsid : class_ =
    match Hashtbl.find clsctx clsid with
    | Some cls -> cls
    | None -> raise @@ Undefined_class clsid

  let getClassFieldType clsctx clsid fldid : type_ =
    let cls = getClass clsctx clsid in
    let fld_opt = List.find cls.fields ~f:(fun fld' -> eqId fldid fld'.id) in
    match fld_opt with
    | Some fld -> fld.type_
    | None -> raise @@ Undefined_field (clsid, fldid)

  let getClassPredicate clsctx clsid predid : predicate =
    let cls = getClass clsctx clsid in
    let pred_opt = List.find cls.predicates ~f:(fun pred' -> eqId predid pred'.id) in
    match pred_opt with
    | Some pred -> pred
    | None -> raise @@ Undefined_predicate (clsid, predid)

  let getClassMethod clsctx clsid methid : method_ =
    let cls = getClass clsctx clsid in
    let meth_opt = List.find cls.methods ~f:(fun meth' -> eqId methid meth'.id) in
    match meth_opt with
    | Some meth -> meth
    | None -> raise @@ Undefined_method (clsid, methid)

  let constructClass ctx cls : unit =
    addClass ctx cls

  (* for each class, register the class which records its methods/predicates and argument types *)
  let construct (prgm:program) : t =
    List.iter prgm.classes ~f:(constructClass top_ctx);
    top_ctx

end

(* for each statement scope *)
module TypeContext =
struct
  type t = (id, type_) Hashtbl.t

  let sexp_of_t : t -> Sexp.t = Hashtbl.sexp_of_t sexp_of_id sexp_of_type_
  let to_string : t -> string = Sexp.to_string @< sexp_of_t

  let create : unit -> t = String.Table.create
  let copy : t -> t = Hashtbl.copy
  let equal typctx typctx' : bool = Hashtbl.equal typctx typctx' eqType

  let setIdType typctx id typ : unit = Hashtbl.set typctx ~key:id ~data:typ
  let getIdType typctx id : type_ =
    if id = null_id then null_type else
    match Hashtbl.find typctx id with
    | Some typ -> typ
    | None -> raise @@ Unfound_id id

  (** Derives the type of an expression within the given TypeContext.t, assuming that the expression is well-formed *)
  let rec getExpressionType clsctx typctx : expression -> type_ =
    function
    | Variable var ->
      begin
        match var with
        | Result  -> getIdType typctx "result"
        | Id id   -> getIdType typctx id
        | Old id  -> getIdType typctx id
        | This    -> getIdType typctx "this"
      end
    | Value vlu ->
      begin
        match vlu with
        | Int  _    -> Int
        | Bool _    -> Bool
        | Object id -> getIdType typctx id
      end
    | Operation oper ->
      begin
        match oper.operator with
        | Add | Sub | Mul | Div -> Int
        | And | Or -> Bool
      end
    | Comparison comp ->
      Bool
    | Field_reference fldref ->
      let base_typ = getExpressionType clsctx typctx fldref.base in
      let base_cls =
        begin
          match base_typ with
          | Class clsid -> ClassContext.getClass clsctx clsid
          | _ -> raise @@ Nonobject_base fldref
        end in
      ClassContext.getClassFieldType clsctx base_cls.id fldref.field

  (** A [construct_] function takes an input TypeContext.t and usually adds type declarations to it *)

  let constructArguments typctx (args:argument list) : unit =
    List.iter args ~f:(fun arg -> ignore @@ setIdType typctx arg.id arg.type_)

  let rec constructStatement clsctx typctx : statement -> unit =
    function
    | Skip ->
      () (* doesn't add any declaration to type context *)
    | Sequence seq ->
      List.iter seq.statements ~f:(constructStatement clsctx typctx)
    | Declaration decl ->
      debug ~hide:true @@ "construct(declaration): "^
                           Sexp.to_string (sexp_of_id decl.id)^" "^
                           Sexp.to_string (sexp_of_type_ decl.type_);
      setIdType typctx decl.id decl.type_
    | Assignment asg ->
      () (* doesn't add any declaration to type context *)
    | If_then_else ite ->
      () (* construction of branches will be invoked during type-checking *)
    | While_loop wl ->
      () (* construction of body will be invoked during type-checking *)
    | Field_assignment fldasg ->
      () (* doesn't add any declaration to type context *)
    | New_object newobj ->
      () (* doesn't add any declaration to type context *)
    | Method_call methcall ->
      () (* doesn't add any declaration to type context *)
    | Assertion ass ->
      () (* doesn't add any declaration to type context *)
    | Release rel ->
      () (* doesn't add any declaration to type context *)
    | Hold hld ->
      () (* doesn't add any declaration to type context *)
    | Fold fld ->
      () (* doesn't add any declaration to type context *)
    | Unfold unfld ->
      () (* doesn't add any declaration to type context *)
end

module TypeCheck =
struct
  type class_context = (id, class_) Hashtbl.t
  type type_context = (id, type_) Hashtbl.t

  let rec checkExpression clsctx typctx : expression -> unit =
    function
    | Variable var ->
      () (* trivially type-checks *)
    | Value vlu ->
      () (* trivially type-checks *)
    | Operation oper ->
      (* check and get types of left and right *)
      checkExpression clsctx typctx oper.left;
      let left_typ  = TypeContext.getExpressionType clsctx typctx oper.left  in
      checkExpression clsctx typctx oper.right;
      let right_typ = TypeContext.getExpressionType clsctx typctx oper.right in
      (* check that operator is used correctly *)
      begin
        match oper.operator with
        | Add | Mul | Sub | Div ->
          assertEqType (Type_mismatch_operation (left_typ, Int)) left_typ Int;
          assertEqType (Type_mismatch_operation (right_typ, Int)) right_typ Int
        | And | Or ->
          assertEqType (Type_mismatch_operation (left_typ, right_typ)) left_typ right_typ;
      end
    | Comparison comp ->
      (* check and get types of [left] and [right] *)
      checkExpression clsctx typctx comp.left;
      let left_typ  = TypeContext.getExpressionType clsctx typctx comp.left  in
      checkExpression clsctx typctx comp.right;
      let right_typ = TypeContext.getExpressionType clsctx typctx comp.right in
      (* check that [comparer] is used correctly *)
      begin
        match comp.comparer with
        | Neq | Eq ->
          assertEqType (Type_mismatch_comparison (left_typ, right_typ)) left_typ right_typ
        | Lt | Gt | Le | Ge ->
          assertEqType (Type_mismatch_comparison (left_typ, Int)) left_typ Int;
          assertEqType (Type_mismatch_comparison (right_typ, Int)) right_typ Int
      end
    | Field_reference fldref ->
      (* check [base] *)
      checkExpression clsctx typctx fldref.base;
      let base_typ = TypeContext.getExpressionType clsctx typctx fldref.base in
      (* make sure that [field] is indeed a field of the [base] class *)
      match base_typ with
      | Class clsid -> ignore @@ ClassContext.getClassFieldType clsctx clsid fldref.field
      | _ -> raise @@ Nonobject_base fldref

  let checkPredicateCheck clsctx typctx (predchk:predicate_check) : unit =
    let pred : predicate =
      match predchk.class_ with
      | None -> failwith "UNIMPL: infer predicate class from args"
      | Some clsid -> ClassContext.getClassPredicate clsctx clsid predchk.predicate
    in
    (* matching arguments length *)
    assert( List.length predchk.arguments = List.length pred.arguments );
    (* matching arguments' types *)
    let checkArg ((arg, argdef):expression * argument) : unit =
      checkExpression clsctx typctx arg;
      let arg_typ = TypeContext.getExpressionType clsctx typctx arg in
      assertEqType (Type_mismatch_argument (arg_typ, argdef.type_)) arg_typ argdef.type_
    in
    List.iter (List.zip_exn predchk.arguments pred.arguments) ~f:checkArg

  let rec checkFormula clsctx typctx =
    function
    | Imprecise phi ->
      checkFormula clsctx typctx @@ Concrete phi
    | Concrete phi ->
      begin
        match phi with
        | Expression expr ->
          checkExpression clsctx typctx expr
        | Predicate_check predchk ->
          checkPredicateCheck clsctx typctx predchk
        | Access_check accchk ->
          checkExpression clsctx typctx (Field_reference{ base=accchk.base; field=accchk.field });
        | Operation oper ->
          checkFormula clsctx typctx (Concrete oper.left);
          checkFormula clsctx typctx (Concrete oper.left)
        | If_then_else ite ->
          checkExpression clsctx typctx ite.condition;
          checkFormula clsctx typctx (Concrete (termOf ite.then_));
          checkFormula clsctx typctx (Concrete (termOf ite.else_))
        | Unfolding_in unfolin ->
          checkPredicateCheck clsctx typctx unfolin.predicate_check;
          checkFormula clsctx typctx (Concrete (termOf unfolin.formula))
      end

  let rec checkStatement clsctx typctx stmt : unit =
    (* first, construct *)
    TypeContext.constructStatement clsctx typctx stmt;
    (* then check, given the constructed type context *)
    match stmt with
    | Skip ->
      ()
    | Sequence seq ->
      List.iter seq.statements ~f:(checkStatement clsctx typctx)
    | Declaration decl ->
      ()
    | Assignment asg ->
      let id_typ = TypeContext.getIdType typctx asg.id in
      let expr_typ = TypeContext.getExpressionType clsctx typctx asg.value in
      debug ~hide:true @@ "check(assignment): "^
                           asg.id^" := "^Sexp.to_string(sexp_of_expression asg.value)^" ; "^
                           Sexp.to_string(sexp_of_type_ id_typ)^" === "^
                           Sexp.to_string(sexp_of_type_ expr_typ);
      assertEqType (Type_mismatch_assignment (asg.id, id_typ, expr_typ)) id_typ expr_typ
    | If_then_else ite ->
      (* condition must be bool *)
      let cond_typ = TypeContext.getExpressionType clsctx typctx ite.condition in
      assertEqType (Nonbool_if_condition ite.condition) cond_typ Bool;
      (* check branches *)
      let typctx_then = TypeContext.copy typctx in
      TypeContext.constructStatement clsctx typctx_then ite.then_;
      checkStatement clsctx typctx_then ite.then_;
      let typctx_else = TypeContext.copy typctx in
      TypeContext.constructStatement clsctx typctx_else ite.else_;
      checkStatement clsctx typctx_else ite.else_;
    | While_loop wl ->
      (* condition must be bool *)
      let cond_typ = TypeContext.getExpressionType clsctx typctx wl.condition in
      assertEqType (Nonbool_while_condition wl.condition) cond_typ Bool;
      let typctx_body = TypeContext.copy typctx in
      TypeContext.constructStatement clsctx typctx_body wl.body;
      checkStatement clsctx typctx_body wl.body
    | Field_assignment fldasg ->
      let base_typ = TypeContext.getIdType typctx fldasg.base in
      let base_cls =
        match base_typ with
        | Class clsid -> clsid
        | _ -> raise @@ Nonobject_field_assignment_base fldasg in
      let fld_typ = ClassContext.getClassFieldType clsctx base_cls fldasg.field in
      (* expression must match field's defined type *)
      let expr_typ = TypeContext.getExpressionType clsctx typctx fldasg.source in
      assertEqType (Type_mismatch_field_assignment (expr_typ, fld_typ)) expr_typ fld_typ
    | New_object newobj ->
      (* target type *)
      let typ = TypeContext.getIdType typctx newobj.id in
      (* type must match Class *)
      assertEqType (Type_mismatch_new_object (typ, (Class newobj.class_))) typ (Class newobj.class_)
    | Method_call methcall ->
      let clsid = failwith "UNIMPL: infer method class in method call" in
      (* find method *)
      let meth = ClassContext.getClassMethod clsctx clsid  methcall.method_ in
      (* matching arguments length *)
      assertEq (Argument_length_mismatch_method_call methcall) (List.length methcall.arguments) (List.length meth.arguments);
      (* matching arguments' types *)
      let checkArg (expr, typ) = failwith "TODO" in
      List.iter (List.zip_exn methcall.arguments meth.arguments) ~f:checkArg;
      (* target type *)
      let typ = TypeContext.getIdType typctx methcall.target in
      (* targe type matches method return type *)
      assertEqType (Type_mismatch_method_call (typ, meth.type_)) typ meth.type_
    | Assertion ass ->
      checkFormula clsctx typctx (Concrete ass.concrete)
    | Release rel ->
      checkFormula clsctx typctx (Concrete rel.concrete)
    | Hold hld ->
      checkFormula clsctx typctx hld.formula
    | Fold fld ->
      checkFormula clsctx typctx (Concrete(Predicate_check fld.predicate_check))
    | Unfold unfld ->
      checkFormula clsctx typctx (Concrete(Predicate_check unfld.predicate_check))

  (** type-check the body statement given the arguments' types *)
  let checkPredicate clsctx clsid (pred:predicate) : unit =
    (* new TypeContext.t for predicate *)
    let typctx = TypeContext.create () in
    TypeContext.constructArguments typctx pred.arguments;
    checkFormula clsctx typctx pred.formula

  let checkContract clsctx typctx (ctrt:contract) : unit =
    checkFormula clsctx typctx ctrt.requires;
    checkFormula clsctx typctx ctrt.ensures

  (** type-check the body statement given the arguments' types *)
  let checkMethod clsctx clsid (meth:method_) : unit =
    (* new TypeContext.t for method *)
    let typctx = TypeContext.create () in
    (* arguments *)
    TypeContext.constructArguments typctx meth.arguments;
    (* return type *)
    TypeContext.setIdType typctx "this" (Class clsid);
    (* contracts *)
    checkContract clsctx typctx meth.static;
    checkContract clsctx typctx meth.dynamic;
    (* body *)
    checkStatement clsctx typctx meth.body

  (** for each of the class's predicates/methods, type-check the body statement given the arguments' types *)
  let checkClass clsctx cls : unit =
    List.iter cls.predicates ~f:(checkPredicate clsctx cls.id);
    List.iter cls.methods    ~f:(checkMethod    clsctx cls.id)

  (** To type-check a program:
      - Construct the program's [ClassContext.t], recording the names of classes, the types of fields,
        andthe types of predicates/methods.
      - For each class, for each of the class's predicates/methods, type-check the body statement given the arguments' types.
      - Type-check the main statement *)
  let check (prgm:program) : unit =
    let clsctx = ClassContext.construct prgm in
    let typctx = TypeContext.create () in
    List.iter prgm.classes ~f:(checkClass clsctx);
    checkStatement clsctx typctx prgm.statement
end
