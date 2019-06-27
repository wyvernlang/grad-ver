open Core

open Ast

open Utility
open Functools

(* for all the classes *)
module ClassContext =
struct
  (* [id] is name of class, [class_] is class itself *)
  type t = (id, class_) Hashtbl.t

  let to_string : t -> string = Sexp.to_string @< Hashtbl.sexp_of_t sexp_of_id sexp_of_class_

  let top_ctx : t = String.Table.create ()

  let addClass clsctx cls = Hashtbl.set clsctx ~key:cls.id ~data:cls
  let getClassPredicate : t -> cls:id -> pred:id -> predicate = failwith "TODO"

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

  let to_string : t -> string = Sexp.to_string @< Hashtbl.sexp_of_t sexp_of_id sexp_of_type_

  let create = String.Table.create

  let setIdType typctx id typ = Hashtbl.set typctx ~key:id ~data:typ
  let getIdType typctx id = Hashtbl.find_exn typctx ~key:id
  let getExpressionType typctx (expr:expression) = failwith "TODO"

  let constructExpression : ClassContext.t -> t -> expression -> unit = failwith "TODO"

  let constructArguments typctx (args:argument list) : unit =
    List.iter args ~f:(fun arg -> ignore @@ setIdType typctx arg.id arg.type_)

  let constructPredicateCheck clsctx typctx (predchk:predicate_check) : unit =
    let pred : predicate =
      match predchk.class_ with
      | None -> failwith "TODO: infer predicate class from args"
      | Some clsid -> ClassContext.getClassPredicate clsctx clsid predchk.predicate
    in
    (* matching arguments length *)
    assert (List.length predchk.arguments = List.length pred.arguments);
    (* matching arguments' types *)
    let checkArg ((arg, argdef):expression * argument) : unit =
      constructExpression clsctx typctx arg;
      let arg_typ = getExpressionType typctx arg in
      assert( eqType arg_typ argdef.type_ )
    in
    List.iter (List.zip_exn predchk.arguments pred.arguments) ~f:checkArg

  let rec constructFormula clsctx typctx =
    function
    | Imprecise phi -> constructFormula clsctx typctx @@ Concrete phi
    | Concrete phi ->
      begin
        match phi with
        | Expression expr ->
          constructExpression clsctx typctx expr
        | Predicate_check predchk ->
          constructPredicateCheck clsctx typctx predchk
        | Access_check accchk ->
          constructExpression clsctx typctx (Field_reference{ base=accchk.base; field=accchk.field });
        | Operation oper ->
          constructFormula clsctx typctx (Concrete oper.left);
          constructFormula clsctx typctx (Concrete oper.left)
        | If_then_else ite ->
          constructExpression clsctx typctx ite.condition;
          constructFormula clsctx typctx (Concrete (termOf ite.then_));
          constructFormula clsctx typctx (Concrete (termOf ite.else_))
        | Unfolding_in unfolin ->
          constructPredicateCheck clsctx typctx unfolin.predicate_check;
          constructFormula clsctx typctx (Concrete (termOf unfolin.formula))
      end

  let constructStatement : ClassContext.t -> t -> statement -> unit = failwith "TODO"

end

module TypeCheck =
struct
  let checkStatement clsctx typectx stmt : unit =
    TypeContext.constructStatement clsctx typectx stmt

  let checkFormula clsctx typctx phi : unit =
    TypeContext.constructFormula clsctx typctx phi

  (** type-check the body statement given the arguments' types *)
  let checkPredicate clsctx (pred:predicate) : unit =
    let typctx = TypeContext.create () in
    TypeContext.constructArguments typctx pred.arguments;
    checkFormula clsctx typctx pred.formula

  (** type-check the body statement given the arguments' types *)
  let checkMethod clsctx (meth:method_) : unit =
    let typctx = TypeContext.create () in
    TypeContext.constructArguments typctx meth.arguments;
    checkStatement clsctx typctx meth.body

  (** for each of the class's predicates/methods, type-check the body statement given the arguments' types *)
  let checkClass clsctx cls : unit =
    List.iter cls.predicates ~f:(checkPredicate clsctx);
    List.iter cls.methods    ~f:(checkMethod    clsctx)

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

(* let synthesizeType : TypeContext.t -> expression -> type_ = failwith "TODO" *)
let synthesizeType = failwith "TODO"
