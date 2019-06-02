open Core
open Functools
open Ast_types
open Utility

exception Not_wellformed

(****************************************************************************************************************************)
(* contexts *)

(* context of bound names *)
let class_context    = String.Table.create ()
let variable_context = String.Table.create ()
;;

exception Class_not_found of id

let getClass : id -> class_ =
  fun id ->
  let result = Hashtbl.find class_context id.string in
  match result with
  | Some cls -> cls
  | None     -> raise (Class_not_found id)

exception Field_not_found of class_ * id

let getClassField : id -> class_ -> class_field =
  fun id cls ->
  let test (field:class_field) = field.id = id in
  let result = List.find cls.fields ~f:test in
  match result with
  | Some field -> field
  | None       -> raise (Field_not_found (cls, id))

(****************************************************************************************************************************)
(* types *)

(* type synthesis *)
let rec synthesizeType : expression -> type_ =
  fun e ->
  match e with
  | Variable x ->
    begin
      match x with
      | Result -> unimplemented ()
      | Id id -> unimplemented ()
      | Old id -> unimplemented ()
      | This -> unimplemented ()
    end
  | Value v ->
    begin
      match v with
      | Int x -> unimplemented ()
      | Objectid id -> unimplemented ()
      | Null -> unimplemented ()
      | True -> unimplemented ()
      | False -> unimplemented ()
    end
  | Binaryoperation biop ->
    begin
      let leftType  = synthesizeType biop.binaryoperationleft in
      let rightType = synthesizeType biop.binaryoperationright in
      match biop.binaryoperator with
      | Add | Sub | Mul | Div ->
        if (leftType = rightType) && (leftType = Int) then
          Int
        else
          raise Not_wellformed
    end
  | Binarycomparison bico ->
    begin
      let leftType  = synthesizeType bico.binarycomparisonleft in
      let rightType = synthesizeType bico.binarycomparisonright in
      if leftType = rightType then
        Top
      else
        raise Not_wellformed
    end
  | Fieldreference fieldref ->
    begin
      let baseType = synthesizeType fieldref.base in
      match baseType with
      | Class typecls ->
        let cls      = getClass typecls.classid in
        let clsfield = getClassField fieldref.fieldid cls in
        clsfield.type_
      | _ -> raise Not_wellformed
    end

(****************************************************************************************************************************)
(* formulas *)

(* TODO *)
(* val checkFormula : formula -> unit *)

(****************************************************************************************************************************)
(* statements *)

(* TODO *)
(* val processStatement  : unit -> statement -> unit *)
(* val processStatements : statement list ->  unit *)

(****************************************************************************************************************************)
(* program *)

(* TODO *)
(* val initProgram : program -> unit *)
