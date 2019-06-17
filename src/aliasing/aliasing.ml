open Core
open Ast
open Wellformed

(* TODO: implement sexp... *)
(* open Sexplib.Std *)

(* ------------------------------------------------------------------------------------------------------------------------ *)
(* definitions *)
(* ------------------------------------------------------------------------------------------------------------------------ *)

type object_variable =
  | OV_Value            of value
  | OV_Variable         of variable
  | OV_Field_reference of expression_field_reference
  | OV_Null

let extract_object_variable : expression -> object_variable option =
  fun (expr, scope) ->
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

type aliased_prop = object_variable list

type aliasing_context = {
  aliased_props : aliased_prop list;
  children      : (aliasing_context_label * aliasing_context) list;
  scope_id      : scope_id
}

and aliasing_context_label =
  | ACL_Condition of expression
  | ACL_Unfolding of predicate_check

(* ----------------------------------------------------------------------------------------------------------------------- *)
(* entailment from aliasing context *)
(* ----------------------------------------------------------------------------------------------------------------------- *)

(* find any element of ctx that is a superset of prop *)
let entailsAliased ctx prop =
  failwith "todo"

(* ------------------------------------------------------------------------------------------------------------------------ *)
(* utilities *)
(* ------------------------------------------------------------------------------------------------------------------------ *)

let empty_context scope_id : aliasing_context = { aliased_props=[]; children=[]; scope_id=scope_id }

let contextUnion : aliasing_context -> aliasing_context -> aliasing_context =
  fun ac1 ac2 ->
  failwith "TODO"
let (+++) = contextUnion

let contextIntersection : aliasing_context -> aliasing_context -> aliasing_context =
  fun ac1 ac2 ->
  failwith "TODO"
let (&&&) = contextIntersection

let negate : expression -> expression =
  fun expr ->
  failwith "TODO"

(* ----------------------------------------------------------------------------------------------------------------------- *)
(* constructing aliasing context *)
(* ----------------------------------------------------------------------------------------------------------------------- *)

let rec constructAliasingContext : formula -> aliasing_context =
  function
  | Imprecise _ -> failwith "[!] unimplented: construct_aliasing_context of imprecise formulas"
  | Concrete phi -> helper phi

and helper : concrete -> aliasing_context =
  fun (concrete, scope_id) ->
  match concrete with
  | Expression (expr, scope_id) -> begin
      match expr with
      | Variable var ->
        empty_context scope_id
      | Value vlu ->
        empty_context scope_id
      | Operation oper ->
        begin
          match oper.operator with
          | And -> (helper @@ (Expression oper.left, scope_id)) +++ (helper @@ (Expression oper.right, scope_id))
          | Or  -> helper @@ (If_then_else
                                { condition=oper.left;
                                  (* its fine for this scope never to be considered because
                                     it can only ever just `true` as its contents *)
                                  then_=(Expression (Value (Bool true), scope_id), scope_id);
                                  else_=(Expression oper.right, scope_id) },
                              scope_id)
          | _ -> empty_context scope_id
        end
      | Comparison comp ->
        begin
          match comp.comparer with
          | Eq ->
            begin
              match extract_object_variable comp.left, extract_object_variable comp.right with
              | (Some ov1, Some ov2) ->
                { aliased_props = [ [ov1;ov2] ];
                  children = [];
                  scope_id = scope_id }
              | _ -> empty_context scope_id
            end
          | _ -> empty_context scope_id
        end
      | Field_reference fldref ->
        empty_context scope_id
  end
  | Predicate_check predchk ->
    empty_context scope_id
  | Access_check accchk ->
    empty_context scope_id
  | Operation oper ->
    (helper oper.left) +++ (helper oper.right)
  | If_then_else ite ->
    let children =
      [ ACL_Condition ite.condition, helper ite.then_;
        ACL_Condition (negate ite.condition), helper ite.else_ ] in
    { aliased_props = [];
      children      = children;
      scope_id      = scope_id }
  | Unfolding_in unfolin ->
    let children = [ ACL_Unfolding unfolin.predicate_check, helper unfolin.formula ] in
    { aliased_props = [];
      children      = children;
      scope_id      = scope_id }
