open Core
open Ast_types

(* ------------------------------------------------------------------------------------------------------------------------ *)
(* definitions *)
(* ------------------------------------------------------------------------------------------------------------------------ *)

type object_variable =
  | Variable         of variable
  | Value            of value
  | Field_reference of expression_field_reference
  | Null

let extract_object_variable : expression -> object_variable option =
  fun expr ->
  match Wellformed.synthesizeType expr with
  | Class id ->
    begin
      match expr with
      | Variable var -> Some (Variable var)
      | Value vlu -> Some (Value vlu)
      | Field_reference fldref -> Some (Field_reference fldref)
      | _ -> None (* impossible case, since only the previous can have type Object id *)
    end
  | _ -> None

type aliased = object_variable list

type aliasing_context = Aliasing_context of aliased list * (aliasing_context_label * aliasing_context) list

and aliasing_context_label =
  | Condition of expression
  | Unfolding of predicate_check

(* ----------------------------------------------------------------------------------------------------------------------- *)
(* entailment from aliasing context *)
(* ----------------------------------------------------------------------------------------------------------------------- *)

(* find any element of ctx that is a superset of prop *)
let entailsAliased ctx prop =
  match ctx with Aliasing_context (aliaseds, _) ->


(* ------------------------------------------------------------------------------------------------------------------------ *)
(* utilities *)
(* ------------------------------------------------------------------------------------------------------------------------ *)

let empty_context : aliasing_context = Aliasing_context ([], [])

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

let rec constructAliasingContext =
  function
  | Imprecise _ -> failwith "unimplented: construct_aliasing_context of imprecise formulas"
  | Concrete phi -> helper phi

and helper =
  function
  | Expression expr -> begin
      match expr with
      | Variable var ->
        empty_context
      | Value vlu ->
        empty_context
      | Operation oper ->
        begin
          match oper.operator with
          | And -> (helper @@ Expression oper.left) +++ (helper @@ Expression oper.right)
          | Or  -> helper @@ If_then_else
              { condition=oper.left; then_=(Expression (Value (Bool true))); else_=(Expression oper.right) }
          | _ -> empty_context
        end
      | Comparison comp ->
        begin
          match comp.comparer with
          | Eq ->
            begin
              match extract_object_variable comp.left, extract_object_variable comp.right with
              | (Some ov1, Some ov2) -> Aliasing_context ([ [ov1;ov2] ], [])
              | _ -> empty_context
            end
          | _ -> empty_context
        end
      | Field_reference fldref ->
        empty_context
  end
  | Predicate_check predchk ->
    empty_context
  | Access_check accchk ->
    empty_context
  | Operation oper ->
    (helper oper.left) +++ (helper oper.right)
  | If_then_else ite ->
    let children = [
      Condition ite.condition, helper ite.then_;
      Condition (negate ite.condition), helper ite.else_
    ] in
    Aliasing_context ([], children)
  | Unfolding_in unfolin ->
    let children = [
      Unfolding unfolin.predicate_check, helper unfolin.formula
    ] in
    Aliasing_context ([], children)
