open Ast_types

(* {0 Abstract Syntax Tree Wrapper} *)

(****************************************************************************************************************************)
(** {1 Types} *)

type id = string

(** {1 Exceptions} *)

exception Unexpected_nonid_value of value
exception Unexpected_nonid_expression of expression

(****************************************************************************************************************************)
(** {1 Equalities}  *)

let eqId : id -> id -> bool = (=)

let eqType : type_  -> type_  -> bool =
  fun typ typ' ->
  match (typ, typ') with
  | Int      , Int       -> true
  | Bool     , Bool      -> true
  | Class id , Class id' -> eqId id id'
  | Top      , Top       -> true
  | _ -> false

let eqClass : class_ -> class_ -> bool = fun cls cls' -> eqId cls.id cls'.id

let getExpressionId : expression -> id =
  fun expr ->
  match expr with
  | Variable var ->
    begin
      match var with
      | Result  -> "result"
      | Id id   -> id
      | Old id  -> id
      | This    -> "this"
    end
  | Value vlu ->
    begin
      match vlu with
      | Object id -> id
      | _ -> raise @@ Unexpected_nonid_value vlu
    end
  | _ -> raise @@ Unexpected_nonid_expression expr
