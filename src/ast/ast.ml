open Core
open Sexplib.Std

(*--------------------------------------------------------------------------------------------------------------------------*)
(* types *)

type id = string
[@@deriving sexp]

type scope_id = int
[@@deriving sexp]

type type_ =
  | Int
  | Bool
  | Class of string
  | Top
[@@deriving sexp]

type class_field = {
  type_ : type_;
  id : string;
} [@@deriving sexp]

type argument = {
  type_ : type_;
  id : string;
} [@@deriving sexp]

type variable =
  | Result
  | Id of string
  | Old of string
  | This
[@@deriving sexp]

type value =
  | Int of int32
  | Bool of bool
  | Object of string
  | Null
[@@deriving sexp]

type expression_operator =
  | Add
  | Sub
  | Mul
  | Div
  | And
  | Or
[@@deriving sexp]

type expression_comparer =
  | Neq
  | Eq
  | Lt
  | Gt
  | Le
  | Ge
[@@deriving sexp]

type expression = _expression * scope_id
and _expression =
  | Variable of variable
  | Value of value
  | Operation of expression_operation
  | Comparison of expression_comparison
  | Field_reference of expression_field_reference
[@@deriving sexp]

and expression_operation = {
  operator : expression_operator;
  left : expression;
  right : expression;
} [@@deriving sexp]

and expression_comparison = {
  comparer : expression_comparer;
  left : expression;
  right : expression;
} [@@deriving sexp]

and expression_field_reference = {
  base : expression;
  field : string;
} [@@deriving sexp]

type predicate_check = {
  predicate : string;
  arguments : expression list;
  class_ : string option;
} [@@deriving sexp]

type concrete_access_check = {
  base : expression;
  field : string;
} [@@deriving sexp]

type concrete_operator =
  | And
  | Sep
[@@deriving sexp]

type concrete = _concrete * scope_id
and _concrete =
  | Expression of expression
  | Predicate_check of predicate_check
  | Access_check of concrete_access_check
  | Operation of concrete_operation
  | If_then_else of concrete_if_then_else
  | Unfolding_in of concrete_unfolding_in
[@@deriving sexp]

and concrete_operation = {
  operator : concrete_operator;
  left : concrete;
  right : concrete;
} [@@deriving sexp]

and concrete_if_then_else = {
  condition : expression;
  then_ : concrete;
  else_ : concrete;
} [@@deriving sexp]

and concrete_unfolding_in = {
  predicate_check : predicate_check;
  formula : concrete;
} [@@deriving sexp]

type formula =
  | Concrete of concrete
  | Imprecise of concrete
[@@deriving sexp]

type predicate = {
  id : string;
  arguments : argument list;
  formula : formula;
} [@@deriving sexp]

type contract = {
  requires : formula;
  ensures : formula;
} [@@deriving sexp]

type statement_declaration = {
  type_ : type_;
  id : string;
} [@@deriving sexp]

type statement_assignment = {
  id : string;
  value : expression;
} [@@deriving sexp]

type statement_field_assignment = {
  base : string;
  field : string;
  source : string;
} [@@deriving sexp]

type statement_new_object = {
  id : string;
  class_ : string;
} [@@deriving sexp]

type statement_method_call = {
  target : string;
  base : string;
  method_ : string;
  arguments : string list;
  class_ : string option;
} [@@deriving sexp]

type statement_assertion = {
  concrete : concrete;
} [@@deriving sexp]

type statement_release = {
  concrete : concrete;
} [@@deriving sexp]

type statement_fold = {
  predicate_check : predicate_check;
} [@@deriving sexp]

type statement_unfold = {
  predicate_check : predicate_check;
} [@@deriving sexp]

type statement = _statement * scope_id
and _statement =
  | Skip
  | Sequence of statement_sequence
  | Declaration of statement_declaration
  | Assignment of statement_assignment
  | If_then_else of statement_if_then_else
  | While_loop of statement_while_loop
  | Field_assignment of statement_field_assignment
  | New_object of statement_new_object
  | Method_call of statement_method_call
  | Assertion of statement_assertion
  | Release of statement_release
  | Hold of statement_hold
  | Fold of statement_fold
  | Unfold of statement_unfold
[@@deriving sexp]

and statement_sequence = {
  statements : statement list;
} [@@deriving sexp]

and statement_if_then_else = {
  condition : expression;
  then_ : statement;
  else_ : statement;
} [@@deriving sexp]

and statement_while_loop = {
  condition : expression;
  invariant : formula;
  body : statement;
} [@@deriving sexp]

and statement_hold = {
  formula : formula;
  body : statement;
} [@@deriving sexp]

type method_ = {
  type_ : type_;
  id : string;
  arguments : argument list;
  dynamic : contract;
  static : contract;
  body : statement;
} [@@deriving sexp]

type class_ = {
  id : string;
  super : string;
  fields : class_field list;
  predicates : predicate list;
  methods : method_ list;
} [@@deriving sexp]

type program = {
  classes : class_ list;
  statement : statement;
} [@@deriving sexp]

(*--------------------------------------------------------------------------------------------------------------------------*)
(* exceptions *)

exception Unexpected_nonid_value of value
exception Unexpected_nonid_expression of expression

(*--------------------------------------------------------------------------------------------------------------------------*)
(* wrap parsed  syntax tree  *)

let wrapAST : Ast_types.program -> program =
  fun _ -> failwith "unimplemented"

(*--------------------------------------------------------------------------------------------------------------------------*)
(* equalities  *)

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
  function (expr, scope) as expression ->
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
  | _ -> raise @@ Unexpected_nonid_expression expression
