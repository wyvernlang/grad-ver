(*--------------------------------------------------------------------------------------------------------------------------*)
(* types *)

type id = string

type 'a in_scope = 'a * scope_id
and scope_id = int

type type_ =
  | Int
  | Bool
  | Class of string
  | Top

type class_field = {
  type_ : type_;
  id : string;
}

type argument = {
  type_ : type_;
  id : string;
}

type variable =
  | Result
  | Id of string
  | Old of string
  | This

type value =
  | Int of int32
  | Bool of bool
  | Object of string
  | Null

type expression_operator =
  | Add
  | Sub
  | Mul
  | Div
  | And
  | Or

type expression_comparer =
  | Neq
  | Eq
  | Lt
  | Gt
  | Le
  | Ge

type expression = _expression in_scope
and _expression =
  | Variable of variable
  | Value of value
  | Operation of expression_operation
  | Comparison of expression_comparison
  | Field_reference of expression_field_reference

and expression_operation = {
  operator : expression_operator;
  left : expression;
  right : expression;
}

and expression_comparison = {
  comparer : expression_comparer;
  left : expression;
  right : expression;
}

and expression_field_reference = {
  base : expression;
  field : string;
}

type predicate_check = {
  predicate : string;
  arguments : expression list;
  class_ : string option;
}

type concrete_access_check = {
  base : expression;
  field : string;
}

type concrete_operator =
  | And
  | Sep

type concrete = _concrete in_scope
and _concrete =
  | Expression of expression
  | Predicate_check of predicate_check
  | Access_check of concrete_access_check
  | Operation of concrete_operation
  | If_then_else of concrete_if_then_else
  | Unfolding_in of concrete_unfolding_in

and concrete_operation = {
  operator : concrete_operator;
  left : concrete;
  right : concrete;
}

and concrete_if_then_else = {
  condition : expression;
  then_ : concrete;
  else_ : concrete;
}

and concrete_unfolding_in = {
  predicate_check : predicate_check;
  formula : concrete;
}

type formula =
  | Concrete of concrete
  | Imprecise of concrete

type predicate = {
  id : string;
  arguments : argument list;
  formula : formula;
}

type contract = {
  requires : formula;
  ensures : formula;
}

type statement_declaration = {
  type_ : type_;
  id : string;
}

type statement_assignment = {
  id : string;
  value : expression;
}

type statement_field_assignment = {
  base : string;
  field : string;
  source : string;
}

type statement_new_object = {
  id : string;
  class_ : string;
}

type statement_method_call = {
  target : string;
  base : string;
  method_ : string;
  arguments : string list;
  class_ : string option;
}

type statement_assertion = {
  concrete : concrete;
}

type statement_release = {
  concrete : concrete;
}

type statement_fold = {
  predicate_check : predicate_check;
}

type statement_unfold = {
  predicate_check : predicate_check;
}

type statement = _statement in_scope
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

and statement_sequence = {
  statements : statement list;
}

and statement_if_then_else = {
  condition : expression;
  then_ : statement;
  else_ : statement;
}

and statement_while_loop = {
  condition : expression;
  invariant : formula;
  body : statement;
}

and statement_hold = {
  formula : formula;
  body : statement;
}

type method_ = {
  type_ : type_;
  id : string;
  arguments : argument list;
  dynamic : contract;
  static : contract;
  body : statement;
}

type class_ = {
  id : string;
  super : string;
  fields : class_field list;
  predicates : predicate list;
  methods : method_ list;
}

type program = {
  classes : class_ list;
  statement : statement;
}

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
