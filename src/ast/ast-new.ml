(* ast.ml *)

module PB = Ast_pb

type id = {
  name : string;
}

let matchIdent = String.equal
let identifier s = s
let name s = s
module IdentMap = Core.String.Map

let id : string -> id =
  fun s -> { name=s }

let idEq : id -> id -> bool =
  fun id id' -> id.name = id'.name

let name : id -> string =
  fun id -> id.name

module IdMap : Core.Map.S = Core.String.Map

type type_ =
  | Class of id
  | Int
  | Top

type class_field = {
  type_ : type_;
  id : id;
}

type predicate_argument = {
  type_ : type_;
  id : id;
}

type variable_old = {
  id : id;
}

type variable =
  | Result
  | Id of id
  | Old of variable_old
  | Thisvariable

type expression =
  | Variable of variable
  | Value
  | Binop
  | Binarycomparison
  | Fieldreference

type formula_concrete_predicate_check = {
  predicateid : id;
  arguments : expression list;
}

type formula_concrete_access_check = {
  base : expression;
  fieldid : id;
}

type formula_concrete =
  | Expression of expression
  | Predicatecheck of formula_concrete_predicate_check
  | Accesscheck of formula_concrete_access_check
  | Logicaland of formula_concrete_logical_and
  | Logicalseparate of formula_concrete_logical_separate
  | Ifthenelse of formula_concrete_if_then_else
  | Unfoldingin of formula_concrete_unfolding_in

and formula_concrete_logical_and = {
  andleft : formula_concrete;
  andright : formula_concrete;
}

and formula_concrete_logical_separate = {
  separateleft : formula_concrete;
  separateright : formula_concrete;
}

and formula_concrete_if_then_else = {
  condition : expression;
  thenformula : formula_concrete;
  elseformula : formula_concrete;
}

and formula_concrete_unfolding_in = {
  predicateid : id;
  arguments : expression list;
  formula : formula_concrete;
}

type formula_imprecise = {
  concrete : formula_concrete;
}

type formula =
  | Concrete of formula_concrete
  | Imprecise of formula_imprecise

type predicate = {
  id : id;
  classid : id;
  arguments : predicate_argument list;
  formula : formula;
}

type method_argument = {
  type_ : type_;
  id : id;
}

type contract = {
  requires : formula;
  ensured : formula;
}

type statement_declaration = {
  type_ : type_;
  id : id;
}

type statement_assignment = {
  id : id;
  value : expression;
}

type statement_while_loop = {
  condition : expression;
  invariant : formula;
}

type statement_field_assignment = {
  baseid : id;
  fieldid : id;
  sourceid : id;
}

type statement_new_object = {
  id : id;
  classid : id;
}

type statement_method_call = {
  targetid : id;
  baseid : id;
  methodid : id;
  classid : id;
  arguments : id list;
}

type statement_assertion = {
  formula : formula;
}

type statement_release = {
  formula : formula;
}

type statement_fold = {
  predicateid : id;
  arguments : expression list;
}

type statement_unfold = {
  predicateid : id;
  arguments : expression list;
}

type statement =
  | Skip
  | Sequence of statement_sequence
  | Declaration of statement_declaration
  | Assignmnet of statement_assignment
  | Ifthenelse of statement_if_then_else
  | Whileloop of statement_while_loop
  | Fieldassignment of statement_field_assignment
  | Newobject of statement_new_object
  | Methodcall of statement_method_call
  | Assertion of statement_assertion
  | Release of statement_release
  | Hold of statement_hold
  | Fold of statement_fold
  | Unfold of statement_unfold

and statement_sequence = {
  prev : statement;
  next : statement;
}

and statement_if_then_else = {
  condition : expression;
  thenbody : statement;
  elsebody : statement;
}

and statement_hold = {
  formula : formula;
  body : statement;
}

type method_ = {
  type_ : id;
  id : id;
  arguments : method_argument list;
  dynamic : contract;
  static : contract;
  body : statement;
}

type class_ = {
  id : id;
  super : id;
  fields : class_field list;
  predicates : predicate list;
  methods : method_ list;
}

type program = {
  classes : class_ list;
  statement : statement;
}

type binary_operation =
  | Add
  | Sub
  | Mul
  | Div

type binary_comparison =
  | Neq
  | Eq
  | Lt
  | Gt
  | Le
  | Ge

type number_int = {
  int : int32;
}

type number =
  | Int of number_int

type value =
  | Number of number
  | Objectid of id
  | Null
  | Truevalue
  | Falsevalue

(* pretty printing *)

let pp_binary_operation : binary_operation -> string =
  function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"

let pp_binary_comparison : binary_comparison -> string =
  function
  | Neq -> "<>"
  | Eq  -> "=="
  | Lt  -> "<"
  | Gt  -> ">"
  | Le  -> "<="
  | Ge  -> ">="

let pp_expression : expression -> string

(* val pp_statement : statement -> string *)

(* val pp_formula : formula -> string *)
