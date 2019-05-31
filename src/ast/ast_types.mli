(** ast.proto Types *)



(** {2 Types} *)

type id = {
  name : string;
}

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


(** {2 Default values} *)

val default_id : 
  ?name:string ->
  unit ->
  id
(** [default_id ()] is the default value for type [id] *)

val default_type_ : unit -> type_
(** [default_type_ ()] is the default value for type [type_] *)

val default_class_field : 
  ?type_:type_ ->
  ?id:id ->
  unit ->
  class_field
(** [default_class_field ()] is the default value for type [class_field] *)

val default_predicate_argument : 
  ?type_:type_ ->
  ?id:id ->
  unit ->
  predicate_argument
(** [default_predicate_argument ()] is the default value for type [predicate_argument] *)

val default_variable_old : 
  ?id:id ->
  unit ->
  variable_old
(** [default_variable_old ()] is the default value for type [variable_old] *)

val default_variable : unit -> variable
(** [default_variable ()] is the default value for type [variable] *)

val default_expression : unit -> expression
(** [default_expression ()] is the default value for type [expression] *)

val default_formula_concrete_predicate_check : 
  ?predicateid:id ->
  ?arguments:expression list ->
  unit ->
  formula_concrete_predicate_check
(** [default_formula_concrete_predicate_check ()] is the default value for type [formula_concrete_predicate_check] *)

val default_formula_concrete_access_check : 
  ?base:expression ->
  ?fieldid:id ->
  unit ->
  formula_concrete_access_check
(** [default_formula_concrete_access_check ()] is the default value for type [formula_concrete_access_check] *)

val default_formula_concrete : unit -> formula_concrete
(** [default_formula_concrete ()] is the default value for type [formula_concrete] *)

val default_formula_concrete_logical_and : 
  ?andleft:formula_concrete ->
  ?andright:formula_concrete ->
  unit ->
  formula_concrete_logical_and
(** [default_formula_concrete_logical_and ()] is the default value for type [formula_concrete_logical_and] *)

val default_formula_concrete_logical_separate : 
  ?separateleft:formula_concrete ->
  ?separateright:formula_concrete ->
  unit ->
  formula_concrete_logical_separate
(** [default_formula_concrete_logical_separate ()] is the default value for type [formula_concrete_logical_separate] *)

val default_formula_concrete_if_then_else : 
  ?condition:expression ->
  ?thenformula:formula_concrete ->
  ?elseformula:formula_concrete ->
  unit ->
  formula_concrete_if_then_else
(** [default_formula_concrete_if_then_else ()] is the default value for type [formula_concrete_if_then_else] *)

val default_formula_concrete_unfolding_in : 
  ?predicateid:id ->
  ?arguments:expression list ->
  ?formula:formula_concrete ->
  unit ->
  formula_concrete_unfolding_in
(** [default_formula_concrete_unfolding_in ()] is the default value for type [formula_concrete_unfolding_in] *)

val default_formula_imprecise : 
  ?concrete:formula_concrete ->
  unit ->
  formula_imprecise
(** [default_formula_imprecise ()] is the default value for type [formula_imprecise] *)

val default_formula : unit -> formula
(** [default_formula ()] is the default value for type [formula] *)

val default_predicate : 
  ?id:id ->
  ?classid:id ->
  ?arguments:predicate_argument list ->
  ?formula:formula ->
  unit ->
  predicate
(** [default_predicate ()] is the default value for type [predicate] *)

val default_method_argument : 
  ?type_:type_ ->
  ?id:id ->
  unit ->
  method_argument
(** [default_method_argument ()] is the default value for type [method_argument] *)

val default_contract : 
  ?requires:formula ->
  ?ensured:formula ->
  unit ->
  contract
(** [default_contract ()] is the default value for type [contract] *)

val default_statement_declaration : 
  ?type_:type_ ->
  ?id:id ->
  unit ->
  statement_declaration
(** [default_statement_declaration ()] is the default value for type [statement_declaration] *)

val default_statement_assignment : 
  ?id:id ->
  ?value:expression ->
  unit ->
  statement_assignment
(** [default_statement_assignment ()] is the default value for type [statement_assignment] *)

val default_statement_while_loop : 
  ?condition:expression ->
  ?invariant:formula ->
  unit ->
  statement_while_loop
(** [default_statement_while_loop ()] is the default value for type [statement_while_loop] *)

val default_statement_field_assignment : 
  ?baseid:id ->
  ?fieldid:id ->
  ?sourceid:id ->
  unit ->
  statement_field_assignment
(** [default_statement_field_assignment ()] is the default value for type [statement_field_assignment] *)

val default_statement_new_object : 
  ?id:id ->
  ?classid:id ->
  unit ->
  statement_new_object
(** [default_statement_new_object ()] is the default value for type [statement_new_object] *)

val default_statement_method_call : 
  ?targetid:id ->
  ?baseid:id ->
  ?methodid:id ->
  ?classid:id ->
  ?arguments:id list ->
  unit ->
  statement_method_call
(** [default_statement_method_call ()] is the default value for type [statement_method_call] *)

val default_statement_assertion : 
  ?formula:formula ->
  unit ->
  statement_assertion
(** [default_statement_assertion ()] is the default value for type [statement_assertion] *)

val default_statement_release : 
  ?formula:formula ->
  unit ->
  statement_release
(** [default_statement_release ()] is the default value for type [statement_release] *)

val default_statement_fold : 
  ?predicateid:id ->
  ?arguments:expression list ->
  unit ->
  statement_fold
(** [default_statement_fold ()] is the default value for type [statement_fold] *)

val default_statement_unfold : 
  ?predicateid:id ->
  ?arguments:expression list ->
  unit ->
  statement_unfold
(** [default_statement_unfold ()] is the default value for type [statement_unfold] *)

val default_statement : unit -> statement
(** [default_statement ()] is the default value for type [statement] *)

val default_statement_sequence : 
  ?prev:statement ->
  ?next:statement ->
  unit ->
  statement_sequence
(** [default_statement_sequence ()] is the default value for type [statement_sequence] *)

val default_statement_if_then_else : 
  ?condition:expression ->
  ?thenbody:statement ->
  ?elsebody:statement ->
  unit ->
  statement_if_then_else
(** [default_statement_if_then_else ()] is the default value for type [statement_if_then_else] *)

val default_statement_hold : 
  ?formula:formula ->
  ?body:statement ->
  unit ->
  statement_hold
(** [default_statement_hold ()] is the default value for type [statement_hold] *)

val default_method_ : 
  ?type_:id ->
  ?id:id ->
  ?arguments:method_argument list ->
  ?dynamic:contract ->
  ?static:contract ->
  ?body:statement ->
  unit ->
  method_
(** [default_method_ ()] is the default value for type [method_] *)

val default_class_ : 
  ?id:id ->
  ?super:id ->
  ?fields:class_field list ->
  ?predicates:predicate list ->
  ?methods:method_ list ->
  unit ->
  class_
(** [default_class_ ()] is the default value for type [class_] *)

val default_program : 
  ?classes:class_ list ->
  ?statement:statement ->
  unit ->
  program
(** [default_program ()] is the default value for type [program] *)

val default_binary_operation : unit -> binary_operation
(** [default_binary_operation ()] is the default value for type [binary_operation] *)

val default_binary_comparison : unit -> binary_comparison
(** [default_binary_comparison ()] is the default value for type [binary_comparison] *)

val default_number_int : 
  ?int:int32 ->
  unit ->
  number_int
(** [default_number_int ()] is the default value for type [number_int] *)

val default_number : unit -> number
(** [default_number ()] is the default value for type [number] *)

val default_value : unit -> value
(** [default_value ()] is the default value for type [value] *)
