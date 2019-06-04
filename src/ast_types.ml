[@@@ocaml.warning "-27-30-39"]


type type_ =
  | Int of int32
  | Bool of bool
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
  | Object of string
  | Null
  | True
  | False

type binary_operator =
  | Add 
  | Sub 
  | Mul 
  | Div 
  | And 
  | Or 

type binary_comparer =
  | Neq 
  | Eq 
  | Lt 
  | Gt 
  | Le 
  | Ge 

type expression =
  | Variable of variable
  | Value of value
  | Binary_operation of expression_binary_operation
  | Binary_comparison of expression_binary_comparison
  | Field_reference of expression_field_reference

and expression_binary_operation = {
  operator : binary_operator;
  left : expression;
  right : expression;
}

and expression_binary_comparison = {
  comparer : binary_comparer;
  left : expression;
  right : expression;
}

and expression_field_reference = {
  base : expression;
  field : string;
}

type formula_concrete_predicate_check = {
  predicate : string;
  arguments : expression list;
  class_ : string option;
}

type formula_concrete_access_check = {
  base : expression;
  field : string;
}

type formula_operator =
  | And 
  | Sep 

type formula_concrete =
  | Expression of expression
  | Predicate_check of formula_concrete_predicate_check
  | Access_check of formula_concrete_access_check
  | Formula_operation of formula_concrete_formula_operation
  | If_then_else of formula_concrete_if_then_else
  | Unfolding_in of formula_concrete_unfolding_in

and formula_concrete_formula_operation = {
  operator : formula_operator;
  left : formula_concrete;
  right : formula_concrete;
}

and formula_concrete_if_then_else = {
  condition : expression;
  then_ : formula_concrete;
  else_ : formula_concrete;
}

and formula_concrete_unfolding_in = {
  predicate : string;
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
  formula : formula;
}

type statement_release = {
  formula : formula;
}

type statement_fold = {
  predicate : string;
  arguments : expression list;
}

type statement_unfold = {
  predicate : string;
  arguments : expression list;
}

type statement =
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

let rec default_type_ () : type_ = Int (0l)

let rec default_class_field 
  ?type_:((type_:type_) = default_type_ ())
  ?id:((id:string) = "")
  () : class_field  = {
  type_;
  id;
}

let rec default_argument 
  ?type_:((type_:type_) = default_type_ ())
  ?id:((id:string) = "")
  () : argument  = {
  type_;
  id;
}

let rec default_variable (): variable = Result

let rec default_value () : value = Int (0l)

let rec default_binary_operator () = (Add:binary_operator)

let rec default_binary_comparer () = (Neq:binary_comparer)

let rec default_expression () : expression = Variable (default_variable ())

and default_expression_binary_operation 
  ?operator:((operator:binary_operator) = default_binary_operator ())
  ?left:((left:expression) = default_expression ())
  ?right:((right:expression) = default_expression ())
  () : expression_binary_operation  = {
  operator;
  left;
  right;
}

and default_expression_binary_comparison 
  ?comparer:((comparer:binary_comparer) = default_binary_comparer ())
  ?left:((left:expression) = default_expression ())
  ?right:((right:expression) = default_expression ())
  () : expression_binary_comparison  = {
  comparer;
  left;
  right;
}

and default_expression_field_reference 
  ?base:((base:expression) = default_expression ())
  ?field:((field:string) = "")
  () : expression_field_reference  = {
  base;
  field;
}

let rec default_formula_concrete_predicate_check 
  ?predicate:((predicate:string) = "")
  ?arguments:((arguments:expression list) = [])
  ?class_:((class_:string option) = None)
  () : formula_concrete_predicate_check  = {
  predicate;
  arguments;
  class_;
}

let rec default_formula_concrete_access_check 
  ?base:((base:expression) = default_expression ())
  ?field:((field:string) = "")
  () : formula_concrete_access_check  = {
  base;
  field;
}

let rec default_formula_operator () = (And:formula_operator)

let rec default_formula_concrete () : formula_concrete = Expression (default_expression ())

and default_formula_concrete_formula_operation 
  ?operator:((operator:formula_operator) = default_formula_operator ())
  ?left:((left:formula_concrete) = default_formula_concrete ())
  ?right:((right:formula_concrete) = default_formula_concrete ())
  () : formula_concrete_formula_operation  = {
  operator;
  left;
  right;
}

and default_formula_concrete_if_then_else 
  ?condition:((condition:expression) = default_expression ())
  ?then_:((then_:formula_concrete) = default_formula_concrete ())
  ?else_:((else_:formula_concrete) = default_formula_concrete ())
  () : formula_concrete_if_then_else  = {
  condition;
  then_;
  else_;
}

and default_formula_concrete_unfolding_in 
  ?predicate:((predicate:string) = "")
  ?arguments:((arguments:expression list) = [])
  ?formula:((formula:formula_concrete) = default_formula_concrete ())
  () : formula_concrete_unfolding_in  = {
  predicate;
  arguments;
  formula;
}

let rec default_formula_imprecise 
  ?concrete:((concrete:formula_concrete) = default_formula_concrete ())
  () : formula_imprecise  = {
  concrete;
}

let rec default_formula () : formula = Concrete (default_formula_concrete ())

let rec default_predicate 
  ?id:((id:string) = "")
  ?arguments:((arguments:argument list) = [])
  ?formula:((formula:formula) = default_formula ())
  () : predicate  = {
  id;
  arguments;
  formula;
}

let rec default_contract 
  ?requires:((requires:formula) = default_formula ())
  ?ensures:((ensures:formula) = default_formula ())
  () : contract  = {
  requires;
  ensures;
}

let rec default_statement_declaration 
  ?type_:((type_:type_) = default_type_ ())
  ?id:((id:string) = "")
  () : statement_declaration  = {
  type_;
  id;
}

let rec default_statement_assignment 
  ?id:((id:string) = "")
  ?value:((value:expression) = default_expression ())
  () : statement_assignment  = {
  id;
  value;
}

let rec default_statement_field_assignment 
  ?base:((base:string) = "")
  ?field:((field:string) = "")
  ?source:((source:string) = "")
  () : statement_field_assignment  = {
  base;
  field;
  source;
}

let rec default_statement_new_object 
  ?id:((id:string) = "")
  ?class_:((class_:string) = "")
  () : statement_new_object  = {
  id;
  class_;
}

let rec default_statement_method_call 
  ?target:((target:string) = "")
  ?base:((base:string) = "")
  ?method_:((method_:string) = "")
  ?arguments:((arguments:string list) = [])
  ?class_:((class_:string option) = None)
  () : statement_method_call  = {
  target;
  base;
  method_;
  arguments;
  class_;
}

let rec default_statement_assertion 
  ?formula:((formula:formula) = default_formula ())
  () : statement_assertion  = {
  formula;
}

let rec default_statement_release 
  ?formula:((formula:formula) = default_formula ())
  () : statement_release  = {
  formula;
}

let rec default_statement_fold 
  ?predicate:((predicate:string) = "")
  ?arguments:((arguments:expression list) = [])
  () : statement_fold  = {
  predicate;
  arguments;
}

let rec default_statement_unfold 
  ?predicate:((predicate:string) = "")
  ?arguments:((arguments:expression list) = [])
  () : statement_unfold  = {
  predicate;
  arguments;
}

let rec default_statement (): statement = Skip

and default_statement_sequence 
  ?statements:((statements:statement list) = [])
  () : statement_sequence  = {
  statements;
}

and default_statement_if_then_else 
  ?condition:((condition:expression) = default_expression ())
  ?then_:((then_:statement) = default_statement ())
  ?else_:((else_:statement) = default_statement ())
  () : statement_if_then_else  = {
  condition;
  then_;
  else_;
}

and default_statement_while_loop 
  ?condition:((condition:expression) = default_expression ())
  ?invariant:((invariant:formula) = default_formula ())
  ?body:((body:statement) = default_statement ())
  () : statement_while_loop  = {
  condition;
  invariant;
  body;
}

and default_statement_hold 
  ?formula:((formula:formula) = default_formula ())
  ?body:((body:statement) = default_statement ())
  () : statement_hold  = {
  formula;
  body;
}

let rec default_method_ 
  ?type_:((type_:type_) = default_type_ ())
  ?id:((id:string) = "")
  ?arguments:((arguments:argument list) = [])
  ?dynamic:((dynamic:contract) = default_contract ())
  ?static:((static:contract) = default_contract ())
  ?body:((body:statement) = default_statement ())
  () : method_  = {
  type_;
  id;
  arguments;
  dynamic;
  static;
  body;
}

let rec default_class_ 
  ?id:((id:string) = "")
  ?super:((super:string) = "")
  ?fields:((fields:class_field list) = [])
  ?predicates:((predicates:predicate list) = [])
  ?methods:((methods:method_ list) = [])
  () : class_  = {
  id;
  super;
  fields;
  predicates;
  methods;
}

let rec default_program 
  ?classes:((classes:class_ list) = [])
  ?statement:((statement:statement) = default_statement ())
  () : program  = {
  classes;
  statement;
}
