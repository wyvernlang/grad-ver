[@@@ocaml.warning "-27-30-39"]


type id = {
  string : string;
}

type type_class = {
  classid : id;
}

type type_ =
  | Int
  | Class of type_class
  | Top

type class_field = {
  type_ : type_;
  id : id;
}

type argument = {
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
  | This

type value_int = {
  value : int32;
}

type value =
  | Int of value_int
  | Objectid of id
  | Null
  | True
  | False

type binary_operator =
  | Add 
  | Sub 
  | Mul 
  | Div 

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
  | Binaryoperation of expression_binary_operation
  | Binarycomparison of expression_binary_comparison
  | Fieldreference of expression_field_reference

and expression_binary_operation = {
  binaryoperator : binary_operator;
  binaryoperationleft : expression;
  binaryoperationright : expression;
}

and expression_binary_comparison = {
  binarycomparer : binary_comparer;
  binarycomparisonleft : expression;
  binarycomparisonright : expression;
}

and expression_field_reference = {
  base : expression;
  fieldid : id;
}

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
  arguments : argument list;
  formula : formula;
}

type contract = {
  requires : formula;
  ensures : formula;
}

type statement_declaration = {
  type_ : type_;
  id : id;
}

type statement_assignment = {
  id : id;
  value : expression;
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
  classid : id option;
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
  | Assignment of statement_assignment
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
  id : id;
  arguments : argument list;
  dynamic : contract;
  static : contract;
  body : statement;
}

type class_ = {
  id : id;
  superid : id;
  fields : class_field list;
  predicates : predicate list;
  methods : method_ list;
}

type program = {
  classes : class_ list;
  statement : statement;
}

let rec default_id 
  ?string:((string:string) = "")
  () : id  = {
  string;
}

let rec default_type_class 
  ?classid:((classid:id) = default_id ())
  () : type_class  = {
  classid;
}

let rec default_type_ (): type_ = Int

let rec default_class_field 
  ?type_:((type_:type_) = default_type_ ())
  ?id:((id:id) = default_id ())
  () : class_field  = {
  type_;
  id;
}

let rec default_argument 
  ?type_:((type_:type_) = default_type_ ())
  ?id:((id:id) = default_id ())
  () : argument  = {
  type_;
  id;
}

let rec default_variable_old 
  ?id:((id:id) = default_id ())
  () : variable_old  = {
  id;
}

let rec default_variable (): variable = Result

let rec default_value_int 
  ?value:((value:int32) = 0l)
  () : value_int  = {
  value;
}

let rec default_value () : value = Int (default_value_int ())

let rec default_binary_operator () = (Add:binary_operator)

let rec default_binary_comparer () = (Neq:binary_comparer)

let rec default_expression () : expression = Variable (default_variable ())

and default_expression_binary_operation 
  ?binaryoperator:((binaryoperator:binary_operator) = default_binary_operator ())
  ?binaryoperationleft:((binaryoperationleft:expression) = default_expression ())
  ?binaryoperationright:((binaryoperationright:expression) = default_expression ())
  () : expression_binary_operation  = {
  binaryoperator;
  binaryoperationleft;
  binaryoperationright;
}

and default_expression_binary_comparison 
  ?binarycomparer:((binarycomparer:binary_comparer) = default_binary_comparer ())
  ?binarycomparisonleft:((binarycomparisonleft:expression) = default_expression ())
  ?binarycomparisonright:((binarycomparisonright:expression) = default_expression ())
  () : expression_binary_comparison  = {
  binarycomparer;
  binarycomparisonleft;
  binarycomparisonright;
}

and default_expression_field_reference 
  ?base:((base:expression) = default_expression ())
  ?fieldid:((fieldid:id) = default_id ())
  () : expression_field_reference  = {
  base;
  fieldid;
}

let rec default_formula_concrete_predicate_check 
  ?predicateid:((predicateid:id) = default_id ())
  ?arguments:((arguments:expression list) = [])
  () : formula_concrete_predicate_check  = {
  predicateid;
  arguments;
}

let rec default_formula_concrete_access_check 
  ?base:((base:expression) = default_expression ())
  ?fieldid:((fieldid:id) = default_id ())
  () : formula_concrete_access_check  = {
  base;
  fieldid;
}

let rec default_formula_concrete () : formula_concrete = Expression (default_expression ())

and default_formula_concrete_logical_and 
  ?andleft:((andleft:formula_concrete) = default_formula_concrete ())
  ?andright:((andright:formula_concrete) = default_formula_concrete ())
  () : formula_concrete_logical_and  = {
  andleft;
  andright;
}

and default_formula_concrete_logical_separate 
  ?separateleft:((separateleft:formula_concrete) = default_formula_concrete ())
  ?separateright:((separateright:formula_concrete) = default_formula_concrete ())
  () : formula_concrete_logical_separate  = {
  separateleft;
  separateright;
}

and default_formula_concrete_if_then_else 
  ?condition:((condition:expression) = default_expression ())
  ?thenformula:((thenformula:formula_concrete) = default_formula_concrete ())
  ?elseformula:((elseformula:formula_concrete) = default_formula_concrete ())
  () : formula_concrete_if_then_else  = {
  condition;
  thenformula;
  elseformula;
}

and default_formula_concrete_unfolding_in 
  ?predicateid:((predicateid:id) = default_id ())
  ?arguments:((arguments:expression list) = [])
  ?formula:((formula:formula_concrete) = default_formula_concrete ())
  () : formula_concrete_unfolding_in  = {
  predicateid;
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
  ?id:((id:id) = default_id ())
  ?classid:((classid:id) = default_id ())
  ?arguments:((arguments:argument list) = [])
  ?formula:((formula:formula) = default_formula ())
  () : predicate  = {
  id;
  classid;
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
  ?id:((id:id) = default_id ())
  () : statement_declaration  = {
  type_;
  id;
}

let rec default_statement_assignment 
  ?id:((id:id) = default_id ())
  ?value:((value:expression) = default_expression ())
  () : statement_assignment  = {
  id;
  value;
}

let rec default_statement_field_assignment 
  ?baseid:((baseid:id) = default_id ())
  ?fieldid:((fieldid:id) = default_id ())
  ?sourceid:((sourceid:id) = default_id ())
  () : statement_field_assignment  = {
  baseid;
  fieldid;
  sourceid;
}

let rec default_statement_new_object 
  ?id:((id:id) = default_id ())
  ?classid:((classid:id) = default_id ())
  () : statement_new_object  = {
  id;
  classid;
}

let rec default_statement_method_call 
  ?targetid:((targetid:id) = default_id ())
  ?baseid:((baseid:id) = default_id ())
  ?methodid:((methodid:id) = default_id ())
  ?classid:((classid:id option) = None)
  ?arguments:((arguments:id list) = [])
  () : statement_method_call  = {
  targetid;
  baseid;
  methodid;
  classid;
  arguments;
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
  ?predicateid:((predicateid:id) = default_id ())
  ?arguments:((arguments:expression list) = [])
  () : statement_fold  = {
  predicateid;
  arguments;
}

let rec default_statement_unfold 
  ?predicateid:((predicateid:id) = default_id ())
  ?arguments:((arguments:expression list) = [])
  () : statement_unfold  = {
  predicateid;
  arguments;
}

let rec default_statement (): statement = Skip

and default_statement_sequence 
  ?prev:((prev:statement) = default_statement ())
  ?next:((next:statement) = default_statement ())
  () : statement_sequence  = {
  prev;
  next;
}

and default_statement_if_then_else 
  ?condition:((condition:expression) = default_expression ())
  ?thenbody:((thenbody:statement) = default_statement ())
  ?elsebody:((elsebody:statement) = default_statement ())
  () : statement_if_then_else  = {
  condition;
  thenbody;
  elsebody;
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
  ?id:((id:id) = default_id ())
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
  ?id:((id:id) = default_id ())
  ?superid:((superid:id) = default_id ())
  ?fields:((fields:class_field list) = [])
  ?predicates:((predicates:predicate list) = [])
  ?methods:((methods:method_ list) = [])
  () : class_  = {
  id;
  superid;
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
