[@@@ocaml.warning "-27-30-39"]


type identifier = {
  name : string;
}

type type_ =
  | Cls of identifier
  | Int
  | Top

type expop =
  | Plus 
  | Minus 
  | Times 
  | Div 

type cmpop =
  | Neq 
  | Eq 
  | Lt 
  | Gt 
  | Le 
  | Ge 

type val_ =
  | Nil
  | Num of int32
  | Cls

type expression_binop = {
  left : expression;
  oper : expop;
  right : expression;
}

and expression =
  | Binop of expression_binop
  | Fieldaccess of expression_field_acc
  | Val of val_
  | Var of identifier

and expression_field_acc = {
  base : expression;
  fieldname : identifier;
}

type formula_compare = {
  left : expression;
  oper : cmpop;
  right : expression;
}

type formula_alpha = {
  clsname : identifier;
  arg : expression list;
}

type formula_access = {
  base : expression;
  fieldname : identifier;
}

type formula_separate = {
  left : formula;
  right : formula;
}

and formula =
  | Cmpf of formula_compare
  | Alpha of formula_alpha
  | Access of formula_access
  | Sep of formula_separate

type phi =
  | Concrete of formula
  | Grad of formula

type contract = {
  requires : phi;
  ensures : phi;
}

type statement_assign = {
  name : identifier;
  value : expression;
}

type statement_field_assign = {
  base : identifier;
  fieldname : identifier;
  source : identifier;
}

type statement_new = {
  name : identifier;
  classname : identifier;
}

type statement_method_call = {
  target : identifier;
  base : identifier;
  methodname : identifier;
  args : identifier list;
}

type statement_seq = {
  prev : statement;
  next : statement;
}

and statement =
  | Skip
  | Seq of statement_seq
  | Assign of statement_assign
  | Ifthen of statement_if
  | Fieldasgn of statement_field_assign
  | New_obj of statement_new
  | Call of statement_method_call
  | Assert of formula
  | Release of formula
  | Hold of statement_hold

and statement_if = {
  left : identifier;
  oper : cmpop;
  right : identifier;
  thenclause : statement;
  elseclause : statement;
}

and statement_hold = {
  invariant : formula;
  body : statement;
}

type statement_declare = {
  t : type_;
  name : identifier;
}

type abs_pred_defn = {
  name : identifier;
  args : identifier list;
  body : phi;
}

type method_argument = {
  t : type_;
  name : identifier;
}

type method_ = {
  name : identifier;
  out_type : type_;
  args : method_argument list;
  dynamic : contract;
  static : contract;
  body : statement;
}

type class_field = {
  t : type_;
  name : identifier;
}

type class_ = {
  name : identifier;
  super : identifier;
  fields : class_field list;
  abspreds : abs_pred_defn list;
  methods : method_ list;
}

type program = {
  classes : class_ list;
  stmts : statement list;
}

let rec default_identifier 
  ?name:((name:string) = "")
  () : identifier  = {
  name;
}

let rec default_type_ () : type_ = Cls (default_identifier ())

let rec default_expop () = (Plus:expop)

let rec default_cmpop () = (Neq:cmpop)

let rec default_val_ (): val_ = Nil

let rec default_expression_binop 
  ?left:((left:expression) = default_expression ())
  ?oper:((oper:expop) = default_expop ())
  ?right:((right:expression) = default_expression ())
  () : expression_binop  = {
  left;
  oper;
  right;
}

and default_expression () : expression = Binop (default_expression_binop ())

and default_expression_field_acc 
  ?base:((base:expression) = default_expression ())
  ?fieldname:((fieldname:identifier) = default_identifier ())
  () : expression_field_acc  = {
  base;
  fieldname;
}

let rec default_formula_compare 
  ?left:((left:expression) = default_expression ())
  ?oper:((oper:cmpop) = default_cmpop ())
  ?right:((right:expression) = default_expression ())
  () : formula_compare  = {
  left;
  oper;
  right;
}

let rec default_formula_alpha 
  ?clsname:((clsname:identifier) = default_identifier ())
  ?arg:((arg:expression list) = [])
  () : formula_alpha  = {
  clsname;
  arg;
}

let rec default_formula_access 
  ?base:((base:expression) = default_expression ())
  ?fieldname:((fieldname:identifier) = default_identifier ())
  () : formula_access  = {
  base;
  fieldname;
}

let rec default_formula_separate 
  ?left:((left:formula) = default_formula ())
  ?right:((right:formula) = default_formula ())
  () : formula_separate  = {
  left;
  right;
}

and default_formula () : formula = Cmpf (default_formula_compare ())

let rec default_phi () : phi = Concrete (default_formula ())

let rec default_contract 
  ?requires:((requires:phi) = default_phi ())
  ?ensures:((ensures:phi) = default_phi ())
  () : contract  = {
  requires;
  ensures;
}

let rec default_statement_assign 
  ?name:((name:identifier) = default_identifier ())
  ?value:((value:expression) = default_expression ())
  () : statement_assign  = {
  name;
  value;
}

let rec default_statement_field_assign 
  ?base:((base:identifier) = default_identifier ())
  ?fieldname:((fieldname:identifier) = default_identifier ())
  ?source:((source:identifier) = default_identifier ())
  () : statement_field_assign  = {
  base;
  fieldname;
  source;
}

let rec default_statement_new 
  ?name:((name:identifier) = default_identifier ())
  ?classname:((classname:identifier) = default_identifier ())
  () : statement_new  = {
  name;
  classname;
}

let rec default_statement_method_call 
  ?target:((target:identifier) = default_identifier ())
  ?base:((base:identifier) = default_identifier ())
  ?methodname:((methodname:identifier) = default_identifier ())
  ?args:((args:identifier list) = [])
  () : statement_method_call  = {
  target;
  base;
  methodname;
  args;
}

let rec default_statement_seq 
  ?prev:((prev:statement) = default_statement ())
  ?next:((next:statement) = default_statement ())
  () : statement_seq  = {
  prev;
  next;
}

and default_statement (): statement = Skip

and default_statement_if 
  ?left:((left:identifier) = default_identifier ())
  ?oper:((oper:cmpop) = default_cmpop ())
  ?right:((right:identifier) = default_identifier ())
  ?thenclause:((thenclause:statement) = default_statement ())
  ?elseclause:((elseclause:statement) = default_statement ())
  () : statement_if  = {
  left;
  oper;
  right;
  thenclause;
  elseclause;
}

and default_statement_hold 
  ?invariant:((invariant:formula) = default_formula ())
  ?body:((body:statement) = default_statement ())
  () : statement_hold  = {
  invariant;
  body;
}

let rec default_statement_declare 
  ?t:((t:type_) = default_type_ ())
  ?name:((name:identifier) = default_identifier ())
  () : statement_declare  = {
  t;
  name;
}

let rec default_abs_pred_defn 
  ?name:((name:identifier) = default_identifier ())
  ?args:((args:identifier list) = [])
  ?body:((body:phi) = default_phi ())
  () : abs_pred_defn  = {
  name;
  args;
  body;
}

let rec default_method_argument 
  ?t:((t:type_) = default_type_ ())
  ?name:((name:identifier) = default_identifier ())
  () : method_argument  = {
  t;
  name;
}

let rec default_method_ 
  ?name:((name:identifier) = default_identifier ())
  ?out_type:((out_type:type_) = default_type_ ())
  ?args:((args:method_argument list) = [])
  ?dynamic:((dynamic:contract) = default_contract ())
  ?static:((static:contract) = default_contract ())
  ?body:((body:statement) = default_statement ())
  () : method_  = {
  name;
  out_type;
  args;
  dynamic;
  static;
  body;
}

let rec default_class_field 
  ?t:((t:type_) = default_type_ ())
  ?name:((name:identifier) = default_identifier ())
  () : class_field  = {
  t;
  name;
}

let rec default_class_ 
  ?name:((name:identifier) = default_identifier ())
  ?super:((super:identifier) = default_identifier ())
  ?fields:((fields:class_field list) = [])
  ?abspreds:((abspreds:abs_pred_defn list) = [])
  ?methods:((methods:method_ list) = [])
  () : class_  = {
  name;
  super;
  fields;
  abspreds;
  methods;
}

let rec default_program 
  ?classes:((classes:class_ list) = [])
  ?stmts:((stmts:statement list) = [])
  () : program  = {
  classes;
  stmts;
}
