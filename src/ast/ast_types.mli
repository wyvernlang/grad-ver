(** ast.proto Types *)



(** {2 Types} *)

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
  arg : expression;
}

type formula_access = {
  base : expression list;
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
  t : type_;
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


(** {2 Default values} *)

val default_identifier : 
  ?name:string ->
  unit ->
  identifier
(** [default_identifier ()] is the default value for type [identifier] *)

val default_type_ : unit -> type_
(** [default_type_ ()] is the default value for type [type_] *)

val default_expop : unit -> expop
(** [default_expop ()] is the default value for type [expop] *)

val default_cmpop : unit -> cmpop
(** [default_cmpop ()] is the default value for type [cmpop] *)

val default_val_ : unit -> val_
(** [default_val_ ()] is the default value for type [val_] *)

val default_expression_binop : 
  ?left:expression ->
  ?oper:expop ->
  ?right:expression ->
  unit ->
  expression_binop
(** [default_expression_binop ()] is the default value for type [expression_binop] *)

val default_expression : unit -> expression
(** [default_expression ()] is the default value for type [expression] *)

val default_expression_field_acc : 
  ?base:expression ->
  ?fieldname:identifier ->
  unit ->
  expression_field_acc
(** [default_expression_field_acc ()] is the default value for type [expression_field_acc] *)

val default_formula_compare : 
  ?left:expression ->
  ?oper:cmpop ->
  ?right:expression ->
  unit ->
  formula_compare
(** [default_formula_compare ()] is the default value for type [formula_compare] *)

val default_formula_alpha : 
  ?clsname:identifier ->
  ?arg:expression ->
  unit ->
  formula_alpha
(** [default_formula_alpha ()] is the default value for type [formula_alpha] *)

val default_formula_access : 
  ?base:expression list ->
  ?fieldname:identifier ->
  unit ->
  formula_access
(** [default_formula_access ()] is the default value for type [formula_access] *)

val default_formula_separate : 
  ?left:formula ->
  ?right:formula ->
  unit ->
  formula_separate
(** [default_formula_separate ()] is the default value for type [formula_separate] *)

val default_formula : unit -> formula
(** [default_formula ()] is the default value for type [formula] *)

val default_phi : unit -> phi
(** [default_phi ()] is the default value for type [phi] *)

val default_contract : 
  ?requires:phi ->
  ?ensures:phi ->
  unit ->
  contract
(** [default_contract ()] is the default value for type [contract] *)

val default_statement_assign : 
  ?t:type_ ->
  ?name:identifier ->
  ?value:expression ->
  unit ->
  statement_assign
(** [default_statement_assign ()] is the default value for type [statement_assign] *)

val default_statement_field_assign : 
  ?base:identifier ->
  ?fieldname:identifier ->
  ?source:identifier ->
  unit ->
  statement_field_assign
(** [default_statement_field_assign ()] is the default value for type [statement_field_assign] *)

val default_statement_new : 
  ?name:identifier ->
  ?classname:identifier ->
  unit ->
  statement_new
(** [default_statement_new ()] is the default value for type [statement_new] *)

val default_statement_method_call : 
  ?target:identifier ->
  ?base:identifier ->
  ?methodname:identifier ->
  ?args:identifier list ->
  unit ->
  statement_method_call
(** [default_statement_method_call ()] is the default value for type [statement_method_call] *)

val default_statement_seq : 
  ?prev:statement ->
  ?next:statement ->
  unit ->
  statement_seq
(** [default_statement_seq ()] is the default value for type [statement_seq] *)

val default_statement : unit -> statement
(** [default_statement ()] is the default value for type [statement] *)

val default_statement_if : 
  ?left:identifier ->
  ?oper:cmpop ->
  ?right:identifier ->
  ?thenclause:statement ->
  ?elseclause:statement ->
  unit ->
  statement_if
(** [default_statement_if ()] is the default value for type [statement_if] *)

val default_statement_hold : 
  ?invariant:formula ->
  ?body:statement ->
  unit ->
  statement_hold
(** [default_statement_hold ()] is the default value for type [statement_hold] *)

val default_abs_pred_defn : 
  ?name:identifier ->
  ?args:identifier list ->
  ?body:phi ->
  unit ->
  abs_pred_defn
(** [default_abs_pred_defn ()] is the default value for type [abs_pred_defn] *)

val default_method_argument : 
  ?t:type_ ->
  ?name:identifier ->
  unit ->
  method_argument
(** [default_method_argument ()] is the default value for type [method_argument] *)

val default_method_ : 
  ?name:identifier ->
  ?out_type:type_ ->
  ?args:method_argument list ->
  ?dynamic:contract ->
  ?static:contract ->
  ?body:statement ->
  unit ->
  method_
(** [default_method_ ()] is the default value for type [method_] *)

val default_class_field : 
  ?t:type_ ->
  ?name:identifier ->
  unit ->
  class_field
(** [default_class_field ()] is the default value for type [class_field] *)

val default_class_ : 
  ?name:identifier ->
  ?super:identifier ->
  ?fields:class_field list ->
  ?abspreds:abs_pred_defn list ->
  ?methods:method_ list ->
  unit ->
  class_
(** [default_class_ ()] is the default value for type [class_] *)

val default_program : 
  ?classes:class_ list ->
  ?stmts:statement list ->
  unit ->
  program
(** [default_program ()] is the default value for type [program] *)
