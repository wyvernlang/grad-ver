[@@@ocaml.warning "-27-30-39"]

let rec pp_type_ fmt (v:Ast_types.type_) =
  match v with
  | Ast_types.Int  -> Format.fprintf fmt "Int"
  | Ast_types.Bool  -> Format.fprintf fmt "Bool"
  | Ast_types.Class x -> Format.fprintf fmt "@[Class(%a)@]" Pbrt.Pp.pp_string x
  | Ast_types.Top  -> Format.fprintf fmt "Top"

let rec pp_class_field fmt (v:Ast_types.class_field) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "type_" pp_type_ fmt v.Ast_types.type_;
    Pbrt.Pp.pp_record_field "id" Pbrt.Pp.pp_string fmt v.Ast_types.id;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_argument fmt (v:Ast_types.argument) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "type_" pp_type_ fmt v.Ast_types.type_;
    Pbrt.Pp.pp_record_field "id" Pbrt.Pp.pp_string fmt v.Ast_types.id;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_variable fmt (v:Ast_types.variable) =
  match v with
  | Ast_types.Result  -> Format.fprintf fmt "Result"
  | Ast_types.Id x -> Format.fprintf fmt "@[Id(%a)@]" Pbrt.Pp.pp_string x
  | Ast_types.Old x -> Format.fprintf fmt "@[Old(%a)@]" Pbrt.Pp.pp_string x
  | Ast_types.This  -> Format.fprintf fmt "This"

let rec pp_value fmt (v:Ast_types.value) =
  match v with
  | Ast_types.Int x -> Format.fprintf fmt "@[Int(%a)@]" Pbrt.Pp.pp_int32 x
  | Ast_types.Bool x -> Format.fprintf fmt "@[Bool(%a)@]" Pbrt.Pp.pp_bool x
  | Ast_types.Object x -> Format.fprintf fmt "@[Object(%a)@]" Pbrt.Pp.pp_string x
  | Ast_types.Null  -> Format.fprintf fmt "Null"

let rec pp_expression_operator fmt (v:Ast_types.expression_operator) =
  match v with
  | Ast_types.Add -> Format.fprintf fmt "Add"
  | Ast_types.Sub -> Format.fprintf fmt "Sub"
  | Ast_types.Mul -> Format.fprintf fmt "Mul"
  | Ast_types.Div -> Format.fprintf fmt "Div"
  | Ast_types.And -> Format.fprintf fmt "And"
  | Ast_types.Or -> Format.fprintf fmt "Or"

let rec pp_expression_comparer fmt (v:Ast_types.expression_comparer) =
  match v with
  | Ast_types.Neq -> Format.fprintf fmt "Neq"
  | Ast_types.Eq -> Format.fprintf fmt "Eq"
  | Ast_types.Lt -> Format.fprintf fmt "Lt"
  | Ast_types.Gt -> Format.fprintf fmt "Gt"
  | Ast_types.Le -> Format.fprintf fmt "Le"
  | Ast_types.Ge -> Format.fprintf fmt "Ge"

let rec pp_expression fmt (v:Ast_types.expression) =
  match v with
  | Ast_types.Variable x -> Format.fprintf fmt "@[Variable(%a)@]" pp_variable x
  | Ast_types.Value x -> Format.fprintf fmt "@[Value(%a)@]" pp_value x
  | Ast_types.Operation x -> Format.fprintf fmt "@[Operation(%a)@]" pp_expression_operation x
  | Ast_types.Comparison x -> Format.fprintf fmt "@[Comparison(%a)@]" pp_expression_comparison x
  | Ast_types.Field_reference x -> Format.fprintf fmt "@[Field_reference(%a)@]" pp_expression_field_reference x

and pp_expression_operation fmt (v:Ast_types.expression_operation) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "operator" pp_expression_operator fmt v.Ast_types.operator;
    Pbrt.Pp.pp_record_field "left" pp_expression fmt v.Ast_types.left;
    Pbrt.Pp.pp_record_field "right" pp_expression fmt v.Ast_types.right;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_expression_comparison fmt (v:Ast_types.expression_comparison) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "comparer" pp_expression_comparer fmt v.Ast_types.comparer;
    Pbrt.Pp.pp_record_field "left" pp_expression fmt v.Ast_types.left;
    Pbrt.Pp.pp_record_field "right" pp_expression fmt v.Ast_types.right;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_expression_field_reference fmt (v:Ast_types.expression_field_reference) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "base" pp_expression fmt v.Ast_types.base;
    Pbrt.Pp.pp_record_field "field" Pbrt.Pp.pp_string fmt v.Ast_types.field;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_predicate_check fmt (v:Ast_types.predicate_check) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "predicate" Pbrt.Pp.pp_string fmt v.Ast_types.predicate;
    Pbrt.Pp.pp_record_field "arguments" (Pbrt.Pp.pp_list pp_expression) fmt v.Ast_types.arguments;
    Pbrt.Pp.pp_record_field "class_" (Pbrt.Pp.pp_option Pbrt.Pp.pp_string) fmt v.Ast_types.class_;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_concrete_access_check fmt (v:Ast_types.concrete_access_check) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "base" pp_expression fmt v.Ast_types.base;
    Pbrt.Pp.pp_record_field "field" Pbrt.Pp.pp_string fmt v.Ast_types.field;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_concrete_operator fmt (v:Ast_types.concrete_operator) =
  match v with
  | Ast_types.And -> Format.fprintf fmt "And"
  | Ast_types.Sep -> Format.fprintf fmt "Sep"

let rec pp_concrete fmt (v:Ast_types.concrete) =
  match v with
  | Ast_types.Expression x -> Format.fprintf fmt "@[Expression(%a)@]" pp_expression x
  | Ast_types.Predicate_check x -> Format.fprintf fmt "@[Predicate_check(%a)@]" pp_predicate_check x
  | Ast_types.Access_check x -> Format.fprintf fmt "@[Access_check(%a)@]" pp_concrete_access_check x
  | Ast_types.Operation x -> Format.fprintf fmt "@[Operation(%a)@]" pp_concrete_operation x
  | Ast_types.If_then_else x -> Format.fprintf fmt "@[If_then_else(%a)@]" pp_concrete_if_then_else x
  | Ast_types.Unfolding_in x -> Format.fprintf fmt "@[Unfolding_in(%a)@]" pp_concrete_unfolding_in x

and pp_concrete_operation fmt (v:Ast_types.concrete_operation) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "operator" pp_concrete_operator fmt v.Ast_types.operator;
    Pbrt.Pp.pp_record_field "left" pp_concrete fmt v.Ast_types.left;
    Pbrt.Pp.pp_record_field "right" pp_concrete fmt v.Ast_types.right;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_concrete_if_then_else fmt (v:Ast_types.concrete_if_then_else) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "condition" pp_expression fmt v.Ast_types.condition;
    Pbrt.Pp.pp_record_field "then_" pp_concrete fmt v.Ast_types.then_;
    Pbrt.Pp.pp_record_field "else_" pp_concrete fmt v.Ast_types.else_;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_concrete_unfolding_in fmt (v:Ast_types.concrete_unfolding_in) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "predicate_check" pp_predicate_check fmt v.Ast_types.predicate_check;
    Pbrt.Pp.pp_record_field "formula" pp_concrete fmt v.Ast_types.formula;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_formula fmt (v:Ast_types.formula) =
  match v with
  | Ast_types.Concrete x -> Format.fprintf fmt "@[Concrete(%a)@]" pp_concrete x
  | Ast_types.Imprecise x -> Format.fprintf fmt "@[Imprecise(%a)@]" pp_concrete x

let rec pp_predicate fmt (v:Ast_types.predicate) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "id" Pbrt.Pp.pp_string fmt v.Ast_types.id;
    Pbrt.Pp.pp_record_field "arguments" (Pbrt.Pp.pp_list pp_argument) fmt v.Ast_types.arguments;
    Pbrt.Pp.pp_record_field "formula" pp_formula fmt v.Ast_types.formula;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_contract fmt (v:Ast_types.contract) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "requires" pp_formula fmt v.Ast_types.requires;
    Pbrt.Pp.pp_record_field "ensures" pp_formula fmt v.Ast_types.ensures;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_statement_declaration fmt (v:Ast_types.statement_declaration) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "type_" pp_type_ fmt v.Ast_types.type_;
    Pbrt.Pp.pp_record_field "id" Pbrt.Pp.pp_string fmt v.Ast_types.id;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_statement_assignment fmt (v:Ast_types.statement_assignment) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "id" Pbrt.Pp.pp_string fmt v.Ast_types.id;
    Pbrt.Pp.pp_record_field "value" pp_expression fmt v.Ast_types.value;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_statement_field_assignment fmt (v:Ast_types.statement_field_assignment) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "base" Pbrt.Pp.pp_string fmt v.Ast_types.base;
    Pbrt.Pp.pp_record_field "field" Pbrt.Pp.pp_string fmt v.Ast_types.field;
    Pbrt.Pp.pp_record_field "source" Pbrt.Pp.pp_string fmt v.Ast_types.source;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_statement_new_object fmt (v:Ast_types.statement_new_object) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "id" Pbrt.Pp.pp_string fmt v.Ast_types.id;
    Pbrt.Pp.pp_record_field "class_" Pbrt.Pp.pp_string fmt v.Ast_types.class_;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_statement_method_call fmt (v:Ast_types.statement_method_call) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "target" Pbrt.Pp.pp_string fmt v.Ast_types.target;
    Pbrt.Pp.pp_record_field "base" Pbrt.Pp.pp_string fmt v.Ast_types.base;
    Pbrt.Pp.pp_record_field "method_" Pbrt.Pp.pp_string fmt v.Ast_types.method_;
    Pbrt.Pp.pp_record_field "arguments" (Pbrt.Pp.pp_list Pbrt.Pp.pp_string) fmt v.Ast_types.arguments;
    Pbrt.Pp.pp_record_field "class_" (Pbrt.Pp.pp_option Pbrt.Pp.pp_string) fmt v.Ast_types.class_;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_statement_assertion fmt (v:Ast_types.statement_assertion) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "concrete" pp_concrete fmt v.Ast_types.concrete;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_statement_release fmt (v:Ast_types.statement_release) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "concrete" pp_concrete fmt v.Ast_types.concrete;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_statement_fold fmt (v:Ast_types.statement_fold) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "predicate_check" pp_predicate_check fmt v.Ast_types.predicate_check;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_statement_unfold fmt (v:Ast_types.statement_unfold) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "predicate_check" pp_predicate_check fmt v.Ast_types.predicate_check;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_statement fmt (v:Ast_types.statement) =
  match v with
  | Ast_types.Skip  -> Format.fprintf fmt "Skip"
  | Ast_types.Sequence x -> Format.fprintf fmt "@[Sequence(%a)@]" pp_statement_sequence x
  | Ast_types.Declaration x -> Format.fprintf fmt "@[Declaration(%a)@]" pp_statement_declaration x
  | Ast_types.Assignment x -> Format.fprintf fmt "@[Assignment(%a)@]" pp_statement_assignment x
  | Ast_types.If_then_else x -> Format.fprintf fmt "@[If_then_else(%a)@]" pp_statement_if_then_else x
  | Ast_types.While_loop x -> Format.fprintf fmt "@[While_loop(%a)@]" pp_statement_while_loop x
  | Ast_types.Field_assignment x -> Format.fprintf fmt "@[Field_assignment(%a)@]" pp_statement_field_assignment x
  | Ast_types.New_object x -> Format.fprintf fmt "@[New_object(%a)@]" pp_statement_new_object x
  | Ast_types.Method_call x -> Format.fprintf fmt "@[Method_call(%a)@]" pp_statement_method_call x
  | Ast_types.Assertion x -> Format.fprintf fmt "@[Assertion(%a)@]" pp_statement_assertion x
  | Ast_types.Release x -> Format.fprintf fmt "@[Release(%a)@]" pp_statement_release x
  | Ast_types.Hold x -> Format.fprintf fmt "@[Hold(%a)@]" pp_statement_hold x
  | Ast_types.Fold x -> Format.fprintf fmt "@[Fold(%a)@]" pp_statement_fold x
  | Ast_types.Unfold x -> Format.fprintf fmt "@[Unfold(%a)@]" pp_statement_unfold x

and pp_statement_sequence fmt (v:Ast_types.statement_sequence) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "statements" (Pbrt.Pp.pp_list pp_statement) fmt v.Ast_types.statements;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_statement_if_then_else fmt (v:Ast_types.statement_if_then_else) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "condition" pp_expression fmt v.Ast_types.condition;
    Pbrt.Pp.pp_record_field "then_" pp_statement fmt v.Ast_types.then_;
    Pbrt.Pp.pp_record_field "else_" pp_statement fmt v.Ast_types.else_;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_statement_while_loop fmt (v:Ast_types.statement_while_loop) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "condition" pp_expression fmt v.Ast_types.condition;
    Pbrt.Pp.pp_record_field "invariant" pp_formula fmt v.Ast_types.invariant;
    Pbrt.Pp.pp_record_field "body" pp_statement fmt v.Ast_types.body;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_statement_hold fmt (v:Ast_types.statement_hold) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "formula" pp_formula fmt v.Ast_types.formula;
    Pbrt.Pp.pp_record_field "body" pp_statement fmt v.Ast_types.body;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_method_ fmt (v:Ast_types.method_) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "type_" pp_type_ fmt v.Ast_types.type_;
    Pbrt.Pp.pp_record_field "id" Pbrt.Pp.pp_string fmt v.Ast_types.id;
    Pbrt.Pp.pp_record_field "arguments" (Pbrt.Pp.pp_list pp_argument) fmt v.Ast_types.arguments;
    Pbrt.Pp.pp_record_field "dynamic" pp_contract fmt v.Ast_types.dynamic;
    Pbrt.Pp.pp_record_field "static" pp_contract fmt v.Ast_types.static;
    Pbrt.Pp.pp_record_field "body" pp_statement fmt v.Ast_types.body;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_class_ fmt (v:Ast_types.class_) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "id" Pbrt.Pp.pp_string fmt v.Ast_types.id;
    Pbrt.Pp.pp_record_field "super" Pbrt.Pp.pp_string fmt v.Ast_types.super;
    Pbrt.Pp.pp_record_field "fields" (Pbrt.Pp.pp_list pp_class_field) fmt v.Ast_types.fields;
    Pbrt.Pp.pp_record_field "predicates" (Pbrt.Pp.pp_list pp_predicate) fmt v.Ast_types.predicates;
    Pbrt.Pp.pp_record_field "methods" (Pbrt.Pp.pp_list pp_method_) fmt v.Ast_types.methods;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_program fmt (v:Ast_types.program) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "classes" (Pbrt.Pp.pp_list pp_class_) fmt v.Ast_types.classes;
    Pbrt.Pp.pp_record_field "statement" pp_statement fmt v.Ast_types.statement;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
