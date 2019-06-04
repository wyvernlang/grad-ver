[@@@ocaml.warning "-27-30-39"]

let rec pp_id fmt (v:Ast_types.id) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "string" Pbrt.Pp.pp_string fmt v.Ast_types.string;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_type_class fmt (v:Ast_types.type_class) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "classid" pp_id fmt v.Ast_types.classid;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_type_ fmt (v:Ast_types.type_) =
  match v with
  | Ast_types.Int  -> Format.fprintf fmt "Int"
  | Ast_types.Class x -> Format.fprintf fmt "@[Class(%a)@]" pp_type_class x
  | Ast_types.Top  -> Format.fprintf fmt "Top"

let rec pp_class_field fmt (v:Ast_types.class_field) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "type_" pp_type_ fmt v.Ast_types.type_;
    Pbrt.Pp.pp_record_field "id" pp_id fmt v.Ast_types.id;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_argument fmt (v:Ast_types.argument) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "type_" pp_type_ fmt v.Ast_types.type_;
    Pbrt.Pp.pp_record_field "id" pp_id fmt v.Ast_types.id;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_variable_old fmt (v:Ast_types.variable_old) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "id" pp_id fmt v.Ast_types.id;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_variable fmt (v:Ast_types.variable) =
  match v with
  | Ast_types.Result  -> Format.fprintf fmt "Result"
  | Ast_types.Id x -> Format.fprintf fmt "@[Id(%a)@]" pp_id x
  | Ast_types.Old x -> Format.fprintf fmt "@[Old(%a)@]" pp_variable_old x
  | Ast_types.This  -> Format.fprintf fmt "This"

let rec pp_value_int fmt (v:Ast_types.value_int) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "value" Pbrt.Pp.pp_int32 fmt v.Ast_types.value;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_value fmt (v:Ast_types.value) =
  match v with
  | Ast_types.Int x -> Format.fprintf fmt "@[Int(%a)@]" pp_value_int x
  | Ast_types.Objectid x -> Format.fprintf fmt "@[Objectid(%a)@]" pp_id x
  | Ast_types.Null  -> Format.fprintf fmt "Null"
  | Ast_types.True  -> Format.fprintf fmt "True"
  | Ast_types.False  -> Format.fprintf fmt "False"

let rec pp_binary_operator fmt (v:Ast_types.binary_operator) =
  match v with
  | Ast_types.Add -> Format.fprintf fmt "Add"
  | Ast_types.Sub -> Format.fprintf fmt "Sub"
  | Ast_types.Mul -> Format.fprintf fmt "Mul"
  | Ast_types.Div -> Format.fprintf fmt "Div"

let rec pp_binary_comparer fmt (v:Ast_types.binary_comparer) =
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
  | Ast_types.Binaryoperation x -> Format.fprintf fmt "@[Binaryoperation(%a)@]" pp_expression_binary_operation x
  | Ast_types.Binarycomparison x -> Format.fprintf fmt "@[Binarycomparison(%a)@]" pp_expression_binary_comparison x
  | Ast_types.Fieldreference x -> Format.fprintf fmt "@[Fieldreference(%a)@]" pp_expression_field_reference x

and pp_expression_binary_operation fmt (v:Ast_types.expression_binary_operation) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "binaryoperator" pp_binary_operator fmt v.Ast_types.binaryoperator;
    Pbrt.Pp.pp_record_field "binaryoperationleft" pp_expression fmt v.Ast_types.binaryoperationleft;
    Pbrt.Pp.pp_record_field "binaryoperationright" pp_expression fmt v.Ast_types.binaryoperationright;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_expression_binary_comparison fmt (v:Ast_types.expression_binary_comparison) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "binarycomparer" pp_binary_comparer fmt v.Ast_types.binarycomparer;
    Pbrt.Pp.pp_record_field "binarycomparisonleft" pp_expression fmt v.Ast_types.binarycomparisonleft;
    Pbrt.Pp.pp_record_field "binarycomparisonright" pp_expression fmt v.Ast_types.binarycomparisonright;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_expression_field_reference fmt (v:Ast_types.expression_field_reference) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "base" pp_expression fmt v.Ast_types.base;
    Pbrt.Pp.pp_record_field "fieldid" pp_id fmt v.Ast_types.fieldid;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_formula_concrete_predicate_check fmt (v:Ast_types.formula_concrete_predicate_check) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "predicateid" pp_id fmt v.Ast_types.predicateid;
    Pbrt.Pp.pp_record_field "arguments" (Pbrt.Pp.pp_list pp_expression) fmt v.Ast_types.arguments;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_formula_concrete_access_check fmt (v:Ast_types.formula_concrete_access_check) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "base" pp_expression fmt v.Ast_types.base;
    Pbrt.Pp.pp_record_field "fieldid" pp_id fmt v.Ast_types.fieldid;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_formula_concrete fmt (v:Ast_types.formula_concrete) =
  match v with
  | Ast_types.Expression x -> Format.fprintf fmt "@[Expression(%a)@]" pp_expression x
  | Ast_types.Predicatecheck x -> Format.fprintf fmt "@[Predicatecheck(%a)@]" pp_formula_concrete_predicate_check x
  | Ast_types.Accesscheck x -> Format.fprintf fmt "@[Accesscheck(%a)@]" pp_formula_concrete_access_check x
  | Ast_types.Logicaland x -> Format.fprintf fmt "@[Logicaland(%a)@]" pp_formula_concrete_logical_and x
  | Ast_types.Logicalseparate x -> Format.fprintf fmt "@[Logicalseparate(%a)@]" pp_formula_concrete_logical_separate x
  | Ast_types.Ifthenelse x -> Format.fprintf fmt "@[Ifthenelse(%a)@]" pp_formula_concrete_if_then_else x
  | Ast_types.Unfoldingin x -> Format.fprintf fmt "@[Unfoldingin(%a)@]" pp_formula_concrete_unfolding_in x

and pp_formula_concrete_logical_and fmt (v:Ast_types.formula_concrete_logical_and) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "andleft" pp_formula_concrete fmt v.Ast_types.andleft;
    Pbrt.Pp.pp_record_field "andright" pp_formula_concrete fmt v.Ast_types.andright;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_formula_concrete_logical_separate fmt (v:Ast_types.formula_concrete_logical_separate) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "separateleft" pp_formula_concrete fmt v.Ast_types.separateleft;
    Pbrt.Pp.pp_record_field "separateright" pp_formula_concrete fmt v.Ast_types.separateright;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_formula_concrete_if_then_else fmt (v:Ast_types.formula_concrete_if_then_else) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "condition" pp_expression fmt v.Ast_types.condition;
    Pbrt.Pp.pp_record_field "thenformula" pp_formula_concrete fmt v.Ast_types.thenformula;
    Pbrt.Pp.pp_record_field "elseformula" pp_formula_concrete fmt v.Ast_types.elseformula;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_formula_concrete_unfolding_in fmt (v:Ast_types.formula_concrete_unfolding_in) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "predicateid" pp_id fmt v.Ast_types.predicateid;
    Pbrt.Pp.pp_record_field "arguments" (Pbrt.Pp.pp_list pp_expression) fmt v.Ast_types.arguments;
    Pbrt.Pp.pp_record_field "formula" pp_formula_concrete fmt v.Ast_types.formula;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_formula_imprecise fmt (v:Ast_types.formula_imprecise) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "concrete" pp_formula_concrete fmt v.Ast_types.concrete;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_formula fmt (v:Ast_types.formula) =
  match v with
  | Ast_types.Concrete x -> Format.fprintf fmt "@[Concrete(%a)@]" pp_formula_concrete x
  | Ast_types.Imprecise x -> Format.fprintf fmt "@[Imprecise(%a)@]" pp_formula_imprecise x

let rec pp_predicate fmt (v:Ast_types.predicate) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "id" pp_id fmt v.Ast_types.id;
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
    Pbrt.Pp.pp_record_field "id" pp_id fmt v.Ast_types.id;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_statement_assignment fmt (v:Ast_types.statement_assignment) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "id" pp_id fmt v.Ast_types.id;
    Pbrt.Pp.pp_record_field "value" pp_expression fmt v.Ast_types.value;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_statement_field_assignment fmt (v:Ast_types.statement_field_assignment) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "baseid" pp_id fmt v.Ast_types.baseid;
    Pbrt.Pp.pp_record_field "fieldid" pp_id fmt v.Ast_types.fieldid;
    Pbrt.Pp.pp_record_field "sourceid" pp_id fmt v.Ast_types.sourceid;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_statement_new_object fmt (v:Ast_types.statement_new_object) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "id" pp_id fmt v.Ast_types.id;
    Pbrt.Pp.pp_record_field "classid" pp_id fmt v.Ast_types.classid;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_statement_method_call fmt (v:Ast_types.statement_method_call) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "targetid" pp_id fmt v.Ast_types.targetid;
    Pbrt.Pp.pp_record_field "baseid" pp_id fmt v.Ast_types.baseid;
    Pbrt.Pp.pp_record_field "methodid" pp_id fmt v.Ast_types.methodid;
    Pbrt.Pp.pp_record_field "arguments" (Pbrt.Pp.pp_list pp_id) fmt v.Ast_types.arguments;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_statement_assertion fmt (v:Ast_types.statement_assertion) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "formula" pp_formula fmt v.Ast_types.formula;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_statement_release fmt (v:Ast_types.statement_release) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "formula" pp_formula fmt v.Ast_types.formula;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_statement_fold fmt (v:Ast_types.statement_fold) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "predicateid" pp_id fmt v.Ast_types.predicateid;
    Pbrt.Pp.pp_record_field "arguments" (Pbrt.Pp.pp_list pp_expression) fmt v.Ast_types.arguments;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_statement_unfold fmt (v:Ast_types.statement_unfold) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "predicateid" pp_id fmt v.Ast_types.predicateid;
    Pbrt.Pp.pp_record_field "arguments" (Pbrt.Pp.pp_list pp_expression) fmt v.Ast_types.arguments;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_statement fmt (v:Ast_types.statement) =
  match v with
  | Ast_types.Skip  -> Format.fprintf fmt "Skip"
  | Ast_types.Sequence x -> Format.fprintf fmt "@[Sequence(%a)@]" pp_statement_sequence x
  | Ast_types.Declaration x -> Format.fprintf fmt "@[Declaration(%a)@]" pp_statement_declaration x
  | Ast_types.Assignment x -> Format.fprintf fmt "@[Assignment(%a)@]" pp_statement_assignment x
  | Ast_types.Ifthenelse x -> Format.fprintf fmt "@[Ifthenelse(%a)@]" pp_statement_if_then_else x
  | Ast_types.Whileloop x -> Format.fprintf fmt "@[Whileloop(%a)@]" pp_statement_while_loop x
  | Ast_types.Fieldassignment x -> Format.fprintf fmt "@[Fieldassignment(%a)@]" pp_statement_field_assignment x
  | Ast_types.Newobject x -> Format.fprintf fmt "@[Newobject(%a)@]" pp_statement_new_object x
  | Ast_types.Methodcall x -> Format.fprintf fmt "@[Methodcall(%a)@]" pp_statement_method_call x
  | Ast_types.Assertion x -> Format.fprintf fmt "@[Assertion(%a)@]" pp_statement_assertion x
  | Ast_types.Release x -> Format.fprintf fmt "@[Release(%a)@]" pp_statement_release x
  | Ast_types.Hold x -> Format.fprintf fmt "@[Hold(%a)@]" pp_statement_hold x
  | Ast_types.Fold x -> Format.fprintf fmt "@[Fold(%a)@]" pp_statement_fold x
  | Ast_types.Unfold x -> Format.fprintf fmt "@[Unfold(%a)@]" pp_statement_unfold x

and pp_statement_sequence fmt (v:Ast_types.statement_sequence) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "prev" pp_statement fmt v.Ast_types.prev;
    Pbrt.Pp.pp_record_field "next" pp_statement fmt v.Ast_types.next;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_statement_if_then_else fmt (v:Ast_types.statement_if_then_else) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "ifcondition" pp_expression fmt v.Ast_types.ifcondition;
    Pbrt.Pp.pp_record_field "thenbody" pp_statement fmt v.Ast_types.thenbody;
    Pbrt.Pp.pp_record_field "elsebody" pp_statement fmt v.Ast_types.elsebody;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_statement_while_loop fmt (v:Ast_types.statement_while_loop) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "whilecondition" pp_expression fmt v.Ast_types.whilecondition;
    Pbrt.Pp.pp_record_field "invariant" pp_formula fmt v.Ast_types.invariant;
    Pbrt.Pp.pp_record_field "whilebody" pp_statement fmt v.Ast_types.whilebody;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_statement_hold fmt (v:Ast_types.statement_hold) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "formula" pp_formula fmt v.Ast_types.formula;
    Pbrt.Pp.pp_record_field "holdbody" pp_statement fmt v.Ast_types.holdbody;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_method_ fmt (v:Ast_types.method_) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "type_" pp_type_ fmt v.Ast_types.type_;
    Pbrt.Pp.pp_record_field "id" pp_id fmt v.Ast_types.id;
    Pbrt.Pp.pp_record_field "arguments" (Pbrt.Pp.pp_list pp_argument) fmt v.Ast_types.arguments;
    Pbrt.Pp.pp_record_field "dynamic" pp_contract fmt v.Ast_types.dynamic;
    Pbrt.Pp.pp_record_field "static" pp_contract fmt v.Ast_types.static;
    Pbrt.Pp.pp_record_field "methodbody" pp_statement fmt v.Ast_types.methodbody;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_class_ fmt (v:Ast_types.class_) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "id" pp_id fmt v.Ast_types.id;
    Pbrt.Pp.pp_record_field "superid" pp_id fmt v.Ast_types.superid;
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
