[@@@ocaml.warning "-27-30-39"]

type id_mutable = {
  mutable string : string;
}

let default_id_mutable () : id_mutable = {
  string = "";
}

type type_class_mutable = {
  mutable classid : Ast_types.id;
}

let default_type_class_mutable () : type_class_mutable = {
  classid = Ast_types.default_id ();
}

type class_field_mutable = {
  mutable type_ : Ast_types.type_;
  mutable id : Ast_types.id;
}

let default_class_field_mutable () : class_field_mutable = {
  type_ = Ast_types.default_type_ ();
  id = Ast_types.default_id ();
}

type argument_mutable = {
  mutable type_ : Ast_types.type_;
  mutable id : Ast_types.id;
}

let default_argument_mutable () : argument_mutable = {
  type_ = Ast_types.default_type_ ();
  id = Ast_types.default_id ();
}

type variable_old_mutable = {
  mutable id : Ast_types.id;
}

let default_variable_old_mutable () : variable_old_mutable = {
  id = Ast_types.default_id ();
}

type value_int_mutable = {
  mutable value : int32;
}

let default_value_int_mutable () : value_int_mutable = {
  value = 0l;
}

type expression_binary_operation_mutable = {
  mutable binaryoperator : Ast_types.binary_operator;
  mutable binaryoperationleft : Ast_types.expression;
  mutable binaryoperationright : Ast_types.expression;
}

let default_expression_binary_operation_mutable () : expression_binary_operation_mutable = {
  binaryoperator = Ast_types.default_binary_operator ();
  binaryoperationleft = Ast_types.default_expression ();
  binaryoperationright = Ast_types.default_expression ();
}

type expression_binary_comparison_mutable = {
  mutable binarycomparer : Ast_types.binary_comparer;
  mutable binarycomparisonleft : Ast_types.expression;
  mutable binarycomparisonright : Ast_types.expression;
}

let default_expression_binary_comparison_mutable () : expression_binary_comparison_mutable = {
  binarycomparer = Ast_types.default_binary_comparer ();
  binarycomparisonleft = Ast_types.default_expression ();
  binarycomparisonright = Ast_types.default_expression ();
}

type expression_field_reference_mutable = {
  mutable base : Ast_types.expression;
  mutable fieldid : Ast_types.id;
}

let default_expression_field_reference_mutable () : expression_field_reference_mutable = {
  base = Ast_types.default_expression ();
  fieldid = Ast_types.default_id ();
}

type formula_concrete_predicate_check_mutable = {
  mutable predicateid : Ast_types.id;
  mutable arguments : Ast_types.expression list;
}

let default_formula_concrete_predicate_check_mutable () : formula_concrete_predicate_check_mutable = {
  predicateid = Ast_types.default_id ();
  arguments = [];
}

type formula_concrete_access_check_mutable = {
  mutable base : Ast_types.expression;
  mutable fieldid : Ast_types.id;
}

let default_formula_concrete_access_check_mutable () : formula_concrete_access_check_mutable = {
  base = Ast_types.default_expression ();
  fieldid = Ast_types.default_id ();
}

type formula_concrete_logical_and_mutable = {
  mutable andleft : Ast_types.formula_concrete;
  mutable andright : Ast_types.formula_concrete;
}

let default_formula_concrete_logical_and_mutable () : formula_concrete_logical_and_mutable = {
  andleft = Ast_types.default_formula_concrete ();
  andright = Ast_types.default_formula_concrete ();
}

type formula_concrete_logical_separate_mutable = {
  mutable separateleft : Ast_types.formula_concrete;
  mutable separateright : Ast_types.formula_concrete;
}

let default_formula_concrete_logical_separate_mutable () : formula_concrete_logical_separate_mutable = {
  separateleft = Ast_types.default_formula_concrete ();
  separateright = Ast_types.default_formula_concrete ();
}

type formula_concrete_if_then_else_mutable = {
  mutable condition : Ast_types.expression;
  mutable thenformula : Ast_types.formula_concrete;
  mutable elseformula : Ast_types.formula_concrete;
}

let default_formula_concrete_if_then_else_mutable () : formula_concrete_if_then_else_mutable = {
  condition = Ast_types.default_expression ();
  thenformula = Ast_types.default_formula_concrete ();
  elseformula = Ast_types.default_formula_concrete ();
}

type formula_concrete_unfolding_in_mutable = {
  mutable predicateid : Ast_types.id;
  mutable arguments : Ast_types.expression list;
  mutable formula : Ast_types.formula_concrete;
}

let default_formula_concrete_unfolding_in_mutable () : formula_concrete_unfolding_in_mutable = {
  predicateid = Ast_types.default_id ();
  arguments = [];
  formula = Ast_types.default_formula_concrete ();
}

type formula_imprecise_mutable = {
  mutable concrete : Ast_types.formula_concrete;
}

let default_formula_imprecise_mutable () : formula_imprecise_mutable = {
  concrete = Ast_types.default_formula_concrete ();
}

type predicate_mutable = {
  mutable id : Ast_types.id;
  mutable classid : Ast_types.id;
  mutable arguments : Ast_types.argument list;
  mutable formula : Ast_types.formula;
}

let default_predicate_mutable () : predicate_mutable = {
  id = Ast_types.default_id ();
  classid = Ast_types.default_id ();
  arguments = [];
  formula = Ast_types.default_formula ();
}

type contract_mutable = {
  mutable requires : Ast_types.formula;
  mutable ensured : Ast_types.formula;
}

let default_contract_mutable () : contract_mutable = {
  requires = Ast_types.default_formula ();
  ensured = Ast_types.default_formula ();
}

type statement_declaration_mutable = {
  mutable type_ : Ast_types.type_;
  mutable id : Ast_types.id;
}

let default_statement_declaration_mutable () : statement_declaration_mutable = {
  type_ = Ast_types.default_type_ ();
  id = Ast_types.default_id ();
}

type statement_assignment_mutable = {
  mutable id : Ast_types.id;
  mutable value : Ast_types.expression;
}

let default_statement_assignment_mutable () : statement_assignment_mutable = {
  id = Ast_types.default_id ();
  value = Ast_types.default_expression ();
}

type statement_while_loop_mutable = {
  mutable condition : Ast_types.expression;
  mutable invariant : Ast_types.formula;
}

let default_statement_while_loop_mutable () : statement_while_loop_mutable = {
  condition = Ast_types.default_expression ();
  invariant = Ast_types.default_formula ();
}

type statement_field_assignment_mutable = {
  mutable baseid : Ast_types.id;
  mutable fieldid : Ast_types.id;
  mutable sourceid : Ast_types.id;
}

let default_statement_field_assignment_mutable () : statement_field_assignment_mutable = {
  baseid = Ast_types.default_id ();
  fieldid = Ast_types.default_id ();
  sourceid = Ast_types.default_id ();
}

type statement_new_object_mutable = {
  mutable id : Ast_types.id;
  mutable classid : Ast_types.id;
}

let default_statement_new_object_mutable () : statement_new_object_mutable = {
  id = Ast_types.default_id ();
  classid = Ast_types.default_id ();
}

type statement_method_call_mutable = {
  mutable targetid : Ast_types.id;
  mutable baseid : Ast_types.id;
  mutable methodid : Ast_types.id;
  mutable classid : Ast_types.id;
  mutable arguments : Ast_types.id list;
}

let default_statement_method_call_mutable () : statement_method_call_mutable = {
  targetid = Ast_types.default_id ();
  baseid = Ast_types.default_id ();
  methodid = Ast_types.default_id ();
  classid = Ast_types.default_id ();
  arguments = [];
}

type statement_assertion_mutable = {
  mutable formula : Ast_types.formula;
}

let default_statement_assertion_mutable () : statement_assertion_mutable = {
  formula = Ast_types.default_formula ();
}

type statement_release_mutable = {
  mutable formula : Ast_types.formula;
}

let default_statement_release_mutable () : statement_release_mutable = {
  formula = Ast_types.default_formula ();
}

type statement_fold_mutable = {
  mutable predicateid : Ast_types.id;
  mutable arguments : Ast_types.expression list;
}

let default_statement_fold_mutable () : statement_fold_mutable = {
  predicateid = Ast_types.default_id ();
  arguments = [];
}

type statement_unfold_mutable = {
  mutable predicateid : Ast_types.id;
  mutable arguments : Ast_types.expression list;
}

let default_statement_unfold_mutable () : statement_unfold_mutable = {
  predicateid = Ast_types.default_id ();
  arguments = [];
}

type statement_sequence_mutable = {
  mutable prev : Ast_types.statement;
  mutable next : Ast_types.statement;
}

let default_statement_sequence_mutable () : statement_sequence_mutable = {
  prev = Ast_types.default_statement ();
  next = Ast_types.default_statement ();
}

type statement_if_then_else_mutable = {
  mutable condition : Ast_types.expression;
  mutable thenbody : Ast_types.statement;
  mutable elsebody : Ast_types.statement;
}

let default_statement_if_then_else_mutable () : statement_if_then_else_mutable = {
  condition = Ast_types.default_expression ();
  thenbody = Ast_types.default_statement ();
  elsebody = Ast_types.default_statement ();
}

type statement_hold_mutable = {
  mutable formula : Ast_types.formula;
  mutable body : Ast_types.statement;
}

let default_statement_hold_mutable () : statement_hold_mutable = {
  formula = Ast_types.default_formula ();
  body = Ast_types.default_statement ();
}

type method__mutable = {
  mutable type_ : Ast_types.type_;
  mutable id : Ast_types.id;
  mutable arguments : Ast_types.argument list;
  mutable dynamic : Ast_types.contract;
  mutable static : Ast_types.contract;
  mutable body : Ast_types.statement;
}

let default_method__mutable () : method__mutable = {
  type_ = Ast_types.default_type_ ();
  id = Ast_types.default_id ();
  arguments = [];
  dynamic = Ast_types.default_contract ();
  static = Ast_types.default_contract ();
  body = Ast_types.default_statement ();
}

type class__mutable = {
  mutable id : Ast_types.id;
  mutable superid : Ast_types.id;
  mutable fields : Ast_types.class_field list;
  mutable predicates : Ast_types.predicate list;
  mutable methods : Ast_types.method_ list;
}

let default_class__mutable () : class__mutable = {
  id = Ast_types.default_id ();
  superid = Ast_types.default_id ();
  fields = [];
  predicates = [];
  methods = [];
}

type program_mutable = {
  mutable classes : Ast_types.class_ list;
  mutable statement : Ast_types.statement;
}

let default_program_mutable () : program_mutable = {
  classes = [];
  statement = Ast_types.default_statement ();
}


let rec decode_id d =
  let v = default_id_mutable () in
  let continue__= ref true in
  let string_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.string <- Pbrt.Decoder.string d; string_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(id), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !string_is_set then Pbrt.Decoder.missing_field "string" end;
  ({
    Ast_types.string = v.string;
  } : Ast_types.id)

let rec decode_type_class d =
  let v = default_type_class_mutable () in
  let continue__= ref true in
  let classid_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.classid <- decode_id (Pbrt.Decoder.nested d); classid_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(type_class), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !classid_is_set then Pbrt.Decoder.missing_field "classid" end;
  ({
    Ast_types.classid = v.classid;
  } : Ast_types.type_class)

let rec decode_type_ d = 
  let rec loop () = 
    let ret:Ast_types.type_ = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "type_"
      | Some (1, _) -> (Pbrt.Decoder.empty_nested d ; Ast_types.Int)
      | Some (2, _) -> Ast_types.Class (decode_type_class (Pbrt.Decoder.nested d))
      | Some (3, _) -> (Pbrt.Decoder.empty_nested d ; Ast_types.Top)
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_class_field d =
  let v = default_class_field_mutable () in
  let continue__= ref true in
  let id_is_set = ref false in
  let type__is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.type_ <- decode_type_ (Pbrt.Decoder.nested d); type__is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(class_field), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.id <- decode_id (Pbrt.Decoder.nested d); id_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(class_field), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !id_is_set then Pbrt.Decoder.missing_field "id" end;
  begin if not !type__is_set then Pbrt.Decoder.missing_field "type_" end;
  ({
    Ast_types.type_ = v.type_;
    Ast_types.id = v.id;
  } : Ast_types.class_field)

let rec decode_argument d =
  let v = default_argument_mutable () in
  let continue__= ref true in
  let id_is_set = ref false in
  let type__is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.type_ <- decode_type_ (Pbrt.Decoder.nested d); type__is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(argument), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.id <- decode_id (Pbrt.Decoder.nested d); id_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(argument), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !id_is_set then Pbrt.Decoder.missing_field "id" end;
  begin if not !type__is_set then Pbrt.Decoder.missing_field "type_" end;
  ({
    Ast_types.type_ = v.type_;
    Ast_types.id = v.id;
  } : Ast_types.argument)

let rec decode_variable_old d =
  let v = default_variable_old_mutable () in
  let continue__= ref true in
  let id_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.id <- decode_id (Pbrt.Decoder.nested d); id_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(variable_old), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !id_is_set then Pbrt.Decoder.missing_field "id" end;
  ({
    Ast_types.id = v.id;
  } : Ast_types.variable_old)

let rec decode_variable d = 
  let rec loop () = 
    let ret:Ast_types.variable = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "variable"
      | Some (1, _) -> (Pbrt.Decoder.empty_nested d ; Ast_types.Result)
      | Some (2, _) -> Ast_types.Id (decode_id (Pbrt.Decoder.nested d))
      | Some (3, _) -> Ast_types.Old (decode_variable_old (Pbrt.Decoder.nested d))
      | Some (4, _) -> (Pbrt.Decoder.empty_nested d ; Ast_types.This)
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_value_int d =
  let v = default_value_int_mutable () in
  let continue__= ref true in
  let value_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.value <- Pbrt.Decoder.int32_as_varint d; value_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(value_int), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !value_is_set then Pbrt.Decoder.missing_field "value" end;
  ({
    Ast_types.value = v.value;
  } : Ast_types.value_int)

let rec decode_value d = 
  let rec loop () = 
    let ret:Ast_types.value = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "value"
      | Some (1, _) -> Ast_types.Int (decode_value_int (Pbrt.Decoder.nested d))
      | Some (2, _) -> Ast_types.Objectid (decode_id (Pbrt.Decoder.nested d))
      | Some (3, _) -> (Pbrt.Decoder.empty_nested d ; Ast_types.Null)
      | Some (4, _) -> (Pbrt.Decoder.empty_nested d ; Ast_types.True)
      | Some (5, _) -> (Pbrt.Decoder.empty_nested d ; Ast_types.False)
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_binary_operator d = 
  match Pbrt.Decoder.int_as_varint d with
  | 1 -> (Ast_types.Add:Ast_types.binary_operator)
  | 2 -> (Ast_types.Sub:Ast_types.binary_operator)
  | 3 -> (Ast_types.Mul:Ast_types.binary_operator)
  | 4 -> (Ast_types.Div:Ast_types.binary_operator)
  | _ -> Pbrt.Decoder.malformed_variant "binary_operator"

let rec decode_binary_comparer d = 
  match Pbrt.Decoder.int_as_varint d with
  | 1 -> (Ast_types.Neq:Ast_types.binary_comparer)
  | 2 -> (Ast_types.Eq:Ast_types.binary_comparer)
  | 3 -> (Ast_types.Lt:Ast_types.binary_comparer)
  | 4 -> (Ast_types.Gt:Ast_types.binary_comparer)
  | 5 -> (Ast_types.Le:Ast_types.binary_comparer)
  | 6 -> (Ast_types.Ge:Ast_types.binary_comparer)
  | _ -> Pbrt.Decoder.malformed_variant "binary_comparer"

let rec decode_expression d = 
  let rec loop () = 
    let ret:Ast_types.expression = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "expression"
      | Some (1, _) -> Ast_types.Variable (decode_variable (Pbrt.Decoder.nested d))
      | Some (2, _) -> Ast_types.Value (decode_value (Pbrt.Decoder.nested d))
      | Some (3, _) -> Ast_types.Binaryoperation (decode_expression_binary_operation (Pbrt.Decoder.nested d))
      | Some (4, _) -> Ast_types.Binarycomparison (decode_expression_binary_comparison (Pbrt.Decoder.nested d))
      | Some (5, _) -> Ast_types.Fieldreference (decode_expression_field_reference (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_expression_binary_operation d =
  let v = default_expression_binary_operation_mutable () in
  let continue__= ref true in
  let binaryoperationright_is_set = ref false in
  let binaryoperationleft_is_set = ref false in
  let binaryoperator_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.binaryoperator <- decode_binary_operator d; binaryoperator_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expression_binary_operation), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.binaryoperationleft <- decode_expression (Pbrt.Decoder.nested d); binaryoperationleft_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expression_binary_operation), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.binaryoperationright <- decode_expression (Pbrt.Decoder.nested d); binaryoperationright_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expression_binary_operation), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !binaryoperationright_is_set then Pbrt.Decoder.missing_field "binaryoperationright" end;
  begin if not !binaryoperationleft_is_set then Pbrt.Decoder.missing_field "binaryoperationleft" end;
  begin if not !binaryoperator_is_set then Pbrt.Decoder.missing_field "binaryoperator" end;
  ({
    Ast_types.binaryoperator = v.binaryoperator;
    Ast_types.binaryoperationleft = v.binaryoperationleft;
    Ast_types.binaryoperationright = v.binaryoperationright;
  } : Ast_types.expression_binary_operation)

and decode_expression_binary_comparison d =
  let v = default_expression_binary_comparison_mutable () in
  let continue__= ref true in
  let binarycomparisonright_is_set = ref false in
  let binarycomparisonleft_is_set = ref false in
  let binarycomparer_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.binarycomparer <- decode_binary_comparer d; binarycomparer_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expression_binary_comparison), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.binarycomparisonleft <- decode_expression (Pbrt.Decoder.nested d); binarycomparisonleft_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expression_binary_comparison), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.binarycomparisonright <- decode_expression (Pbrt.Decoder.nested d); binarycomparisonright_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expression_binary_comparison), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !binarycomparisonright_is_set then Pbrt.Decoder.missing_field "binarycomparisonright" end;
  begin if not !binarycomparisonleft_is_set then Pbrt.Decoder.missing_field "binarycomparisonleft" end;
  begin if not !binarycomparer_is_set then Pbrt.Decoder.missing_field "binarycomparer" end;
  ({
    Ast_types.binarycomparer = v.binarycomparer;
    Ast_types.binarycomparisonleft = v.binarycomparisonleft;
    Ast_types.binarycomparisonright = v.binarycomparisonright;
  } : Ast_types.expression_binary_comparison)

and decode_expression_field_reference d =
  let v = default_expression_field_reference_mutable () in
  let continue__= ref true in
  let fieldid_is_set = ref false in
  let base_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.base <- decode_expression (Pbrt.Decoder.nested d); base_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expression_field_reference), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.fieldid <- decode_id (Pbrt.Decoder.nested d); fieldid_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expression_field_reference), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !fieldid_is_set then Pbrt.Decoder.missing_field "fieldid" end;
  begin if not !base_is_set then Pbrt.Decoder.missing_field "base" end;
  ({
    Ast_types.base = v.base;
    Ast_types.fieldid = v.fieldid;
  } : Ast_types.expression_field_reference)

let rec decode_formula_concrete_predicate_check d =
  let v = default_formula_concrete_predicate_check_mutable () in
  let continue__= ref true in
  let predicateid_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.arguments <- List.rev v.arguments;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.predicateid <- decode_id (Pbrt.Decoder.nested d); predicateid_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_predicate_check), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.arguments <- (decode_expression (Pbrt.Decoder.nested d)) :: v.arguments;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_predicate_check), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !predicateid_is_set then Pbrt.Decoder.missing_field "predicateid" end;
  ({
    Ast_types.predicateid = v.predicateid;
    Ast_types.arguments = v.arguments;
  } : Ast_types.formula_concrete_predicate_check)

let rec decode_formula_concrete_access_check d =
  let v = default_formula_concrete_access_check_mutable () in
  let continue__= ref true in
  let fieldid_is_set = ref false in
  let base_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.base <- decode_expression (Pbrt.Decoder.nested d); base_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_access_check), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.fieldid <- decode_id (Pbrt.Decoder.nested d); fieldid_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_access_check), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !fieldid_is_set then Pbrt.Decoder.missing_field "fieldid" end;
  begin if not !base_is_set then Pbrt.Decoder.missing_field "base" end;
  ({
    Ast_types.base = v.base;
    Ast_types.fieldid = v.fieldid;
  } : Ast_types.formula_concrete_access_check)

let rec decode_formula_concrete d = 
  let rec loop () = 
    let ret:Ast_types.formula_concrete = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "formula_concrete"
      | Some (1, _) -> Ast_types.Expression (decode_expression (Pbrt.Decoder.nested d))
      | Some (2, _) -> Ast_types.Predicatecheck (decode_formula_concrete_predicate_check (Pbrt.Decoder.nested d))
      | Some (3, _) -> Ast_types.Accesscheck (decode_formula_concrete_access_check (Pbrt.Decoder.nested d))
      | Some (4, _) -> Ast_types.Logicaland (decode_formula_concrete_logical_and (Pbrt.Decoder.nested d))
      | Some (5, _) -> Ast_types.Logicalseparate (decode_formula_concrete_logical_separate (Pbrt.Decoder.nested d))
      | Some (6, _) -> Ast_types.Ifthenelse (decode_formula_concrete_if_then_else (Pbrt.Decoder.nested d))
      | Some (7, _) -> Ast_types.Unfoldingin (decode_formula_concrete_unfolding_in (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_formula_concrete_logical_and d =
  let v = default_formula_concrete_logical_and_mutable () in
  let continue__= ref true in
  let andright_is_set = ref false in
  let andleft_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.andleft <- decode_formula_concrete (Pbrt.Decoder.nested d); andleft_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_logical_and), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.andright <- decode_formula_concrete (Pbrt.Decoder.nested d); andright_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_logical_and), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !andright_is_set then Pbrt.Decoder.missing_field "andright" end;
  begin if not !andleft_is_set then Pbrt.Decoder.missing_field "andleft" end;
  ({
    Ast_types.andleft = v.andleft;
    Ast_types.andright = v.andright;
  } : Ast_types.formula_concrete_logical_and)

and decode_formula_concrete_logical_separate d =
  let v = default_formula_concrete_logical_separate_mutable () in
  let continue__= ref true in
  let separateright_is_set = ref false in
  let separateleft_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.separateleft <- decode_formula_concrete (Pbrt.Decoder.nested d); separateleft_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_logical_separate), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.separateright <- decode_formula_concrete (Pbrt.Decoder.nested d); separateright_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_logical_separate), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !separateright_is_set then Pbrt.Decoder.missing_field "separateright" end;
  begin if not !separateleft_is_set then Pbrt.Decoder.missing_field "separateleft" end;
  ({
    Ast_types.separateleft = v.separateleft;
    Ast_types.separateright = v.separateright;
  } : Ast_types.formula_concrete_logical_separate)

and decode_formula_concrete_if_then_else d =
  let v = default_formula_concrete_if_then_else_mutable () in
  let continue__= ref true in
  let elseformula_is_set = ref false in
  let thenformula_is_set = ref false in
  let condition_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.condition <- decode_expression (Pbrt.Decoder.nested d); condition_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_if_then_else), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.thenformula <- decode_formula_concrete (Pbrt.Decoder.nested d); thenformula_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_if_then_else), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.elseformula <- decode_formula_concrete (Pbrt.Decoder.nested d); elseformula_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_if_then_else), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !elseformula_is_set then Pbrt.Decoder.missing_field "elseformula" end;
  begin if not !thenformula_is_set then Pbrt.Decoder.missing_field "thenformula" end;
  begin if not !condition_is_set then Pbrt.Decoder.missing_field "condition" end;
  ({
    Ast_types.condition = v.condition;
    Ast_types.thenformula = v.thenformula;
    Ast_types.elseformula = v.elseformula;
  } : Ast_types.formula_concrete_if_then_else)

and decode_formula_concrete_unfolding_in d =
  let v = default_formula_concrete_unfolding_in_mutable () in
  let continue__= ref true in
  let formula_is_set = ref false in
  let predicateid_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.arguments <- List.rev v.arguments;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.predicateid <- decode_id (Pbrt.Decoder.nested d); predicateid_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_unfolding_in), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.arguments <- (decode_expression (Pbrt.Decoder.nested d)) :: v.arguments;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_unfolding_in), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.formula <- decode_formula_concrete (Pbrt.Decoder.nested d); formula_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_unfolding_in), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !formula_is_set then Pbrt.Decoder.missing_field "formula" end;
  begin if not !predicateid_is_set then Pbrt.Decoder.missing_field "predicateid" end;
  ({
    Ast_types.predicateid = v.predicateid;
    Ast_types.arguments = v.arguments;
    Ast_types.formula = v.formula;
  } : Ast_types.formula_concrete_unfolding_in)

let rec decode_formula_imprecise d =
  let v = default_formula_imprecise_mutable () in
  let continue__= ref true in
  let concrete_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.concrete <- decode_formula_concrete (Pbrt.Decoder.nested d); concrete_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_imprecise), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !concrete_is_set then Pbrt.Decoder.missing_field "concrete" end;
  ({
    Ast_types.concrete = v.concrete;
  } : Ast_types.formula_imprecise)

let rec decode_formula d = 
  let rec loop () = 
    let ret:Ast_types.formula = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "formula"
      | Some (1, _) -> Ast_types.Concrete (decode_formula_concrete (Pbrt.Decoder.nested d))
      | Some (2, _) -> Ast_types.Imprecise (decode_formula_imprecise (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_predicate d =
  let v = default_predicate_mutable () in
  let continue__= ref true in
  let formula_is_set = ref false in
  let classid_is_set = ref false in
  let id_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.arguments <- List.rev v.arguments;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.id <- decode_id (Pbrt.Decoder.nested d); id_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(predicate), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.classid <- decode_id (Pbrt.Decoder.nested d); classid_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(predicate), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.arguments <- (decode_argument (Pbrt.Decoder.nested d)) :: v.arguments;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(predicate), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.formula <- decode_formula (Pbrt.Decoder.nested d); formula_is_set := true;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(predicate), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !formula_is_set then Pbrt.Decoder.missing_field "formula" end;
  begin if not !classid_is_set then Pbrt.Decoder.missing_field "classid" end;
  begin if not !id_is_set then Pbrt.Decoder.missing_field "id" end;
  ({
    Ast_types.id = v.id;
    Ast_types.classid = v.classid;
    Ast_types.arguments = v.arguments;
    Ast_types.formula = v.formula;
  } : Ast_types.predicate)

let rec decode_contract d =
  let v = default_contract_mutable () in
  let continue__= ref true in
  let ensured_is_set = ref false in
  let requires_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.requires <- decode_formula (Pbrt.Decoder.nested d); requires_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(contract), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.ensured <- decode_formula (Pbrt.Decoder.nested d); ensured_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(contract), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !ensured_is_set then Pbrt.Decoder.missing_field "ensured" end;
  begin if not !requires_is_set then Pbrt.Decoder.missing_field "requires" end;
  ({
    Ast_types.requires = v.requires;
    Ast_types.ensured = v.ensured;
  } : Ast_types.contract)

let rec decode_statement_declaration d =
  let v = default_statement_declaration_mutable () in
  let continue__= ref true in
  let id_is_set = ref false in
  let type__is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.type_ <- decode_type_ (Pbrt.Decoder.nested d); type__is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_declaration), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.id <- decode_id (Pbrt.Decoder.nested d); id_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_declaration), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !id_is_set then Pbrt.Decoder.missing_field "id" end;
  begin if not !type__is_set then Pbrt.Decoder.missing_field "type_" end;
  ({
    Ast_types.type_ = v.type_;
    Ast_types.id = v.id;
  } : Ast_types.statement_declaration)

let rec decode_statement_assignment d =
  let v = default_statement_assignment_mutable () in
  let continue__= ref true in
  let value_is_set = ref false in
  let id_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.id <- decode_id (Pbrt.Decoder.nested d); id_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_assignment), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.value <- decode_expression (Pbrt.Decoder.nested d); value_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_assignment), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !value_is_set then Pbrt.Decoder.missing_field "value" end;
  begin if not !id_is_set then Pbrt.Decoder.missing_field "id" end;
  ({
    Ast_types.id = v.id;
    Ast_types.value = v.value;
  } : Ast_types.statement_assignment)

let rec decode_statement_while_loop d =
  let v = default_statement_while_loop_mutable () in
  let continue__= ref true in
  let invariant_is_set = ref false in
  let condition_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.condition <- decode_expression (Pbrt.Decoder.nested d); condition_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_while_loop), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.invariant <- decode_formula (Pbrt.Decoder.nested d); invariant_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_while_loop), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !invariant_is_set then Pbrt.Decoder.missing_field "invariant" end;
  begin if not !condition_is_set then Pbrt.Decoder.missing_field "condition" end;
  ({
    Ast_types.condition = v.condition;
    Ast_types.invariant = v.invariant;
  } : Ast_types.statement_while_loop)

let rec decode_statement_field_assignment d =
  let v = default_statement_field_assignment_mutable () in
  let continue__= ref true in
  let sourceid_is_set = ref false in
  let fieldid_is_set = ref false in
  let baseid_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.baseid <- decode_id (Pbrt.Decoder.nested d); baseid_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_field_assignment), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.fieldid <- decode_id (Pbrt.Decoder.nested d); fieldid_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_field_assignment), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.sourceid <- decode_id (Pbrt.Decoder.nested d); sourceid_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_field_assignment), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !sourceid_is_set then Pbrt.Decoder.missing_field "sourceid" end;
  begin if not !fieldid_is_set then Pbrt.Decoder.missing_field "fieldid" end;
  begin if not !baseid_is_set then Pbrt.Decoder.missing_field "baseid" end;
  ({
    Ast_types.baseid = v.baseid;
    Ast_types.fieldid = v.fieldid;
    Ast_types.sourceid = v.sourceid;
  } : Ast_types.statement_field_assignment)

let rec decode_statement_new_object d =
  let v = default_statement_new_object_mutable () in
  let continue__= ref true in
  let classid_is_set = ref false in
  let id_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.id <- decode_id (Pbrt.Decoder.nested d); id_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_new_object), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.classid <- decode_id (Pbrt.Decoder.nested d); classid_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_new_object), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !classid_is_set then Pbrt.Decoder.missing_field "classid" end;
  begin if not !id_is_set then Pbrt.Decoder.missing_field "id" end;
  ({
    Ast_types.id = v.id;
    Ast_types.classid = v.classid;
  } : Ast_types.statement_new_object)

let rec decode_statement_method_call d =
  let v = default_statement_method_call_mutable () in
  let continue__= ref true in
  let classid_is_set = ref false in
  let methodid_is_set = ref false in
  let baseid_is_set = ref false in
  let targetid_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.arguments <- List.rev v.arguments;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.targetid <- decode_id (Pbrt.Decoder.nested d); targetid_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_method_call), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.baseid <- decode_id (Pbrt.Decoder.nested d); baseid_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_method_call), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.methodid <- decode_id (Pbrt.Decoder.nested d); methodid_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_method_call), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.classid <- decode_id (Pbrt.Decoder.nested d); classid_is_set := true;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_method_call), field(4)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.arguments <- (decode_id (Pbrt.Decoder.nested d)) :: v.arguments;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_method_call), field(5)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !classid_is_set then Pbrt.Decoder.missing_field "classid" end;
  begin if not !methodid_is_set then Pbrt.Decoder.missing_field "methodid" end;
  begin if not !baseid_is_set then Pbrt.Decoder.missing_field "baseid" end;
  begin if not !targetid_is_set then Pbrt.Decoder.missing_field "targetid" end;
  ({
    Ast_types.targetid = v.targetid;
    Ast_types.baseid = v.baseid;
    Ast_types.methodid = v.methodid;
    Ast_types.classid = v.classid;
    Ast_types.arguments = v.arguments;
  } : Ast_types.statement_method_call)

let rec decode_statement_assertion d =
  let v = default_statement_assertion_mutable () in
  let continue__= ref true in
  let formula_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.formula <- decode_formula (Pbrt.Decoder.nested d); formula_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_assertion), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !formula_is_set then Pbrt.Decoder.missing_field "formula" end;
  ({
    Ast_types.formula = v.formula;
  } : Ast_types.statement_assertion)

let rec decode_statement_release d =
  let v = default_statement_release_mutable () in
  let continue__= ref true in
  let formula_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.formula <- decode_formula (Pbrt.Decoder.nested d); formula_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_release), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !formula_is_set then Pbrt.Decoder.missing_field "formula" end;
  ({
    Ast_types.formula = v.formula;
  } : Ast_types.statement_release)

let rec decode_statement_fold d =
  let v = default_statement_fold_mutable () in
  let continue__= ref true in
  let predicateid_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.arguments <- List.rev v.arguments;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.predicateid <- decode_id (Pbrt.Decoder.nested d); predicateid_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_fold), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.arguments <- (decode_expression (Pbrt.Decoder.nested d)) :: v.arguments;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_fold), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !predicateid_is_set then Pbrt.Decoder.missing_field "predicateid" end;
  ({
    Ast_types.predicateid = v.predicateid;
    Ast_types.arguments = v.arguments;
  } : Ast_types.statement_fold)

let rec decode_statement_unfold d =
  let v = default_statement_unfold_mutable () in
  let continue__= ref true in
  let predicateid_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.arguments <- List.rev v.arguments;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.predicateid <- decode_id (Pbrt.Decoder.nested d); predicateid_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_unfold), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.arguments <- (decode_expression (Pbrt.Decoder.nested d)) :: v.arguments;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_unfold), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !predicateid_is_set then Pbrt.Decoder.missing_field "predicateid" end;
  ({
    Ast_types.predicateid = v.predicateid;
    Ast_types.arguments = v.arguments;
  } : Ast_types.statement_unfold)

let rec decode_statement d = 
  let rec loop () = 
    let ret:Ast_types.statement = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "statement"
      | Some (1, _) -> (Pbrt.Decoder.empty_nested d ; Ast_types.Skip)
      | Some (2, _) -> Ast_types.Sequence (decode_statement_sequence (Pbrt.Decoder.nested d))
      | Some (3, _) -> Ast_types.Declaration (decode_statement_declaration (Pbrt.Decoder.nested d))
      | Some (4, _) -> Ast_types.Assignment (decode_statement_assignment (Pbrt.Decoder.nested d))
      | Some (5, _) -> Ast_types.Ifthenelse (decode_statement_if_then_else (Pbrt.Decoder.nested d))
      | Some (6, _) -> Ast_types.Whileloop (decode_statement_while_loop (Pbrt.Decoder.nested d))
      | Some (7, _) -> Ast_types.Fieldassignment (decode_statement_field_assignment (Pbrt.Decoder.nested d))
      | Some (8, _) -> Ast_types.Newobject (decode_statement_new_object (Pbrt.Decoder.nested d))
      | Some (9, _) -> Ast_types.Methodcall (decode_statement_method_call (Pbrt.Decoder.nested d))
      | Some (10, _) -> Ast_types.Assertion (decode_statement_assertion (Pbrt.Decoder.nested d))
      | Some (11, _) -> Ast_types.Release (decode_statement_release (Pbrt.Decoder.nested d))
      | Some (12, _) -> Ast_types.Hold (decode_statement_hold (Pbrt.Decoder.nested d))
      | Some (13, _) -> Ast_types.Fold (decode_statement_fold (Pbrt.Decoder.nested d))
      | Some (14, _) -> Ast_types.Unfold (decode_statement_unfold (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_statement_sequence d =
  let v = default_statement_sequence_mutable () in
  let continue__= ref true in
  let next_is_set = ref false in
  let prev_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.prev <- decode_statement (Pbrt.Decoder.nested d); prev_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_sequence), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.next <- decode_statement (Pbrt.Decoder.nested d); next_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_sequence), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !next_is_set then Pbrt.Decoder.missing_field "next" end;
  begin if not !prev_is_set then Pbrt.Decoder.missing_field "prev" end;
  ({
    Ast_types.prev = v.prev;
    Ast_types.next = v.next;
  } : Ast_types.statement_sequence)

and decode_statement_if_then_else d =
  let v = default_statement_if_then_else_mutable () in
  let continue__= ref true in
  let elsebody_is_set = ref false in
  let thenbody_is_set = ref false in
  let condition_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.condition <- decode_expression (Pbrt.Decoder.nested d); condition_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_if_then_else), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.thenbody <- decode_statement (Pbrt.Decoder.nested d); thenbody_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_if_then_else), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.elsebody <- decode_statement (Pbrt.Decoder.nested d); elsebody_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_if_then_else), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !elsebody_is_set then Pbrt.Decoder.missing_field "elsebody" end;
  begin if not !thenbody_is_set then Pbrt.Decoder.missing_field "thenbody" end;
  begin if not !condition_is_set then Pbrt.Decoder.missing_field "condition" end;
  ({
    Ast_types.condition = v.condition;
    Ast_types.thenbody = v.thenbody;
    Ast_types.elsebody = v.elsebody;
  } : Ast_types.statement_if_then_else)

and decode_statement_hold d =
  let v = default_statement_hold_mutable () in
  let continue__= ref true in
  let body_is_set = ref false in
  let formula_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.formula <- decode_formula (Pbrt.Decoder.nested d); formula_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_hold), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.body <- decode_statement (Pbrt.Decoder.nested d); body_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_hold), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !body_is_set then Pbrt.Decoder.missing_field "body" end;
  begin if not !formula_is_set then Pbrt.Decoder.missing_field "formula" end;
  ({
    Ast_types.formula = v.formula;
    Ast_types.body = v.body;
  } : Ast_types.statement_hold)

let rec decode_method_ d =
  let v = default_method__mutable () in
  let continue__= ref true in
  let body_is_set = ref false in
  let static_is_set = ref false in
  let dynamic_is_set = ref false in
  let id_is_set = ref false in
  let type__is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.arguments <- List.rev v.arguments;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.type_ <- decode_type_ (Pbrt.Decoder.nested d); type__is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(method_), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.id <- decode_id (Pbrt.Decoder.nested d); id_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(method_), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.arguments <- (decode_argument (Pbrt.Decoder.nested d)) :: v.arguments;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(method_), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.dynamic <- decode_contract (Pbrt.Decoder.nested d); dynamic_is_set := true;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(method_), field(4)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.static <- decode_contract (Pbrt.Decoder.nested d); static_is_set := true;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(method_), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      v.body <- decode_statement (Pbrt.Decoder.nested d); body_is_set := true;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(method_), field(6)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !body_is_set then Pbrt.Decoder.missing_field "body" end;
  begin if not !static_is_set then Pbrt.Decoder.missing_field "static" end;
  begin if not !dynamic_is_set then Pbrt.Decoder.missing_field "dynamic" end;
  begin if not !id_is_set then Pbrt.Decoder.missing_field "id" end;
  begin if not !type__is_set then Pbrt.Decoder.missing_field "type_" end;
  ({
    Ast_types.type_ = v.type_;
    Ast_types.id = v.id;
    Ast_types.arguments = v.arguments;
    Ast_types.dynamic = v.dynamic;
    Ast_types.static = v.static;
    Ast_types.body = v.body;
  } : Ast_types.method_)

let rec decode_class_ d =
  let v = default_class__mutable () in
  let continue__= ref true in
  let superid_is_set = ref false in
  let id_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.methods <- List.rev v.methods;
      v.predicates <- List.rev v.predicates;
      v.fields <- List.rev v.fields;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.id <- decode_id (Pbrt.Decoder.nested d); id_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(class_), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.superid <- decode_id (Pbrt.Decoder.nested d); superid_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(class_), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.fields <- (decode_class_field (Pbrt.Decoder.nested d)) :: v.fields;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(class_), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.predicates <- (decode_predicate (Pbrt.Decoder.nested d)) :: v.predicates;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(class_), field(4)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.methods <- (decode_method_ (Pbrt.Decoder.nested d)) :: v.methods;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(class_), field(5)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !superid_is_set then Pbrt.Decoder.missing_field "superid" end;
  begin if not !id_is_set then Pbrt.Decoder.missing_field "id" end;
  ({
    Ast_types.id = v.id;
    Ast_types.superid = v.superid;
    Ast_types.fields = v.fields;
    Ast_types.predicates = v.predicates;
    Ast_types.methods = v.methods;
  } : Ast_types.class_)

let rec decode_program d =
  let v = default_program_mutable () in
  let continue__= ref true in
  let statement_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.classes <- List.rev v.classes;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.classes <- (decode_class_ (Pbrt.Decoder.nested d)) :: v.classes;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(program), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.statement <- decode_statement (Pbrt.Decoder.nested d); statement_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(program), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !statement_is_set then Pbrt.Decoder.missing_field "statement" end;
  ({
    Ast_types.classes = v.classes;
    Ast_types.statement = v.statement;
  } : Ast_types.program)

let rec encode_id (v:Ast_types.id) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Ast_types.string encoder;
  ()

let rec encode_type_class (v:Ast_types.type_class) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.classid) encoder;
  ()

let rec encode_type_ (v:Ast_types.type_) encoder = 
  begin match v with
  | Ast_types.Int ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Ast_types.Class x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_type_class x) encoder;
  | Ast_types.Top ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  end

let rec encode_class_field (v:Ast_types.class_field) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_type_ v.Ast_types.type_) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.id) encoder;
  ()

let rec encode_argument (v:Ast_types.argument) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_type_ v.Ast_types.type_) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.id) encoder;
  ()

let rec encode_variable_old (v:Ast_types.variable_old) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.id) encoder;
  ()

let rec encode_variable (v:Ast_types.variable) encoder = 
  begin match v with
  | Ast_types.Result ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Ast_types.Id x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_id x) encoder;
  | Ast_types.Old x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_variable_old x) encoder;
  | Ast_types.This ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  end

let rec encode_value_int (v:Ast_types.value_int) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Ast_types.value encoder;
  ()

let rec encode_value (v:Ast_types.value) encoder = 
  begin match v with
  | Ast_types.Int x ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_value_int x) encoder;
  | Ast_types.Objectid x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_id x) encoder;
  | Ast_types.Null ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Ast_types.True ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Ast_types.False ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  end

let rec encode_binary_operator (v:Ast_types.binary_operator) encoder =
  match v with
  | Ast_types.Add -> Pbrt.Encoder.int_as_varint 1 encoder
  | Ast_types.Sub -> Pbrt.Encoder.int_as_varint 2 encoder
  | Ast_types.Mul -> Pbrt.Encoder.int_as_varint 3 encoder
  | Ast_types.Div -> Pbrt.Encoder.int_as_varint 4 encoder

let rec encode_binary_comparer (v:Ast_types.binary_comparer) encoder =
  match v with
  | Ast_types.Neq -> Pbrt.Encoder.int_as_varint 1 encoder
  | Ast_types.Eq -> Pbrt.Encoder.int_as_varint 2 encoder
  | Ast_types.Lt -> Pbrt.Encoder.int_as_varint 3 encoder
  | Ast_types.Gt -> Pbrt.Encoder.int_as_varint 4 encoder
  | Ast_types.Le -> Pbrt.Encoder.int_as_varint 5 encoder
  | Ast_types.Ge -> Pbrt.Encoder.int_as_varint 6 encoder

let rec encode_expression (v:Ast_types.expression) encoder = 
  begin match v with
  | Ast_types.Variable x ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_variable x) encoder;
  | Ast_types.Value x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_value x) encoder;
  | Ast_types.Binaryoperation x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expression_binary_operation x) encoder;
  | Ast_types.Binarycomparison x ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expression_binary_comparison x) encoder;
  | Ast_types.Fieldreference x ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expression_field_reference x) encoder;
  end

and encode_expression_binary_operation (v:Ast_types.expression_binary_operation) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  encode_binary_operator v.Ast_types.binaryoperator encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.binaryoperationleft) encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.binaryoperationright) encoder;
  ()

and encode_expression_binary_comparison (v:Ast_types.expression_binary_comparison) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  encode_binary_comparer v.Ast_types.binarycomparer encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.binarycomparisonleft) encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.binarycomparisonright) encoder;
  ()

and encode_expression_field_reference (v:Ast_types.expression_field_reference) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.base) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.fieldid) encoder;
  ()

let rec encode_formula_concrete_predicate_check (v:Ast_types.formula_concrete_predicate_check) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.predicateid) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expression x) encoder;
  ) v.Ast_types.arguments;
  ()

let rec encode_formula_concrete_access_check (v:Ast_types.formula_concrete_access_check) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.base) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.fieldid) encoder;
  ()

let rec encode_formula_concrete (v:Ast_types.formula_concrete) encoder = 
  begin match v with
  | Ast_types.Expression x ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expression x) encoder;
  | Ast_types.Predicatecheck x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_formula_concrete_predicate_check x) encoder;
  | Ast_types.Accesscheck x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_formula_concrete_access_check x) encoder;
  | Ast_types.Logicaland x ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_formula_concrete_logical_and x) encoder;
  | Ast_types.Logicalseparate x ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_formula_concrete_logical_separate x) encoder;
  | Ast_types.Ifthenelse x ->
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_formula_concrete_if_then_else x) encoder;
  | Ast_types.Unfoldingin x ->
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_formula_concrete_unfolding_in x) encoder;
  end

and encode_formula_concrete_logical_and (v:Ast_types.formula_concrete_logical_and) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula_concrete v.Ast_types.andleft) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula_concrete v.Ast_types.andright) encoder;
  ()

and encode_formula_concrete_logical_separate (v:Ast_types.formula_concrete_logical_separate) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula_concrete v.Ast_types.separateleft) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula_concrete v.Ast_types.separateright) encoder;
  ()

and encode_formula_concrete_if_then_else (v:Ast_types.formula_concrete_if_then_else) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.condition) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula_concrete v.Ast_types.thenformula) encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula_concrete v.Ast_types.elseformula) encoder;
  ()

and encode_formula_concrete_unfolding_in (v:Ast_types.formula_concrete_unfolding_in) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.predicateid) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expression x) encoder;
  ) v.Ast_types.arguments;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula_concrete v.Ast_types.formula) encoder;
  ()

let rec encode_formula_imprecise (v:Ast_types.formula_imprecise) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula_concrete v.Ast_types.concrete) encoder;
  ()

let rec encode_formula (v:Ast_types.formula) encoder = 
  begin match v with
  | Ast_types.Concrete x ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_formula_concrete x) encoder;
  | Ast_types.Imprecise x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_formula_imprecise x) encoder;
  end

let rec encode_predicate (v:Ast_types.predicate) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.id) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.classid) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_argument x) encoder;
  ) v.Ast_types.arguments;
  Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula v.Ast_types.formula) encoder;
  ()

let rec encode_contract (v:Ast_types.contract) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula v.Ast_types.requires) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula v.Ast_types.ensured) encoder;
  ()

let rec encode_statement_declaration (v:Ast_types.statement_declaration) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_type_ v.Ast_types.type_) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.id) encoder;
  ()

let rec encode_statement_assignment (v:Ast_types.statement_assignment) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.id) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.value) encoder;
  ()

let rec encode_statement_while_loop (v:Ast_types.statement_while_loop) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.condition) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula v.Ast_types.invariant) encoder;
  ()

let rec encode_statement_field_assignment (v:Ast_types.statement_field_assignment) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.baseid) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.fieldid) encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.sourceid) encoder;
  ()

let rec encode_statement_new_object (v:Ast_types.statement_new_object) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.id) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.classid) encoder;
  ()

let rec encode_statement_method_call (v:Ast_types.statement_method_call) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.targetid) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.baseid) encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.methodid) encoder;
  Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.classid) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_id x) encoder;
  ) v.Ast_types.arguments;
  ()

let rec encode_statement_assertion (v:Ast_types.statement_assertion) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula v.Ast_types.formula) encoder;
  ()

let rec encode_statement_release (v:Ast_types.statement_release) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula v.Ast_types.formula) encoder;
  ()

let rec encode_statement_fold (v:Ast_types.statement_fold) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.predicateid) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expression x) encoder;
  ) v.Ast_types.arguments;
  ()

let rec encode_statement_unfold (v:Ast_types.statement_unfold) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.predicateid) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expression x) encoder;
  ) v.Ast_types.arguments;
  ()

let rec encode_statement (v:Ast_types.statement) encoder = 
  begin match v with
  | Ast_types.Skip ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Ast_types.Sequence x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_sequence x) encoder;
  | Ast_types.Declaration x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_declaration x) encoder;
  | Ast_types.Assignment x ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_assignment x) encoder;
  | Ast_types.Ifthenelse x ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_if_then_else x) encoder;
  | Ast_types.Whileloop x ->
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_while_loop x) encoder;
  | Ast_types.Fieldassignment x ->
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_field_assignment x) encoder;
  | Ast_types.Newobject x ->
    Pbrt.Encoder.key (8, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_new_object x) encoder;
  | Ast_types.Methodcall x ->
    Pbrt.Encoder.key (9, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_method_call x) encoder;
  | Ast_types.Assertion x ->
    Pbrt.Encoder.key (10, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_assertion x) encoder;
  | Ast_types.Release x ->
    Pbrt.Encoder.key (11, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_release x) encoder;
  | Ast_types.Hold x ->
    Pbrt.Encoder.key (12, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_hold x) encoder;
  | Ast_types.Fold x ->
    Pbrt.Encoder.key (13, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_fold x) encoder;
  | Ast_types.Unfold x ->
    Pbrt.Encoder.key (14, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_unfold x) encoder;
  end

and encode_statement_sequence (v:Ast_types.statement_sequence) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_statement v.Ast_types.prev) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_statement v.Ast_types.next) encoder;
  ()

and encode_statement_if_then_else (v:Ast_types.statement_if_then_else) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.condition) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_statement v.Ast_types.thenbody) encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_statement v.Ast_types.elsebody) encoder;
  ()

and encode_statement_hold (v:Ast_types.statement_hold) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula v.Ast_types.formula) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_statement v.Ast_types.body) encoder;
  ()

let rec encode_method_ (v:Ast_types.method_) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_type_ v.Ast_types.type_) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.id) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_argument x) encoder;
  ) v.Ast_types.arguments;
  Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_contract v.Ast_types.dynamic) encoder;
  Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_contract v.Ast_types.static) encoder;
  Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_statement v.Ast_types.body) encoder;
  ()

let rec encode_class_ (v:Ast_types.class_) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.id) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_id v.Ast_types.superid) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_class_field x) encoder;
  ) v.Ast_types.fields;
  List.iter (fun x -> 
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_predicate x) encoder;
  ) v.Ast_types.predicates;
  List.iter (fun x -> 
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_method_ x) encoder;
  ) v.Ast_types.methods;
  ()

let rec encode_program (v:Ast_types.program) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_class_ x) encoder;
  ) v.Ast_types.classes;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_statement v.Ast_types.statement) encoder;
  ()
