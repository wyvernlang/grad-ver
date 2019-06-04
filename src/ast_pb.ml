[@@@ocaml.warning "-27-30-39"]

type class_field_mutable = {
  mutable type_ : Ast_types.type_;
  mutable id : string;
}

let default_class_field_mutable () : class_field_mutable = {
  type_ = Ast_types.default_type_ ();
  id = "";
}

type argument_mutable = {
  mutable type_ : Ast_types.type_;
  mutable id : string;
}

let default_argument_mutable () : argument_mutable = {
  type_ = Ast_types.default_type_ ();
  id = "";
}

type expression_binary_operation_mutable = {
  mutable operator : Ast_types.binary_operator;
  mutable left : Ast_types.expression;
  mutable right : Ast_types.expression;
}

let default_expression_binary_operation_mutable () : expression_binary_operation_mutable = {
  operator = Ast_types.default_binary_operator ();
  left = Ast_types.default_expression ();
  right = Ast_types.default_expression ();
}

type expression_binary_comparison_mutable = {
  mutable comparer : Ast_types.binary_comparer;
  mutable left : Ast_types.expression;
  mutable right : Ast_types.expression;
}

let default_expression_binary_comparison_mutable () : expression_binary_comparison_mutable = {
  comparer = Ast_types.default_binary_comparer ();
  left = Ast_types.default_expression ();
  right = Ast_types.default_expression ();
}

type expression_field_reference_mutable = {
  mutable base : Ast_types.expression;
  mutable field : string;
}

let default_expression_field_reference_mutable () : expression_field_reference_mutable = {
  base = Ast_types.default_expression ();
  field = "";
}

type formula_concrete_predicate_check_mutable = {
  mutable predicate : string;
  mutable arguments : Ast_types.expression list;
  mutable class_ : string option;
}

let default_formula_concrete_predicate_check_mutable () : formula_concrete_predicate_check_mutable = {
  predicate = "";
  arguments = [];
  class_ = None;
}

type formula_concrete_access_check_mutable = {
  mutable base : Ast_types.expression;
  mutable field : string;
}

let default_formula_concrete_access_check_mutable () : formula_concrete_access_check_mutable = {
  base = Ast_types.default_expression ();
  field = "";
}

type formula_concrete_formula_operation_mutable = {
  mutable operator : Ast_types.formula_operator;
  mutable left : Ast_types.formula_concrete;
  mutable right : Ast_types.formula_concrete;
}

let default_formula_concrete_formula_operation_mutable () : formula_concrete_formula_operation_mutable = {
  operator = Ast_types.default_formula_operator ();
  left = Ast_types.default_formula_concrete ();
  right = Ast_types.default_formula_concrete ();
}

type formula_concrete_if_then_else_mutable = {
  mutable condition : Ast_types.expression;
  mutable then_ : Ast_types.formula_concrete;
  mutable else_ : Ast_types.formula_concrete;
}

let default_formula_concrete_if_then_else_mutable () : formula_concrete_if_then_else_mutable = {
  condition = Ast_types.default_expression ();
  then_ = Ast_types.default_formula_concrete ();
  else_ = Ast_types.default_formula_concrete ();
}

type formula_concrete_unfolding_in_mutable = {
  mutable predicate : string;
  mutable arguments : Ast_types.expression list;
  mutable formula : Ast_types.formula_concrete;
}

let default_formula_concrete_unfolding_in_mutable () : formula_concrete_unfolding_in_mutable = {
  predicate = "";
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
  mutable id : string;
  mutable arguments : Ast_types.argument list;
  mutable formula : Ast_types.formula;
}

let default_predicate_mutable () : predicate_mutable = {
  id = "";
  arguments = [];
  formula = Ast_types.default_formula ();
}

type contract_mutable = {
  mutable requires : Ast_types.formula;
  mutable ensures : Ast_types.formula;
}

let default_contract_mutable () : contract_mutable = {
  requires = Ast_types.default_formula ();
  ensures = Ast_types.default_formula ();
}

type statement_declaration_mutable = {
  mutable type_ : Ast_types.type_;
  mutable id : string;
}

let default_statement_declaration_mutable () : statement_declaration_mutable = {
  type_ = Ast_types.default_type_ ();
  id = "";
}

type statement_assignment_mutable = {
  mutable id : string;
  mutable value : Ast_types.expression;
}

let default_statement_assignment_mutable () : statement_assignment_mutable = {
  id = "";
  value = Ast_types.default_expression ();
}

type statement_field_assignment_mutable = {
  mutable base : string;
  mutable field : string;
  mutable source : string;
}

let default_statement_field_assignment_mutable () : statement_field_assignment_mutable = {
  base = "";
  field = "";
  source = "";
}

type statement_new_object_mutable = {
  mutable id : string;
  mutable class_ : string;
}

let default_statement_new_object_mutable () : statement_new_object_mutable = {
  id = "";
  class_ = "";
}

type statement_method_call_mutable = {
  mutable target : string;
  mutable base : string;
  mutable method_ : string;
  mutable arguments : string list;
  mutable class_ : string option;
}

let default_statement_method_call_mutable () : statement_method_call_mutable = {
  target = "";
  base = "";
  method_ = "";
  arguments = [];
  class_ = None;
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
  mutable predicate : string;
  mutable arguments : Ast_types.expression list;
}

let default_statement_fold_mutable () : statement_fold_mutable = {
  predicate = "";
  arguments = [];
}

type statement_unfold_mutable = {
  mutable predicate : string;
  mutable arguments : Ast_types.expression list;
}

let default_statement_unfold_mutable () : statement_unfold_mutable = {
  predicate = "";
  arguments = [];
}

type statement_sequence_mutable = {
  mutable statements : Ast_types.statement list;
}

let default_statement_sequence_mutable () : statement_sequence_mutable = {
  statements = [];
}

type statement_if_then_else_mutable = {
  mutable condition : Ast_types.expression;
  mutable then_ : Ast_types.statement;
  mutable else_ : Ast_types.statement;
}

let default_statement_if_then_else_mutable () : statement_if_then_else_mutable = {
  condition = Ast_types.default_expression ();
  then_ = Ast_types.default_statement ();
  else_ = Ast_types.default_statement ();
}

type statement_while_loop_mutable = {
  mutable condition : Ast_types.expression;
  mutable invariant : Ast_types.formula;
  mutable body : Ast_types.statement;
}

let default_statement_while_loop_mutable () : statement_while_loop_mutable = {
  condition = Ast_types.default_expression ();
  invariant = Ast_types.default_formula ();
  body = Ast_types.default_statement ();
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
  mutable id : string;
  mutable arguments : Ast_types.argument list;
  mutable dynamic : Ast_types.contract;
  mutable static : Ast_types.contract;
  mutable body : Ast_types.statement;
}

let default_method__mutable () : method__mutable = {
  type_ = Ast_types.default_type_ ();
  id = "";
  arguments = [];
  dynamic = Ast_types.default_contract ();
  static = Ast_types.default_contract ();
  body = Ast_types.default_statement ();
}

type class__mutable = {
  mutable id : string;
  mutable super : string;
  mutable fields : Ast_types.class_field list;
  mutable predicates : Ast_types.predicate list;
  mutable methods : Ast_types.method_ list;
}

let default_class__mutable () : class__mutable = {
  id = "";
  super = "";
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


let rec decode_type_ d = 
  let rec loop () = 
    let ret:Ast_types.type_ = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "type_"
      | Some (1, _) -> (Pbrt.Decoder.empty_nested d ; Ast_types.Int)
      | Some (2, _) -> (Pbrt.Decoder.empty_nested d ; Ast_types.Bool)
      | Some (3, _) -> Ast_types.Class (Pbrt.Decoder.string d)
      | Some (4, _) -> (Pbrt.Decoder.empty_nested d ; Ast_types.Top)
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
      v.id <- Pbrt.Decoder.string d; id_is_set := true;
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
      v.id <- Pbrt.Decoder.string d; id_is_set := true;
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

let rec decode_variable d = 
  let rec loop () = 
    let ret:Ast_types.variable = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "variable"
      | Some (1, _) -> (Pbrt.Decoder.empty_nested d ; Ast_types.Result)
      | Some (2, _) -> Ast_types.Id (Pbrt.Decoder.string d)
      | Some (3, _) -> Ast_types.Old (Pbrt.Decoder.string d)
      | Some (4, _) -> (Pbrt.Decoder.empty_nested d ; Ast_types.This)
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_value d = 
  let rec loop () = 
    let ret:Ast_types.value = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "value"
      | Some (1, _) -> Ast_types.Int (Pbrt.Decoder.int32_as_varint d)
      | Some (2, _) -> Ast_types.Bool (Pbrt.Decoder.bool d)
      | Some (3, _) -> Ast_types.Object (Pbrt.Decoder.string d)
      | Some (4, _) -> (Pbrt.Decoder.empty_nested d ; Ast_types.Null)
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
  | 5 -> (Ast_types.And:Ast_types.binary_operator)
  | 6 -> (Ast_types.Or:Ast_types.binary_operator)
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
      | Some (3, _) -> Ast_types.Binary_operation (decode_expression_binary_operation (Pbrt.Decoder.nested d))
      | Some (4, _) -> Ast_types.Binary_comparison (decode_expression_binary_comparison (Pbrt.Decoder.nested d))
      | Some (5, _) -> Ast_types.Field_reference (decode_expression_field_reference (Pbrt.Decoder.nested d))
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
  let right_is_set = ref false in
  let left_is_set = ref false in
  let operator_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.operator <- decode_binary_operator d; operator_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expression_binary_operation), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.left <- decode_expression (Pbrt.Decoder.nested d); left_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expression_binary_operation), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.right <- decode_expression (Pbrt.Decoder.nested d); right_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expression_binary_operation), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !right_is_set then Pbrt.Decoder.missing_field "right" end;
  begin if not !left_is_set then Pbrt.Decoder.missing_field "left" end;
  begin if not !operator_is_set then Pbrt.Decoder.missing_field "operator" end;
  ({
    Ast_types.operator = v.operator;
    Ast_types.left = v.left;
    Ast_types.right = v.right;
  } : Ast_types.expression_binary_operation)

and decode_expression_binary_comparison d =
  let v = default_expression_binary_comparison_mutable () in
  let continue__= ref true in
  let right_is_set = ref false in
  let left_is_set = ref false in
  let comparer_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.comparer <- decode_binary_comparer d; comparer_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expression_binary_comparison), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.left <- decode_expression (Pbrt.Decoder.nested d); left_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expression_binary_comparison), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.right <- decode_expression (Pbrt.Decoder.nested d); right_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expression_binary_comparison), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !right_is_set then Pbrt.Decoder.missing_field "right" end;
  begin if not !left_is_set then Pbrt.Decoder.missing_field "left" end;
  begin if not !comparer_is_set then Pbrt.Decoder.missing_field "comparer" end;
  ({
    Ast_types.comparer = v.comparer;
    Ast_types.left = v.left;
    Ast_types.right = v.right;
  } : Ast_types.expression_binary_comparison)

and decode_expression_field_reference d =
  let v = default_expression_field_reference_mutable () in
  let continue__= ref true in
  let field_is_set = ref false in
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
      v.field <- Pbrt.Decoder.string d; field_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expression_field_reference), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !field_is_set then Pbrt.Decoder.missing_field "field" end;
  begin if not !base_is_set then Pbrt.Decoder.missing_field "base" end;
  ({
    Ast_types.base = v.base;
    Ast_types.field = v.field;
  } : Ast_types.expression_field_reference)

let rec decode_formula_concrete_predicate_check d =
  let v = default_formula_concrete_predicate_check_mutable () in
  let continue__= ref true in
  let predicate_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.arguments <- List.rev v.arguments;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.predicate <- Pbrt.Decoder.string d; predicate_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_predicate_check), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.arguments <- (decode_expression (Pbrt.Decoder.nested d)) :: v.arguments;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_predicate_check), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.class_ <- Some (Pbrt.Decoder.string d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_predicate_check), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !predicate_is_set then Pbrt.Decoder.missing_field "predicate" end;
  ({
    Ast_types.predicate = v.predicate;
    Ast_types.arguments = v.arguments;
    Ast_types.class_ = v.class_;
  } : Ast_types.formula_concrete_predicate_check)

let rec decode_formula_concrete_access_check d =
  let v = default_formula_concrete_access_check_mutable () in
  let continue__= ref true in
  let field_is_set = ref false in
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
      v.field <- Pbrt.Decoder.string d; field_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_access_check), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !field_is_set then Pbrt.Decoder.missing_field "field" end;
  begin if not !base_is_set then Pbrt.Decoder.missing_field "base" end;
  ({
    Ast_types.base = v.base;
    Ast_types.field = v.field;
  } : Ast_types.formula_concrete_access_check)

let rec decode_formula_operator d = 
  match Pbrt.Decoder.int_as_varint d with
  | 1 -> (Ast_types.And:Ast_types.formula_operator)
  | 2 -> (Ast_types.Sep:Ast_types.formula_operator)
  | _ -> Pbrt.Decoder.malformed_variant "formula_operator"

let rec decode_formula_concrete d = 
  let rec loop () = 
    let ret:Ast_types.formula_concrete = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "formula_concrete"
      | Some (1, _) -> Ast_types.Expression (decode_expression (Pbrt.Decoder.nested d))
      | Some (2, _) -> Ast_types.Predicate_check (decode_formula_concrete_predicate_check (Pbrt.Decoder.nested d))
      | Some (3, _) -> Ast_types.Access_check (decode_formula_concrete_access_check (Pbrt.Decoder.nested d))
      | Some (4, _) -> Ast_types.Formula_operation (decode_formula_concrete_formula_operation (Pbrt.Decoder.nested d))
      | Some (5, _) -> Ast_types.If_then_else (decode_formula_concrete_if_then_else (Pbrt.Decoder.nested d))
      | Some (6, _) -> Ast_types.Unfolding_in (decode_formula_concrete_unfolding_in (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_formula_concrete_formula_operation d =
  let v = default_formula_concrete_formula_operation_mutable () in
  let continue__= ref true in
  let right_is_set = ref false in
  let left_is_set = ref false in
  let operator_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.operator <- decode_formula_operator d; operator_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_formula_operation), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.left <- decode_formula_concrete (Pbrt.Decoder.nested d); left_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_formula_operation), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.right <- decode_formula_concrete (Pbrt.Decoder.nested d); right_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_formula_operation), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !right_is_set then Pbrt.Decoder.missing_field "right" end;
  begin if not !left_is_set then Pbrt.Decoder.missing_field "left" end;
  begin if not !operator_is_set then Pbrt.Decoder.missing_field "operator" end;
  ({
    Ast_types.operator = v.operator;
    Ast_types.left = v.left;
    Ast_types.right = v.right;
  } : Ast_types.formula_concrete_formula_operation)

and decode_formula_concrete_if_then_else d =
  let v = default_formula_concrete_if_then_else_mutable () in
  let continue__= ref true in
  let else__is_set = ref false in
  let then__is_set = ref false in
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
      v.then_ <- decode_formula_concrete (Pbrt.Decoder.nested d); then__is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_if_then_else), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.else_ <- decode_formula_concrete (Pbrt.Decoder.nested d); else__is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_concrete_if_then_else), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !else__is_set then Pbrt.Decoder.missing_field "else_" end;
  begin if not !then__is_set then Pbrt.Decoder.missing_field "then_" end;
  begin if not !condition_is_set then Pbrt.Decoder.missing_field "condition" end;
  ({
    Ast_types.condition = v.condition;
    Ast_types.then_ = v.then_;
    Ast_types.else_ = v.else_;
  } : Ast_types.formula_concrete_if_then_else)

and decode_formula_concrete_unfolding_in d =
  let v = default_formula_concrete_unfolding_in_mutable () in
  let continue__= ref true in
  let formula_is_set = ref false in
  let predicate_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.arguments <- List.rev v.arguments;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.predicate <- Pbrt.Decoder.string d; predicate_is_set := true;
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
  begin if not !predicate_is_set then Pbrt.Decoder.missing_field "predicate" end;
  ({
    Ast_types.predicate = v.predicate;
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
  let id_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.arguments <- List.rev v.arguments;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.id <- Pbrt.Decoder.string d; id_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(predicate), field(1)" pk
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
  begin if not !id_is_set then Pbrt.Decoder.missing_field "id" end;
  ({
    Ast_types.id = v.id;
    Ast_types.arguments = v.arguments;
    Ast_types.formula = v.formula;
  } : Ast_types.predicate)

let rec decode_contract d =
  let v = default_contract_mutable () in
  let continue__= ref true in
  let ensures_is_set = ref false in
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
      v.ensures <- decode_formula (Pbrt.Decoder.nested d); ensures_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(contract), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !ensures_is_set then Pbrt.Decoder.missing_field "ensures" end;
  begin if not !requires_is_set then Pbrt.Decoder.missing_field "requires" end;
  ({
    Ast_types.requires = v.requires;
    Ast_types.ensures = v.ensures;
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
      v.id <- Pbrt.Decoder.string d; id_is_set := true;
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
      v.id <- Pbrt.Decoder.string d; id_is_set := true;
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

let rec decode_statement_field_assignment d =
  let v = default_statement_field_assignment_mutable () in
  let continue__= ref true in
  let source_is_set = ref false in
  let field_is_set = ref false in
  let base_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.base <- Pbrt.Decoder.string d; base_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_field_assignment), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.field <- Pbrt.Decoder.string d; field_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_field_assignment), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.source <- Pbrt.Decoder.string d; source_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_field_assignment), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !source_is_set then Pbrt.Decoder.missing_field "source" end;
  begin if not !field_is_set then Pbrt.Decoder.missing_field "field" end;
  begin if not !base_is_set then Pbrt.Decoder.missing_field "base" end;
  ({
    Ast_types.base = v.base;
    Ast_types.field = v.field;
    Ast_types.source = v.source;
  } : Ast_types.statement_field_assignment)

let rec decode_statement_new_object d =
  let v = default_statement_new_object_mutable () in
  let continue__= ref true in
  let class__is_set = ref false in
  let id_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.id <- Pbrt.Decoder.string d; id_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_new_object), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.class_ <- Pbrt.Decoder.string d; class__is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_new_object), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !class__is_set then Pbrt.Decoder.missing_field "class_" end;
  begin if not !id_is_set then Pbrt.Decoder.missing_field "id" end;
  ({
    Ast_types.id = v.id;
    Ast_types.class_ = v.class_;
  } : Ast_types.statement_new_object)

let rec decode_statement_method_call d =
  let v = default_statement_method_call_mutable () in
  let continue__= ref true in
  let method__is_set = ref false in
  let base_is_set = ref false in
  let target_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.arguments <- List.rev v.arguments;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.target <- Pbrt.Decoder.string d; target_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_method_call), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.base <- Pbrt.Decoder.string d; base_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_method_call), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.method_ <- Pbrt.Decoder.string d; method__is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_method_call), field(3)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.arguments <- (Pbrt.Decoder.string d) :: v.arguments;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_method_call), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      v.class_ <- Some (Pbrt.Decoder.string d);
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_method_call), field(6)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !method__is_set then Pbrt.Decoder.missing_field "method_" end;
  begin if not !base_is_set then Pbrt.Decoder.missing_field "base" end;
  begin if not !target_is_set then Pbrt.Decoder.missing_field "target" end;
  ({
    Ast_types.target = v.target;
    Ast_types.base = v.base;
    Ast_types.method_ = v.method_;
    Ast_types.arguments = v.arguments;
    Ast_types.class_ = v.class_;
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
  let predicate_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.arguments <- List.rev v.arguments;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.predicate <- Pbrt.Decoder.string d; predicate_is_set := true;
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
  begin if not !predicate_is_set then Pbrt.Decoder.missing_field "predicate" end;
  ({
    Ast_types.predicate = v.predicate;
    Ast_types.arguments = v.arguments;
  } : Ast_types.statement_fold)

let rec decode_statement_unfold d =
  let v = default_statement_unfold_mutable () in
  let continue__= ref true in
  let predicate_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.arguments <- List.rev v.arguments;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.predicate <- Pbrt.Decoder.string d; predicate_is_set := true;
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
  begin if not !predicate_is_set then Pbrt.Decoder.missing_field "predicate" end;
  ({
    Ast_types.predicate = v.predicate;
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
      | Some (5, _) -> Ast_types.If_then_else (decode_statement_if_then_else (Pbrt.Decoder.nested d))
      | Some (6, _) -> Ast_types.While_loop (decode_statement_while_loop (Pbrt.Decoder.nested d))
      | Some (7, _) -> Ast_types.Field_assignment (decode_statement_field_assignment (Pbrt.Decoder.nested d))
      | Some (8, _) -> Ast_types.New_object (decode_statement_new_object (Pbrt.Decoder.nested d))
      | Some (9, _) -> Ast_types.Method_call (decode_statement_method_call (Pbrt.Decoder.nested d))
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
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.statements <- List.rev v.statements;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.statements <- (decode_statement (Pbrt.Decoder.nested d)) :: v.statements;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_sequence), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Ast_types.statements = v.statements;
  } : Ast_types.statement_sequence)

and decode_statement_if_then_else d =
  let v = default_statement_if_then_else_mutable () in
  let continue__= ref true in
  let else__is_set = ref false in
  let then__is_set = ref false in
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
      v.then_ <- decode_statement (Pbrt.Decoder.nested d); then__is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_if_then_else), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.else_ <- decode_statement (Pbrt.Decoder.nested d); else__is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_if_then_else), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !else__is_set then Pbrt.Decoder.missing_field "else_" end;
  begin if not !then__is_set then Pbrt.Decoder.missing_field "then_" end;
  begin if not !condition_is_set then Pbrt.Decoder.missing_field "condition" end;
  ({
    Ast_types.condition = v.condition;
    Ast_types.then_ = v.then_;
    Ast_types.else_ = v.else_;
  } : Ast_types.statement_if_then_else)

and decode_statement_while_loop d =
  let v = default_statement_while_loop_mutable () in
  let continue__= ref true in
  let body_is_set = ref false in
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
    | Some (3, Pbrt.Bytes) -> begin
      v.body <- decode_statement (Pbrt.Decoder.nested d); body_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_while_loop), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !body_is_set then Pbrt.Decoder.missing_field "body" end;
  begin if not !invariant_is_set then Pbrt.Decoder.missing_field "invariant" end;
  begin if not !condition_is_set then Pbrt.Decoder.missing_field "condition" end;
  ({
    Ast_types.condition = v.condition;
    Ast_types.invariant = v.invariant;
    Ast_types.body = v.body;
  } : Ast_types.statement_while_loop)

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
      v.id <- Pbrt.Decoder.string d; id_is_set := true;
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
  let super_is_set = ref false in
  let id_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.methods <- List.rev v.methods;
      v.predicates <- List.rev v.predicates;
      v.fields <- List.rev v.fields;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.id <- Pbrt.Decoder.string d; id_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(class_), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.super <- Pbrt.Decoder.string d; super_is_set := true;
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
  begin if not !super_is_set then Pbrt.Decoder.missing_field "super" end;
  begin if not !id_is_set then Pbrt.Decoder.missing_field "id" end;
  ({
    Ast_types.id = v.id;
    Ast_types.super = v.super;
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

let rec encode_type_ (v:Ast_types.type_) encoder = 
  begin match v with
  | Ast_types.Int ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Ast_types.Bool ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Ast_types.Class x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | Ast_types.Top ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  end

let rec encode_class_field (v:Ast_types.class_field) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_type_ v.Ast_types.type_) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Ast_types.id encoder;
  ()

let rec encode_argument (v:Ast_types.argument) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_type_ v.Ast_types.type_) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Ast_types.id encoder;
  ()

let rec encode_variable (v:Ast_types.variable) encoder = 
  begin match v with
  | Ast_types.Result ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Ast_types.Id x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | Ast_types.Old x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | Ast_types.This ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  end

let rec encode_value (v:Ast_types.value) encoder = 
  begin match v with
  | Ast_types.Int x ->
    Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
    Pbrt.Encoder.int32_as_varint x encoder;
  | Ast_types.Bool x ->
    Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
    Pbrt.Encoder.bool x encoder;
  | Ast_types.Object x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | Ast_types.Null ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  end

let rec encode_binary_operator (v:Ast_types.binary_operator) encoder =
  match v with
  | Ast_types.Add -> Pbrt.Encoder.int_as_varint 1 encoder
  | Ast_types.Sub -> Pbrt.Encoder.int_as_varint 2 encoder
  | Ast_types.Mul -> Pbrt.Encoder.int_as_varint 3 encoder
  | Ast_types.Div -> Pbrt.Encoder.int_as_varint 4 encoder
  | Ast_types.And -> Pbrt.Encoder.int_as_varint 5 encoder
  | Ast_types.Or -> Pbrt.Encoder.int_as_varint 6 encoder

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
  | Ast_types.Binary_operation x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expression_binary_operation x) encoder;
  | Ast_types.Binary_comparison x ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expression_binary_comparison x) encoder;
  | Ast_types.Field_reference x ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expression_field_reference x) encoder;
  end

and encode_expression_binary_operation (v:Ast_types.expression_binary_operation) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  encode_binary_operator v.Ast_types.operator encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.left) encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.right) encoder;
  ()

and encode_expression_binary_comparison (v:Ast_types.expression_binary_comparison) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  encode_binary_comparer v.Ast_types.comparer encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.left) encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.right) encoder;
  ()

and encode_expression_field_reference (v:Ast_types.expression_field_reference) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.base) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Ast_types.field encoder;
  ()

let rec encode_formula_concrete_predicate_check (v:Ast_types.formula_concrete_predicate_check) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Ast_types.predicate encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expression x) encoder;
  ) v.Ast_types.arguments;
  begin match v.Ast_types.class_ with
  | Some x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  ()

let rec encode_formula_concrete_access_check (v:Ast_types.formula_concrete_access_check) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.base) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Ast_types.field encoder;
  ()

let rec encode_formula_operator (v:Ast_types.formula_operator) encoder =
  match v with
  | Ast_types.And -> Pbrt.Encoder.int_as_varint 1 encoder
  | Ast_types.Sep -> Pbrt.Encoder.int_as_varint 2 encoder

let rec encode_formula_concrete (v:Ast_types.formula_concrete) encoder = 
  begin match v with
  | Ast_types.Expression x ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expression x) encoder;
  | Ast_types.Predicate_check x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_formula_concrete_predicate_check x) encoder;
  | Ast_types.Access_check x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_formula_concrete_access_check x) encoder;
  | Ast_types.Formula_operation x ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_formula_concrete_formula_operation x) encoder;
  | Ast_types.If_then_else x ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_formula_concrete_if_then_else x) encoder;
  | Ast_types.Unfolding_in x ->
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_formula_concrete_unfolding_in x) encoder;
  end

and encode_formula_concrete_formula_operation (v:Ast_types.formula_concrete_formula_operation) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  encode_formula_operator v.Ast_types.operator encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula_concrete v.Ast_types.left) encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula_concrete v.Ast_types.right) encoder;
  ()

and encode_formula_concrete_if_then_else (v:Ast_types.formula_concrete_if_then_else) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.condition) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula_concrete v.Ast_types.then_) encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula_concrete v.Ast_types.else_) encoder;
  ()

and encode_formula_concrete_unfolding_in (v:Ast_types.formula_concrete_unfolding_in) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Ast_types.predicate encoder;
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
  Pbrt.Encoder.string v.Ast_types.id encoder;
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
  Pbrt.Encoder.nested (encode_formula v.Ast_types.ensures) encoder;
  ()

let rec encode_statement_declaration (v:Ast_types.statement_declaration) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_type_ v.Ast_types.type_) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Ast_types.id encoder;
  ()

let rec encode_statement_assignment (v:Ast_types.statement_assignment) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Ast_types.id encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.value) encoder;
  ()

let rec encode_statement_field_assignment (v:Ast_types.statement_field_assignment) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Ast_types.base encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Ast_types.field encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Ast_types.source encoder;
  ()

let rec encode_statement_new_object (v:Ast_types.statement_new_object) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Ast_types.id encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Ast_types.class_ encoder;
  ()

let rec encode_statement_method_call (v:Ast_types.statement_method_call) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Ast_types.target encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Ast_types.base encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Ast_types.method_ encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  ) v.Ast_types.arguments;
  begin match v.Ast_types.class_ with
  | Some x -> 
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
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
  Pbrt.Encoder.string v.Ast_types.predicate encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expression x) encoder;
  ) v.Ast_types.arguments;
  ()

let rec encode_statement_unfold (v:Ast_types.statement_unfold) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Ast_types.predicate encoder;
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
  | Ast_types.If_then_else x ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_if_then_else x) encoder;
  | Ast_types.While_loop x ->
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_while_loop x) encoder;
  | Ast_types.Field_assignment x ->
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_field_assignment x) encoder;
  | Ast_types.New_object x ->
    Pbrt.Encoder.key (8, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_new_object x) encoder;
  | Ast_types.Method_call x ->
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
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement x) encoder;
  ) v.Ast_types.statements;
  ()

and encode_statement_if_then_else (v:Ast_types.statement_if_then_else) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.condition) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_statement v.Ast_types.then_) encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_statement v.Ast_types.else_) encoder;
  ()

and encode_statement_while_loop (v:Ast_types.statement_while_loop) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.condition) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula v.Ast_types.invariant) encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_statement v.Ast_types.body) encoder;
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
  Pbrt.Encoder.string v.Ast_types.id encoder;
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
  Pbrt.Encoder.string v.Ast_types.id encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Ast_types.super encoder;
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
