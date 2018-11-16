[@@@ocaml.warning "-27-30-39"]

type identifier_mutable = {
  mutable name : string;
}

let default_identifier_mutable () : identifier_mutable = {
  name = "";
}

type expression_binop_mutable = {
  mutable left : Ast_types.expression;
  mutable oper : Ast_types.expop;
  mutable right : Ast_types.expression;
}

let default_expression_binop_mutable () : expression_binop_mutable = {
  left = Ast_types.default_expression ();
  oper = Ast_types.default_expop ();
  right = Ast_types.default_expression ();
}

type expression_field_acc_mutable = {
  mutable base : Ast_types.expression;
  mutable fieldname : Ast_types.identifier;
}

let default_expression_field_acc_mutable () : expression_field_acc_mutable = {
  base = Ast_types.default_expression ();
  fieldname = Ast_types.default_identifier ();
}

type formula_compare_mutable = {
  mutable left : Ast_types.expression;
  mutable oper : Ast_types.cmpop;
  mutable right : Ast_types.expression;
}

let default_formula_compare_mutable () : formula_compare_mutable = {
  left = Ast_types.default_expression ();
  oper = Ast_types.default_cmpop ();
  right = Ast_types.default_expression ();
}

type formula_alpha_mutable = {
  mutable clsname : Ast_types.identifier;
  mutable arg : Ast_types.expression;
}

let default_formula_alpha_mutable () : formula_alpha_mutable = {
  clsname = Ast_types.default_identifier ();
  arg = Ast_types.default_expression ();
}

type formula_access_mutable = {
  mutable base : Ast_types.expression list;
  mutable fieldname : Ast_types.identifier;
}

let default_formula_access_mutable () : formula_access_mutable = {
  base = [];
  fieldname = Ast_types.default_identifier ();
}

type formula_separate_mutable = {
  mutable left : Ast_types.formula;
  mutable right : Ast_types.formula;
}

let default_formula_separate_mutable () : formula_separate_mutable = {
  left = Ast_types.default_formula ();
  right = Ast_types.default_formula ();
}

type contract_mutable = {
  mutable requires : Ast_types.phi;
  mutable ensures : Ast_types.phi;
}

let default_contract_mutable () : contract_mutable = {
  requires = Ast_types.default_phi ();
  ensures = Ast_types.default_phi ();
}

type statement_assign_mutable = {
  mutable t : Ast_types.type_;
  mutable name : Ast_types.identifier;
  mutable value : Ast_types.expression;
}

let default_statement_assign_mutable () : statement_assign_mutable = {
  t = Ast_types.default_type_ ();
  name = Ast_types.default_identifier ();
  value = Ast_types.default_expression ();
}

type statement_field_assign_mutable = {
  mutable base : Ast_types.identifier;
  mutable fieldname : Ast_types.identifier;
  mutable source : Ast_types.identifier;
}

let default_statement_field_assign_mutable () : statement_field_assign_mutable = {
  base = Ast_types.default_identifier ();
  fieldname = Ast_types.default_identifier ();
  source = Ast_types.default_identifier ();
}

type statement_new_mutable = {
  mutable name : Ast_types.identifier;
  mutable classname : Ast_types.identifier;
}

let default_statement_new_mutable () : statement_new_mutable = {
  name = Ast_types.default_identifier ();
  classname = Ast_types.default_identifier ();
}

type statement_method_call_mutable = {
  mutable target : Ast_types.identifier;
  mutable base : Ast_types.identifier;
  mutable methodname : Ast_types.identifier;
  mutable args : Ast_types.identifier list;
}

let default_statement_method_call_mutable () : statement_method_call_mutable = {
  target = Ast_types.default_identifier ();
  base = Ast_types.default_identifier ();
  methodname = Ast_types.default_identifier ();
  args = [];
}

type statement_seq_mutable = {
  mutable prev : Ast_types.statement;
  mutable next : Ast_types.statement;
}

let default_statement_seq_mutable () : statement_seq_mutable = {
  prev = Ast_types.default_statement ();
  next = Ast_types.default_statement ();
}

type statement_if_mutable = {
  mutable left : Ast_types.identifier;
  mutable oper : Ast_types.cmpop;
  mutable right : Ast_types.identifier;
  mutable thenclause : Ast_types.statement;
  mutable elseclause : Ast_types.statement;
}

let default_statement_if_mutable () : statement_if_mutable = {
  left = Ast_types.default_identifier ();
  oper = Ast_types.default_cmpop ();
  right = Ast_types.default_identifier ();
  thenclause = Ast_types.default_statement ();
  elseclause = Ast_types.default_statement ();
}

type statement_hold_mutable = {
  mutable invariant : Ast_types.formula;
  mutable body : Ast_types.statement;
}

let default_statement_hold_mutable () : statement_hold_mutable = {
  invariant = Ast_types.default_formula ();
  body = Ast_types.default_statement ();
}

type abs_pred_defn_mutable = {
  mutable name : Ast_types.identifier;
  mutable args : Ast_types.identifier list;
  mutable body : Ast_types.phi;
}

let default_abs_pred_defn_mutable () : abs_pred_defn_mutable = {
  name = Ast_types.default_identifier ();
  args = [];
  body = Ast_types.default_phi ();
}

type method_argument_mutable = {
  mutable t : Ast_types.type_;
  mutable name : Ast_types.identifier;
}

let default_method_argument_mutable () : method_argument_mutable = {
  t = Ast_types.default_type_ ();
  name = Ast_types.default_identifier ();
}

type method__mutable = {
  mutable name : Ast_types.identifier;
  mutable out_type : Ast_types.type_;
  mutable args : Ast_types.method_argument list;
  mutable dynamic : Ast_types.contract;
  mutable static : Ast_types.contract;
  mutable body : Ast_types.statement;
}

let default_method__mutable () : method__mutable = {
  name = Ast_types.default_identifier ();
  out_type = Ast_types.default_type_ ();
  args = [];
  dynamic = Ast_types.default_contract ();
  static = Ast_types.default_contract ();
  body = Ast_types.default_statement ();
}

type class_field_mutable = {
  mutable t : Ast_types.type_;
  mutable name : Ast_types.identifier;
}

let default_class_field_mutable () : class_field_mutable = {
  t = Ast_types.default_type_ ();
  name = Ast_types.default_identifier ();
}

type class__mutable = {
  mutable name : Ast_types.identifier;
  mutable super : Ast_types.identifier;
  mutable fields : Ast_types.class_field list;
  mutable abspreds : Ast_types.abs_pred_defn list;
  mutable methods : Ast_types.method_ list;
}

let default_class__mutable () : class__mutable = {
  name = Ast_types.default_identifier ();
  super = Ast_types.default_identifier ();
  fields = [];
  abspreds = [];
  methods = [];
}

type program_mutable = {
  mutable classes : Ast_types.class_ list;
  mutable stmts : Ast_types.statement list;
}

let default_program_mutable () : program_mutable = {
  classes = [];
  stmts = [];
}


let rec decode_identifier d =
  let v = default_identifier_mutable () in
  let continue__= ref true in
  let name_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d; name_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(identifier), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !name_is_set then Pbrt.Decoder.missing_field "name" end;
  ({
    Ast_types.name = v.name;
  } : Ast_types.identifier)

let rec decode_type_ d = 
  let rec loop () = 
    let ret:Ast_types.type_ = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "type_"
      | Some (1, _) -> Ast_types.Cls (decode_identifier (Pbrt.Decoder.nested d))
      | Some (2, _) -> (Pbrt.Decoder.empty_nested d ; Ast_types.Int)
      | Some (3, _) -> (Pbrt.Decoder.empty_nested d ; Ast_types.Top)
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_expop d = 
  match Pbrt.Decoder.int_as_varint d with
  | 1 -> (Ast_types.Plus:Ast_types.expop)
  | 2 -> (Ast_types.Minus:Ast_types.expop)
  | 3 -> (Ast_types.Times:Ast_types.expop)
  | 4 -> (Ast_types.Div:Ast_types.expop)
  | _ -> Pbrt.Decoder.malformed_variant "expop"

let rec decode_cmpop d = 
  match Pbrt.Decoder.int_as_varint d with
  | 1 -> (Ast_types.Neq:Ast_types.cmpop)
  | 2 -> (Ast_types.Eq:Ast_types.cmpop)
  | 3 -> (Ast_types.Lt:Ast_types.cmpop)
  | 4 -> (Ast_types.Gt:Ast_types.cmpop)
  | 5 -> (Ast_types.Le:Ast_types.cmpop)
  | 6 -> (Ast_types.Ge:Ast_types.cmpop)
  | _ -> Pbrt.Decoder.malformed_variant "cmpop"

let rec decode_val_ d = 
  let rec loop () = 
    let ret:Ast_types.val_ = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "val_"
      | Some (1, _) -> (Pbrt.Decoder.empty_nested d ; Ast_types.Nil)
      | Some (2, _) -> Ast_types.Num (Pbrt.Decoder.int32_as_varint d)
      | Some (3, _) -> (Pbrt.Decoder.empty_nested d ; Ast_types.Cls)
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_expression_binop d =
  let v = default_expression_binop_mutable () in
  let continue__= ref true in
  let right_is_set = ref false in
  let oper_is_set = ref false in
  let left_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.left <- decode_expression (Pbrt.Decoder.nested d); left_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expression_binop), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.oper <- decode_expop d; oper_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expression_binop), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.right <- decode_expression (Pbrt.Decoder.nested d); right_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expression_binop), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !right_is_set then Pbrt.Decoder.missing_field "right" end;
  begin if not !oper_is_set then Pbrt.Decoder.missing_field "oper" end;
  begin if not !left_is_set then Pbrt.Decoder.missing_field "left" end;
  ({
    Ast_types.left = v.left;
    Ast_types.oper = v.oper;
    Ast_types.right = v.right;
  } : Ast_types.expression_binop)

and decode_expression d = 
  let rec loop () = 
    let ret:Ast_types.expression = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "expression"
      | Some (1, _) -> Ast_types.Binop (decode_expression_binop (Pbrt.Decoder.nested d))
      | Some (2, _) -> Ast_types.Fieldaccess (decode_expression_field_acc (Pbrt.Decoder.nested d))
      | Some (3, _) -> Ast_types.Val (decode_val_ (Pbrt.Decoder.nested d))
      | Some (4, _) -> Ast_types.Var (decode_identifier (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_expression_field_acc d =
  let v = default_expression_field_acc_mutable () in
  let continue__= ref true in
  let fieldname_is_set = ref false in
  let base_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.base <- decode_expression (Pbrt.Decoder.nested d); base_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expression_field_acc), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.fieldname <- decode_identifier (Pbrt.Decoder.nested d); fieldname_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expression_field_acc), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !fieldname_is_set then Pbrt.Decoder.missing_field "fieldname" end;
  begin if not !base_is_set then Pbrt.Decoder.missing_field "base" end;
  ({
    Ast_types.base = v.base;
    Ast_types.fieldname = v.fieldname;
  } : Ast_types.expression_field_acc)

let rec decode_formula_compare d =
  let v = default_formula_compare_mutable () in
  let continue__= ref true in
  let right_is_set = ref false in
  let oper_is_set = ref false in
  let left_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.left <- decode_expression (Pbrt.Decoder.nested d); left_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_compare), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.oper <- decode_cmpop d; oper_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_compare), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.right <- decode_expression (Pbrt.Decoder.nested d); right_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_compare), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !right_is_set then Pbrt.Decoder.missing_field "right" end;
  begin if not !oper_is_set then Pbrt.Decoder.missing_field "oper" end;
  begin if not !left_is_set then Pbrt.Decoder.missing_field "left" end;
  ({
    Ast_types.left = v.left;
    Ast_types.oper = v.oper;
    Ast_types.right = v.right;
  } : Ast_types.formula_compare)

let rec decode_formula_alpha d =
  let v = default_formula_alpha_mutable () in
  let continue__= ref true in
  let arg_is_set = ref false in
  let clsname_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.clsname <- decode_identifier (Pbrt.Decoder.nested d); clsname_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_alpha), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.arg <- decode_expression (Pbrt.Decoder.nested d); arg_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_alpha), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !arg_is_set then Pbrt.Decoder.missing_field "arg" end;
  begin if not !clsname_is_set then Pbrt.Decoder.missing_field "clsname" end;
  ({
    Ast_types.clsname = v.clsname;
    Ast_types.arg = v.arg;
  } : Ast_types.formula_alpha)

let rec decode_formula_access d =
  let v = default_formula_access_mutable () in
  let continue__= ref true in
  let fieldname_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.base <- List.rev v.base;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.base <- (decode_expression (Pbrt.Decoder.nested d)) :: v.base;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_access), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.fieldname <- decode_identifier (Pbrt.Decoder.nested d); fieldname_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_access), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !fieldname_is_set then Pbrt.Decoder.missing_field "fieldname" end;
  ({
    Ast_types.base = v.base;
    Ast_types.fieldname = v.fieldname;
  } : Ast_types.formula_access)

let rec decode_formula_separate d =
  let v = default_formula_separate_mutable () in
  let continue__= ref true in
  let right_is_set = ref false in
  let left_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.left <- decode_formula (Pbrt.Decoder.nested d); left_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_separate), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.right <- decode_formula (Pbrt.Decoder.nested d); right_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(formula_separate), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !right_is_set then Pbrt.Decoder.missing_field "right" end;
  begin if not !left_is_set then Pbrt.Decoder.missing_field "left" end;
  ({
    Ast_types.left = v.left;
    Ast_types.right = v.right;
  } : Ast_types.formula_separate)

and decode_formula d = 
  let rec loop () = 
    let ret:Ast_types.formula = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "formula"
      | Some (1, _) -> Ast_types.Cmpf (decode_formula_compare (Pbrt.Decoder.nested d))
      | Some (2, _) -> Ast_types.Alpha (decode_formula_alpha (Pbrt.Decoder.nested d))
      | Some (3, _) -> Ast_types.Access (decode_formula_access (Pbrt.Decoder.nested d))
      | Some (4, _) -> Ast_types.Sep (decode_formula_separate (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_phi d = 
  let rec loop () = 
    let ret:Ast_types.phi = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "phi"
      | Some (1, _) -> Ast_types.Concrete (decode_formula (Pbrt.Decoder.nested d))
      | Some (2, _) -> Ast_types.Grad (decode_formula (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

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
      v.requires <- decode_phi (Pbrt.Decoder.nested d); requires_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(contract), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.ensures <- decode_phi (Pbrt.Decoder.nested d); ensures_is_set := true;
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

let rec decode_statement_assign d =
  let v = default_statement_assign_mutable () in
  let continue__= ref true in
  let value_is_set = ref false in
  let name_is_set = ref false in
  let t_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.t <- decode_type_ (Pbrt.Decoder.nested d); t_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_assign), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.name <- decode_identifier (Pbrt.Decoder.nested d); name_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_assign), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.value <- decode_expression (Pbrt.Decoder.nested d); value_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_assign), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !value_is_set then Pbrt.Decoder.missing_field "value" end;
  begin if not !name_is_set then Pbrt.Decoder.missing_field "name" end;
  begin if not !t_is_set then Pbrt.Decoder.missing_field "t" end;
  ({
    Ast_types.t = v.t;
    Ast_types.name = v.name;
    Ast_types.value = v.value;
  } : Ast_types.statement_assign)

let rec decode_statement_field_assign d =
  let v = default_statement_field_assign_mutable () in
  let continue__= ref true in
  let source_is_set = ref false in
  let fieldname_is_set = ref false in
  let base_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.base <- decode_identifier (Pbrt.Decoder.nested d); base_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_field_assign), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.fieldname <- decode_identifier (Pbrt.Decoder.nested d); fieldname_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_field_assign), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.source <- decode_identifier (Pbrt.Decoder.nested d); source_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_field_assign), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !source_is_set then Pbrt.Decoder.missing_field "source" end;
  begin if not !fieldname_is_set then Pbrt.Decoder.missing_field "fieldname" end;
  begin if not !base_is_set then Pbrt.Decoder.missing_field "base" end;
  ({
    Ast_types.base = v.base;
    Ast_types.fieldname = v.fieldname;
    Ast_types.source = v.source;
  } : Ast_types.statement_field_assign)

let rec decode_statement_new d =
  let v = default_statement_new_mutable () in
  let continue__= ref true in
  let classname_is_set = ref false in
  let name_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.name <- decode_identifier (Pbrt.Decoder.nested d); name_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_new), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.classname <- decode_identifier (Pbrt.Decoder.nested d); classname_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_new), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !classname_is_set then Pbrt.Decoder.missing_field "classname" end;
  begin if not !name_is_set then Pbrt.Decoder.missing_field "name" end;
  ({
    Ast_types.name = v.name;
    Ast_types.classname = v.classname;
  } : Ast_types.statement_new)

let rec decode_statement_method_call d =
  let v = default_statement_method_call_mutable () in
  let continue__= ref true in
  let methodname_is_set = ref false in
  let base_is_set = ref false in
  let target_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.args <- List.rev v.args;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.target <- decode_identifier (Pbrt.Decoder.nested d); target_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_method_call), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.base <- decode_identifier (Pbrt.Decoder.nested d); base_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_method_call), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.methodname <- decode_identifier (Pbrt.Decoder.nested d); methodname_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_method_call), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.args <- (decode_identifier (Pbrt.Decoder.nested d)) :: v.args;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_method_call), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !methodname_is_set then Pbrt.Decoder.missing_field "methodname" end;
  begin if not !base_is_set then Pbrt.Decoder.missing_field "base" end;
  begin if not !target_is_set then Pbrt.Decoder.missing_field "target" end;
  ({
    Ast_types.target = v.target;
    Ast_types.base = v.base;
    Ast_types.methodname = v.methodname;
    Ast_types.args = v.args;
  } : Ast_types.statement_method_call)

let rec decode_statement_seq d =
  let v = default_statement_seq_mutable () in
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
      Pbrt.Decoder.unexpected_payload "Message(statement_seq), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.next <- decode_statement (Pbrt.Decoder.nested d); next_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_seq), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !next_is_set then Pbrt.Decoder.missing_field "next" end;
  begin if not !prev_is_set then Pbrt.Decoder.missing_field "prev" end;
  ({
    Ast_types.prev = v.prev;
    Ast_types.next = v.next;
  } : Ast_types.statement_seq)

and decode_statement d = 
  let rec loop () = 
    let ret:Ast_types.statement = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "statement"
      | Some (1, _) -> (Pbrt.Decoder.empty_nested d ; Ast_types.Skip)
      | Some (2, _) -> Ast_types.Seq (decode_statement_seq (Pbrt.Decoder.nested d))
      | Some (5, _) -> Ast_types.Assign (decode_statement_assign (Pbrt.Decoder.nested d))
      | Some (6, _) -> Ast_types.Ifthen (decode_statement_if (Pbrt.Decoder.nested d))
      | Some (7, _) -> Ast_types.Fieldasgn (decode_statement_field_assign (Pbrt.Decoder.nested d))
      | Some (8, _) -> Ast_types.New_obj (decode_statement_new (Pbrt.Decoder.nested d))
      | Some (9, _) -> Ast_types.Call (decode_statement_method_call (Pbrt.Decoder.nested d))
      | Some (10, _) -> Ast_types.Assert (decode_formula (Pbrt.Decoder.nested d))
      | Some (11, _) -> Ast_types.Release (decode_formula (Pbrt.Decoder.nested d))
      | Some (12, _) -> Ast_types.Hold (decode_statement_hold (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_statement_if d =
  let v = default_statement_if_mutable () in
  let continue__= ref true in
  let elseclause_is_set = ref false in
  let thenclause_is_set = ref false in
  let right_is_set = ref false in
  let oper_is_set = ref false in
  let left_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.left <- decode_identifier (Pbrt.Decoder.nested d); left_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_if), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.oper <- decode_cmpop d; oper_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_if), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.right <- decode_identifier (Pbrt.Decoder.nested d); right_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_if), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.thenclause <- decode_statement (Pbrt.Decoder.nested d); thenclause_is_set := true;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_if), field(4)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.elseclause <- decode_statement (Pbrt.Decoder.nested d); elseclause_is_set := true;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(statement_if), field(5)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !elseclause_is_set then Pbrt.Decoder.missing_field "elseclause" end;
  begin if not !thenclause_is_set then Pbrt.Decoder.missing_field "thenclause" end;
  begin if not !right_is_set then Pbrt.Decoder.missing_field "right" end;
  begin if not !oper_is_set then Pbrt.Decoder.missing_field "oper" end;
  begin if not !left_is_set then Pbrt.Decoder.missing_field "left" end;
  ({
    Ast_types.left = v.left;
    Ast_types.oper = v.oper;
    Ast_types.right = v.right;
    Ast_types.thenclause = v.thenclause;
    Ast_types.elseclause = v.elseclause;
  } : Ast_types.statement_if)

and decode_statement_hold d =
  let v = default_statement_hold_mutable () in
  let continue__= ref true in
  let body_is_set = ref false in
  let invariant_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.invariant <- decode_formula (Pbrt.Decoder.nested d); invariant_is_set := true;
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
  begin if not !invariant_is_set then Pbrt.Decoder.missing_field "invariant" end;
  ({
    Ast_types.invariant = v.invariant;
    Ast_types.body = v.body;
  } : Ast_types.statement_hold)

let rec decode_abs_pred_defn d =
  let v = default_abs_pred_defn_mutable () in
  let continue__= ref true in
  let body_is_set = ref false in
  let name_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.args <- List.rev v.args;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.name <- decode_identifier (Pbrt.Decoder.nested d); name_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(abs_pred_defn), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.args <- (decode_identifier (Pbrt.Decoder.nested d)) :: v.args;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(abs_pred_defn), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.body <- decode_phi (Pbrt.Decoder.nested d); body_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(abs_pred_defn), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !body_is_set then Pbrt.Decoder.missing_field "body" end;
  begin if not !name_is_set then Pbrt.Decoder.missing_field "name" end;
  ({
    Ast_types.name = v.name;
    Ast_types.args = v.args;
    Ast_types.body = v.body;
  } : Ast_types.abs_pred_defn)

let rec decode_method_argument d =
  let v = default_method_argument_mutable () in
  let continue__= ref true in
  let name_is_set = ref false in
  let t_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.t <- decode_type_ (Pbrt.Decoder.nested d); t_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(method_argument), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.name <- decode_identifier (Pbrt.Decoder.nested d); name_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(method_argument), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !name_is_set then Pbrt.Decoder.missing_field "name" end;
  begin if not !t_is_set then Pbrt.Decoder.missing_field "t" end;
  ({
    Ast_types.t = v.t;
    Ast_types.name = v.name;
  } : Ast_types.method_argument)

let rec decode_method_ d =
  let v = default_method__mutable () in
  let continue__= ref true in
  let body_is_set = ref false in
  let static_is_set = ref false in
  let dynamic_is_set = ref false in
  let out_type_is_set = ref false in
  let name_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.args <- List.rev v.args;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.name <- decode_identifier (Pbrt.Decoder.nested d); name_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(method_), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.out_type <- decode_type_ (Pbrt.Decoder.nested d); out_type_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(method_), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.args <- (decode_method_argument (Pbrt.Decoder.nested d)) :: v.args;
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
  begin if not !out_type_is_set then Pbrt.Decoder.missing_field "out_type" end;
  begin if not !name_is_set then Pbrt.Decoder.missing_field "name" end;
  ({
    Ast_types.name = v.name;
    Ast_types.out_type = v.out_type;
    Ast_types.args = v.args;
    Ast_types.dynamic = v.dynamic;
    Ast_types.static = v.static;
    Ast_types.body = v.body;
  } : Ast_types.method_)

let rec decode_class_field d =
  let v = default_class_field_mutable () in
  let continue__= ref true in
  let name_is_set = ref false in
  let t_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.t <- decode_type_ (Pbrt.Decoder.nested d); t_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(class_field), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.name <- decode_identifier (Pbrt.Decoder.nested d); name_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(class_field), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !name_is_set then Pbrt.Decoder.missing_field "name" end;
  begin if not !t_is_set then Pbrt.Decoder.missing_field "t" end;
  ({
    Ast_types.t = v.t;
    Ast_types.name = v.name;
  } : Ast_types.class_field)

let rec decode_class_ d =
  let v = default_class__mutable () in
  let continue__= ref true in
  let super_is_set = ref false in
  let name_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.methods <- List.rev v.methods;
      v.abspreds <- List.rev v.abspreds;
      v.fields <- List.rev v.fields;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.name <- decode_identifier (Pbrt.Decoder.nested d); name_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(class_), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.super <- decode_identifier (Pbrt.Decoder.nested d); super_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(class_), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.fields <- (decode_class_field (Pbrt.Decoder.nested d)) :: v.fields;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(class_), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.abspreds <- (decode_abs_pred_defn (Pbrt.Decoder.nested d)) :: v.abspreds;
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
  begin if not !name_is_set then Pbrt.Decoder.missing_field "name" end;
  ({
    Ast_types.name = v.name;
    Ast_types.super = v.super;
    Ast_types.fields = v.fields;
    Ast_types.abspreds = v.abspreds;
    Ast_types.methods = v.methods;
  } : Ast_types.class_)

let rec decode_program d =
  let v = default_program_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.stmts <- List.rev v.stmts;
      v.classes <- List.rev v.classes;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.classes <- (decode_class_ (Pbrt.Decoder.nested d)) :: v.classes;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(program), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.stmts <- (decode_statement (Pbrt.Decoder.nested d)) :: v.stmts;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(program), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Ast_types.classes = v.classes;
    Ast_types.stmts = v.stmts;
  } : Ast_types.program)

let rec encode_identifier (v:Ast_types.identifier) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Ast_types.name encoder;
  ()

let rec encode_type_ (v:Ast_types.type_) encoder = 
  begin match v with
  | Ast_types.Cls x ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_identifier x) encoder;
  | Ast_types.Int ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Ast_types.Top ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  end

let rec encode_expop (v:Ast_types.expop) encoder =
  match v with
  | Ast_types.Plus -> Pbrt.Encoder.int_as_varint 1 encoder
  | Ast_types.Minus -> Pbrt.Encoder.int_as_varint 2 encoder
  | Ast_types.Times -> Pbrt.Encoder.int_as_varint 3 encoder
  | Ast_types.Div -> Pbrt.Encoder.int_as_varint 4 encoder

let rec encode_cmpop (v:Ast_types.cmpop) encoder =
  match v with
  | Ast_types.Neq -> Pbrt.Encoder.int_as_varint 1 encoder
  | Ast_types.Eq -> Pbrt.Encoder.int_as_varint 2 encoder
  | Ast_types.Lt -> Pbrt.Encoder.int_as_varint 3 encoder
  | Ast_types.Gt -> Pbrt.Encoder.int_as_varint 4 encoder
  | Ast_types.Le -> Pbrt.Encoder.int_as_varint 5 encoder
  | Ast_types.Ge -> Pbrt.Encoder.int_as_varint 6 encoder

let rec encode_val_ (v:Ast_types.val_) encoder = 
  begin match v with
  | Ast_types.Nil ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Ast_types.Num x ->
    Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
    Pbrt.Encoder.int32_as_varint x encoder;
  | Ast_types.Cls ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  end

let rec encode_expression_binop (v:Ast_types.expression_binop) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.left) encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  encode_expop v.Ast_types.oper encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.right) encoder;
  ()

and encode_expression (v:Ast_types.expression) encoder = 
  begin match v with
  | Ast_types.Binop x ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expression_binop x) encoder;
  | Ast_types.Fieldaccess x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expression_field_acc x) encoder;
  | Ast_types.Val x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_val_ x) encoder;
  | Ast_types.Var x ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_identifier x) encoder;
  end

and encode_expression_field_acc (v:Ast_types.expression_field_acc) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.base) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_identifier v.Ast_types.fieldname) encoder;
  ()

let rec encode_formula_compare (v:Ast_types.formula_compare) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.left) encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  encode_cmpop v.Ast_types.oper encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.right) encoder;
  ()

let rec encode_formula_alpha (v:Ast_types.formula_alpha) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_identifier v.Ast_types.clsname) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.arg) encoder;
  ()

let rec encode_formula_access (v:Ast_types.formula_access) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expression x) encoder;
  ) v.Ast_types.base;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_identifier v.Ast_types.fieldname) encoder;
  ()

let rec encode_formula_separate (v:Ast_types.formula_separate) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula v.Ast_types.left) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula v.Ast_types.right) encoder;
  ()

and encode_formula (v:Ast_types.formula) encoder = 
  begin match v with
  | Ast_types.Cmpf x ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_formula_compare x) encoder;
  | Ast_types.Alpha x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_formula_alpha x) encoder;
  | Ast_types.Access x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_formula_access x) encoder;
  | Ast_types.Sep x ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_formula_separate x) encoder;
  end

let rec encode_phi (v:Ast_types.phi) encoder = 
  begin match v with
  | Ast_types.Concrete x ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_formula x) encoder;
  | Ast_types.Grad x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_formula x) encoder;
  end

let rec encode_contract (v:Ast_types.contract) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_phi v.Ast_types.requires) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_phi v.Ast_types.ensures) encoder;
  ()

let rec encode_statement_assign (v:Ast_types.statement_assign) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_type_ v.Ast_types.t) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_identifier v.Ast_types.name) encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expression v.Ast_types.value) encoder;
  ()

let rec encode_statement_field_assign (v:Ast_types.statement_field_assign) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_identifier v.Ast_types.base) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_identifier v.Ast_types.fieldname) encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_identifier v.Ast_types.source) encoder;
  ()

let rec encode_statement_new (v:Ast_types.statement_new) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_identifier v.Ast_types.name) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_identifier v.Ast_types.classname) encoder;
  ()

let rec encode_statement_method_call (v:Ast_types.statement_method_call) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_identifier v.Ast_types.target) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_identifier v.Ast_types.base) encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_identifier v.Ast_types.methodname) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_identifier x) encoder;
  ) v.Ast_types.args;
  ()

let rec encode_statement_seq (v:Ast_types.statement_seq) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_statement v.Ast_types.prev) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_statement v.Ast_types.next) encoder;
  ()

and encode_statement (v:Ast_types.statement) encoder = 
  begin match v with
  | Ast_types.Skip ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Ast_types.Seq x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_seq x) encoder;
  | Ast_types.Assign x ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_assign x) encoder;
  | Ast_types.Ifthen x ->
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_if x) encoder;
  | Ast_types.Fieldasgn x ->
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_field_assign x) encoder;
  | Ast_types.New_obj x ->
    Pbrt.Encoder.key (8, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_new x) encoder;
  | Ast_types.Call x ->
    Pbrt.Encoder.key (9, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_method_call x) encoder;
  | Ast_types.Assert x ->
    Pbrt.Encoder.key (10, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_formula x) encoder;
  | Ast_types.Release x ->
    Pbrt.Encoder.key (11, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_formula x) encoder;
  | Ast_types.Hold x ->
    Pbrt.Encoder.key (12, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement_hold x) encoder;
  end

and encode_statement_if (v:Ast_types.statement_if) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_identifier v.Ast_types.left) encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  encode_cmpop v.Ast_types.oper encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_identifier v.Ast_types.right) encoder;
  Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_statement v.Ast_types.thenclause) encoder;
  Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_statement v.Ast_types.elseclause) encoder;
  ()

and encode_statement_hold (v:Ast_types.statement_hold) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_formula v.Ast_types.invariant) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_statement v.Ast_types.body) encoder;
  ()

let rec encode_abs_pred_defn (v:Ast_types.abs_pred_defn) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_identifier v.Ast_types.name) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_identifier x) encoder;
  ) v.Ast_types.args;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_phi v.Ast_types.body) encoder;
  ()

let rec encode_method_argument (v:Ast_types.method_argument) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_type_ v.Ast_types.t) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_identifier v.Ast_types.name) encoder;
  ()

let rec encode_method_ (v:Ast_types.method_) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_identifier v.Ast_types.name) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_type_ v.Ast_types.out_type) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_method_argument x) encoder;
  ) v.Ast_types.args;
  Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_contract v.Ast_types.dynamic) encoder;
  Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_contract v.Ast_types.static) encoder;
  Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_statement v.Ast_types.body) encoder;
  ()

let rec encode_class_field (v:Ast_types.class_field) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_type_ v.Ast_types.t) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_identifier v.Ast_types.name) encoder;
  ()

let rec encode_class_ (v:Ast_types.class_) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_identifier v.Ast_types.name) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_identifier v.Ast_types.super) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_class_field x) encoder;
  ) v.Ast_types.fields;
  List.iter (fun x -> 
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_abs_pred_defn x) encoder;
  ) v.Ast_types.abspreds;
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
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_statement x) encoder;
  ) v.Ast_types.stmts;
  ()
