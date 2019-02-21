(** ast.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_identifier : Ast_types.identifier -> Pbrt.Encoder.t -> unit
(** [encode_identifier v encoder] encodes [v] with the given [encoder] *)

val encode_type_ : Ast_types.type_ -> Pbrt.Encoder.t -> unit
(** [encode_type_ v encoder] encodes [v] with the given [encoder] *)

val encode_expop : Ast_types.expop -> Pbrt.Encoder.t -> unit
(** [encode_expop v encoder] encodes [v] with the given [encoder] *)

val encode_cmpop : Ast_types.cmpop -> Pbrt.Encoder.t -> unit
(** [encode_cmpop v encoder] encodes [v] with the given [encoder] *)

val encode_val_ : Ast_types.val_ -> Pbrt.Encoder.t -> unit
(** [encode_val_ v encoder] encodes [v] with the given [encoder] *)

val encode_expression_binop : Ast_types.expression_binop -> Pbrt.Encoder.t -> unit
(** [encode_expression_binop v encoder] encodes [v] with the given [encoder] *)

val encode_expression : Ast_types.expression -> Pbrt.Encoder.t -> unit
(** [encode_expression v encoder] encodes [v] with the given [encoder] *)

val encode_expression_field_acc : Ast_types.expression_field_acc -> Pbrt.Encoder.t -> unit
(** [encode_expression_field_acc v encoder] encodes [v] with the given [encoder] *)

val encode_formula_compare : Ast_types.formula_compare -> Pbrt.Encoder.t -> unit
(** [encode_formula_compare v encoder] encodes [v] with the given [encoder] *)

val encode_formula_alpha : Ast_types.formula_alpha -> Pbrt.Encoder.t -> unit
(** [encode_formula_alpha v encoder] encodes [v] with the given [encoder] *)

val encode_formula_access : Ast_types.formula_access -> Pbrt.Encoder.t -> unit
(** [encode_formula_access v encoder] encodes [v] with the given [encoder] *)

val encode_formula_separate : Ast_types.formula_separate -> Pbrt.Encoder.t -> unit
(** [encode_formula_separate v encoder] encodes [v] with the given [encoder] *)

val encode_formula : Ast_types.formula -> Pbrt.Encoder.t -> unit
(** [encode_formula v encoder] encodes [v] with the given [encoder] *)

val encode_phi : Ast_types.phi -> Pbrt.Encoder.t -> unit
(** [encode_phi v encoder] encodes [v] with the given [encoder] *)

val encode_contract : Ast_types.contract -> Pbrt.Encoder.t -> unit
(** [encode_contract v encoder] encodes [v] with the given [encoder] *)

val encode_statement_assign : Ast_types.statement_assign -> Pbrt.Encoder.t -> unit
(** [encode_statement_assign v encoder] encodes [v] with the given [encoder] *)

val encode_statement_field_assign : Ast_types.statement_field_assign -> Pbrt.Encoder.t -> unit
(** [encode_statement_field_assign v encoder] encodes [v] with the given [encoder] *)

val encode_statement_new : Ast_types.statement_new -> Pbrt.Encoder.t -> unit
(** [encode_statement_new v encoder] encodes [v] with the given [encoder] *)

val encode_statement_method_call : Ast_types.statement_method_call -> Pbrt.Encoder.t -> unit
(** [encode_statement_method_call v encoder] encodes [v] with the given [encoder] *)

val encode_statement_seq : Ast_types.statement_seq -> Pbrt.Encoder.t -> unit
(** [encode_statement_seq v encoder] encodes [v] with the given [encoder] *)

val encode_statement : Ast_types.statement -> Pbrt.Encoder.t -> unit
(** [encode_statement v encoder] encodes [v] with the given [encoder] *)

val encode_statement_if : Ast_types.statement_if -> Pbrt.Encoder.t -> unit
(** [encode_statement_if v encoder] encodes [v] with the given [encoder] *)

val encode_statement_hold : Ast_types.statement_hold -> Pbrt.Encoder.t -> unit
(** [encode_statement_hold v encoder] encodes [v] with the given [encoder] *)

val encode_statement_declare : Ast_types.statement_declare -> Pbrt.Encoder.t -> unit
(** [encode_statement_declare v encoder] encodes [v] with the given [encoder] *)

val encode_abs_pred_defn : Ast_types.abs_pred_defn -> Pbrt.Encoder.t -> unit
(** [encode_abs_pred_defn v encoder] encodes [v] with the given [encoder] *)

val encode_method_argument : Ast_types.method_argument -> Pbrt.Encoder.t -> unit
(** [encode_method_argument v encoder] encodes [v] with the given [encoder] *)

val encode_method_ : Ast_types.method_ -> Pbrt.Encoder.t -> unit
(** [encode_method_ v encoder] encodes [v] with the given [encoder] *)

val encode_class_field : Ast_types.class_field -> Pbrt.Encoder.t -> unit
(** [encode_class_field v encoder] encodes [v] with the given [encoder] *)

val encode_class_ : Ast_types.class_ -> Pbrt.Encoder.t -> unit
(** [encode_class_ v encoder] encodes [v] with the given [encoder] *)

val encode_program : Ast_types.program -> Pbrt.Encoder.t -> unit
(** [encode_program v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_identifier : Pbrt.Decoder.t -> Ast_types.identifier
(** [decode_identifier decoder] decodes a [identifier] value from [decoder] *)

val decode_type_ : Pbrt.Decoder.t -> Ast_types.type_
(** [decode_type_ decoder] decodes a [type_] value from [decoder] *)

val decode_expop : Pbrt.Decoder.t -> Ast_types.expop
(** [decode_expop decoder] decodes a [expop] value from [decoder] *)

val decode_cmpop : Pbrt.Decoder.t -> Ast_types.cmpop
(** [decode_cmpop decoder] decodes a [cmpop] value from [decoder] *)

val decode_val_ : Pbrt.Decoder.t -> Ast_types.val_
(** [decode_val_ decoder] decodes a [val_] value from [decoder] *)

val decode_expression_binop : Pbrt.Decoder.t -> Ast_types.expression_binop
(** [decode_expression_binop decoder] decodes a [expression_binop] value from [decoder] *)

val decode_expression : Pbrt.Decoder.t -> Ast_types.expression
(** [decode_expression decoder] decodes a [expression] value from [decoder] *)

val decode_expression_field_acc : Pbrt.Decoder.t -> Ast_types.expression_field_acc
(** [decode_expression_field_acc decoder] decodes a [expression_field_acc] value from [decoder] *)

val decode_formula_compare : Pbrt.Decoder.t -> Ast_types.formula_compare
(** [decode_formula_compare decoder] decodes a [formula_compare] value from [decoder] *)

val decode_formula_alpha : Pbrt.Decoder.t -> Ast_types.formula_alpha
(** [decode_formula_alpha decoder] decodes a [formula_alpha] value from [decoder] *)

val decode_formula_access : Pbrt.Decoder.t -> Ast_types.formula_access
(** [decode_formula_access decoder] decodes a [formula_access] value from [decoder] *)

val decode_formula_separate : Pbrt.Decoder.t -> Ast_types.formula_separate
(** [decode_formula_separate decoder] decodes a [formula_separate] value from [decoder] *)

val decode_formula : Pbrt.Decoder.t -> Ast_types.formula
(** [decode_formula decoder] decodes a [formula] value from [decoder] *)

val decode_phi : Pbrt.Decoder.t -> Ast_types.phi
(** [decode_phi decoder] decodes a [phi] value from [decoder] *)

val decode_contract : Pbrt.Decoder.t -> Ast_types.contract
(** [decode_contract decoder] decodes a [contract] value from [decoder] *)

val decode_statement_assign : Pbrt.Decoder.t -> Ast_types.statement_assign
(** [decode_statement_assign decoder] decodes a [statement_assign] value from [decoder] *)

val decode_statement_field_assign : Pbrt.Decoder.t -> Ast_types.statement_field_assign
(** [decode_statement_field_assign decoder] decodes a [statement_field_assign] value from [decoder] *)

val decode_statement_new : Pbrt.Decoder.t -> Ast_types.statement_new
(** [decode_statement_new decoder] decodes a [statement_new] value from [decoder] *)

val decode_statement_method_call : Pbrt.Decoder.t -> Ast_types.statement_method_call
(** [decode_statement_method_call decoder] decodes a [statement_method_call] value from [decoder] *)

val decode_statement_seq : Pbrt.Decoder.t -> Ast_types.statement_seq
(** [decode_statement_seq decoder] decodes a [statement_seq] value from [decoder] *)

val decode_statement : Pbrt.Decoder.t -> Ast_types.statement
(** [decode_statement decoder] decodes a [statement] value from [decoder] *)

val decode_statement_if : Pbrt.Decoder.t -> Ast_types.statement_if
(** [decode_statement_if decoder] decodes a [statement_if] value from [decoder] *)

val decode_statement_hold : Pbrt.Decoder.t -> Ast_types.statement_hold
(** [decode_statement_hold decoder] decodes a [statement_hold] value from [decoder] *)

val decode_statement_declare : Pbrt.Decoder.t -> Ast_types.statement_declare
(** [decode_statement_declare decoder] decodes a [statement_declare] value from [decoder] *)

val decode_abs_pred_defn : Pbrt.Decoder.t -> Ast_types.abs_pred_defn
(** [decode_abs_pred_defn decoder] decodes a [abs_pred_defn] value from [decoder] *)

val decode_method_argument : Pbrt.Decoder.t -> Ast_types.method_argument
(** [decode_method_argument decoder] decodes a [method_argument] value from [decoder] *)

val decode_method_ : Pbrt.Decoder.t -> Ast_types.method_
(** [decode_method_ decoder] decodes a [method_] value from [decoder] *)

val decode_class_field : Pbrt.Decoder.t -> Ast_types.class_field
(** [decode_class_field decoder] decodes a [class_field] value from [decoder] *)

val decode_class_ : Pbrt.Decoder.t -> Ast_types.class_
(** [decode_class_ decoder] decodes a [class_] value from [decoder] *)

val decode_program : Pbrt.Decoder.t -> Ast_types.program
(** [decode_program decoder] decodes a [program] value from [decoder] *)
