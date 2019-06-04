(** ast.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_type_ : Ast_types.type_ -> Pbrt.Encoder.t -> unit
(** [encode_type_ v encoder] encodes [v] with the given [encoder] *)

val encode_class_field : Ast_types.class_field -> Pbrt.Encoder.t -> unit
(** [encode_class_field v encoder] encodes [v] with the given [encoder] *)

val encode_argument : Ast_types.argument -> Pbrt.Encoder.t -> unit
(** [encode_argument v encoder] encodes [v] with the given [encoder] *)

val encode_variable : Ast_types.variable -> Pbrt.Encoder.t -> unit
(** [encode_variable v encoder] encodes [v] with the given [encoder] *)

val encode_value : Ast_types.value -> Pbrt.Encoder.t -> unit
(** [encode_value v encoder] encodes [v] with the given [encoder] *)

val encode_expression_operator : Ast_types.expression_operator -> Pbrt.Encoder.t -> unit
(** [encode_expression_operator v encoder] encodes [v] with the given [encoder] *)

val encode_expression_comparer : Ast_types.expression_comparer -> Pbrt.Encoder.t -> unit
(** [encode_expression_comparer v encoder] encodes [v] with the given [encoder] *)

val encode_expression : Ast_types.expression -> Pbrt.Encoder.t -> unit
(** [encode_expression v encoder] encodes [v] with the given [encoder] *)

val encode_expression_operation : Ast_types.expression_operation -> Pbrt.Encoder.t -> unit
(** [encode_expression_operation v encoder] encodes [v] with the given [encoder] *)

val encode_expression_comparison : Ast_types.expression_comparison -> Pbrt.Encoder.t -> unit
(** [encode_expression_comparison v encoder] encodes [v] with the given [encoder] *)

val encode_expression_field_reference : Ast_types.expression_field_reference -> Pbrt.Encoder.t -> unit
(** [encode_expression_field_reference v encoder] encodes [v] with the given [encoder] *)

val encode_formula_concrete_predicate_check : Ast_types.formula_concrete_predicate_check -> Pbrt.Encoder.t -> unit
(** [encode_formula_concrete_predicate_check v encoder] encodes [v] with the given [encoder] *)

val encode_formula_concrete_access_check : Ast_types.formula_concrete_access_check -> Pbrt.Encoder.t -> unit
(** [encode_formula_concrete_access_check v encoder] encodes [v] with the given [encoder] *)

val encode_formula_concrete_operator : Ast_types.formula_concrete_operator -> Pbrt.Encoder.t -> unit
(** [encode_formula_concrete_operator v encoder] encodes [v] with the given [encoder] *)

val encode_formula_concrete : Ast_types.formula_concrete -> Pbrt.Encoder.t -> unit
(** [encode_formula_concrete v encoder] encodes [v] with the given [encoder] *)

val encode_formula_concrete_operation : Ast_types.formula_concrete_operation -> Pbrt.Encoder.t -> unit
(** [encode_formula_concrete_operation v encoder] encodes [v] with the given [encoder] *)

val encode_formula_concrete_if_then_else : Ast_types.formula_concrete_if_then_else -> Pbrt.Encoder.t -> unit
(** [encode_formula_concrete_if_then_else v encoder] encodes [v] with the given [encoder] *)

val encode_formula_concrete_unfolding_in : Ast_types.formula_concrete_unfolding_in -> Pbrt.Encoder.t -> unit
(** [encode_formula_concrete_unfolding_in v encoder] encodes [v] with the given [encoder] *)

val encode_formula_imprecise : Ast_types.formula_imprecise -> Pbrt.Encoder.t -> unit
(** [encode_formula_imprecise v encoder] encodes [v] with the given [encoder] *)

val encode_formula : Ast_types.formula -> Pbrt.Encoder.t -> unit
(** [encode_formula v encoder] encodes [v] with the given [encoder] *)

val encode_predicate : Ast_types.predicate -> Pbrt.Encoder.t -> unit
(** [encode_predicate v encoder] encodes [v] with the given [encoder] *)

val encode_contract : Ast_types.contract -> Pbrt.Encoder.t -> unit
(** [encode_contract v encoder] encodes [v] with the given [encoder] *)

val encode_statement_declaration : Ast_types.statement_declaration -> Pbrt.Encoder.t -> unit
(** [encode_statement_declaration v encoder] encodes [v] with the given [encoder] *)

val encode_statement_assignment : Ast_types.statement_assignment -> Pbrt.Encoder.t -> unit
(** [encode_statement_assignment v encoder] encodes [v] with the given [encoder] *)

val encode_statement_field_assignment : Ast_types.statement_field_assignment -> Pbrt.Encoder.t -> unit
(** [encode_statement_field_assignment v encoder] encodes [v] with the given [encoder] *)

val encode_statement_new_object : Ast_types.statement_new_object -> Pbrt.Encoder.t -> unit
(** [encode_statement_new_object v encoder] encodes [v] with the given [encoder] *)

val encode_statement_method_call : Ast_types.statement_method_call -> Pbrt.Encoder.t -> unit
(** [encode_statement_method_call v encoder] encodes [v] with the given [encoder] *)

val encode_statement_assertion : Ast_types.statement_assertion -> Pbrt.Encoder.t -> unit
(** [encode_statement_assertion v encoder] encodes [v] with the given [encoder] *)

val encode_statement_release : Ast_types.statement_release -> Pbrt.Encoder.t -> unit
(** [encode_statement_release v encoder] encodes [v] with the given [encoder] *)

val encode_statement_fold : Ast_types.statement_fold -> Pbrt.Encoder.t -> unit
(** [encode_statement_fold v encoder] encodes [v] with the given [encoder] *)

val encode_statement_unfold : Ast_types.statement_unfold -> Pbrt.Encoder.t -> unit
(** [encode_statement_unfold v encoder] encodes [v] with the given [encoder] *)

val encode_statement : Ast_types.statement -> Pbrt.Encoder.t -> unit
(** [encode_statement v encoder] encodes [v] with the given [encoder] *)

val encode_statement_sequence : Ast_types.statement_sequence -> Pbrt.Encoder.t -> unit
(** [encode_statement_sequence v encoder] encodes [v] with the given [encoder] *)

val encode_statement_if_then_else : Ast_types.statement_if_then_else -> Pbrt.Encoder.t -> unit
(** [encode_statement_if_then_else v encoder] encodes [v] with the given [encoder] *)

val encode_statement_while_loop : Ast_types.statement_while_loop -> Pbrt.Encoder.t -> unit
(** [encode_statement_while_loop v encoder] encodes [v] with the given [encoder] *)

val encode_statement_hold : Ast_types.statement_hold -> Pbrt.Encoder.t -> unit
(** [encode_statement_hold v encoder] encodes [v] with the given [encoder] *)

val encode_method_ : Ast_types.method_ -> Pbrt.Encoder.t -> unit
(** [encode_method_ v encoder] encodes [v] with the given [encoder] *)

val encode_class_ : Ast_types.class_ -> Pbrt.Encoder.t -> unit
(** [encode_class_ v encoder] encodes [v] with the given [encoder] *)

val encode_program : Ast_types.program -> Pbrt.Encoder.t -> unit
(** [encode_program v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_type_ : Pbrt.Decoder.t -> Ast_types.type_
(** [decode_type_ decoder] decodes a [type_] value from [decoder] *)

val decode_class_field : Pbrt.Decoder.t -> Ast_types.class_field
(** [decode_class_field decoder] decodes a [class_field] value from [decoder] *)

val decode_argument : Pbrt.Decoder.t -> Ast_types.argument
(** [decode_argument decoder] decodes a [argument] value from [decoder] *)

val decode_variable : Pbrt.Decoder.t -> Ast_types.variable
(** [decode_variable decoder] decodes a [variable] value from [decoder] *)

val decode_value : Pbrt.Decoder.t -> Ast_types.value
(** [decode_value decoder] decodes a [value] value from [decoder] *)

val decode_expression_operator : Pbrt.Decoder.t -> Ast_types.expression_operator
(** [decode_expression_operator decoder] decodes a [expression_operator] value from [decoder] *)

val decode_expression_comparer : Pbrt.Decoder.t -> Ast_types.expression_comparer
(** [decode_expression_comparer decoder] decodes a [expression_comparer] value from [decoder] *)

val decode_expression : Pbrt.Decoder.t -> Ast_types.expression
(** [decode_expression decoder] decodes a [expression] value from [decoder] *)

val decode_expression_operation : Pbrt.Decoder.t -> Ast_types.expression_operation
(** [decode_expression_operation decoder] decodes a [expression_operation] value from [decoder] *)

val decode_expression_comparison : Pbrt.Decoder.t -> Ast_types.expression_comparison
(** [decode_expression_comparison decoder] decodes a [expression_comparison] value from [decoder] *)

val decode_expression_field_reference : Pbrt.Decoder.t -> Ast_types.expression_field_reference
(** [decode_expression_field_reference decoder] decodes a [expression_field_reference] value from [decoder] *)

val decode_formula_concrete_predicate_check : Pbrt.Decoder.t -> Ast_types.formula_concrete_predicate_check
(** [decode_formula_concrete_predicate_check decoder] decodes a [formula_concrete_predicate_check] value from [decoder] *)

val decode_formula_concrete_access_check : Pbrt.Decoder.t -> Ast_types.formula_concrete_access_check
(** [decode_formula_concrete_access_check decoder] decodes a [formula_concrete_access_check] value from [decoder] *)

val decode_formula_concrete_operator : Pbrt.Decoder.t -> Ast_types.formula_concrete_operator
(** [decode_formula_concrete_operator decoder] decodes a [formula_concrete_operator] value from [decoder] *)

val decode_formula_concrete : Pbrt.Decoder.t -> Ast_types.formula_concrete
(** [decode_formula_concrete decoder] decodes a [formula_concrete] value from [decoder] *)

val decode_formula_concrete_operation : Pbrt.Decoder.t -> Ast_types.formula_concrete_operation
(** [decode_formula_concrete_operation decoder] decodes a [formula_concrete_operation] value from [decoder] *)

val decode_formula_concrete_if_then_else : Pbrt.Decoder.t -> Ast_types.formula_concrete_if_then_else
(** [decode_formula_concrete_if_then_else decoder] decodes a [formula_concrete_if_then_else] value from [decoder] *)

val decode_formula_concrete_unfolding_in : Pbrt.Decoder.t -> Ast_types.formula_concrete_unfolding_in
(** [decode_formula_concrete_unfolding_in decoder] decodes a [formula_concrete_unfolding_in] value from [decoder] *)

val decode_formula_imprecise : Pbrt.Decoder.t -> Ast_types.formula_imprecise
(** [decode_formula_imprecise decoder] decodes a [formula_imprecise] value from [decoder] *)

val decode_formula : Pbrt.Decoder.t -> Ast_types.formula
(** [decode_formula decoder] decodes a [formula] value from [decoder] *)

val decode_predicate : Pbrt.Decoder.t -> Ast_types.predicate
(** [decode_predicate decoder] decodes a [predicate] value from [decoder] *)

val decode_contract : Pbrt.Decoder.t -> Ast_types.contract
(** [decode_contract decoder] decodes a [contract] value from [decoder] *)

val decode_statement_declaration : Pbrt.Decoder.t -> Ast_types.statement_declaration
(** [decode_statement_declaration decoder] decodes a [statement_declaration] value from [decoder] *)

val decode_statement_assignment : Pbrt.Decoder.t -> Ast_types.statement_assignment
(** [decode_statement_assignment decoder] decodes a [statement_assignment] value from [decoder] *)

val decode_statement_field_assignment : Pbrt.Decoder.t -> Ast_types.statement_field_assignment
(** [decode_statement_field_assignment decoder] decodes a [statement_field_assignment] value from [decoder] *)

val decode_statement_new_object : Pbrt.Decoder.t -> Ast_types.statement_new_object
(** [decode_statement_new_object decoder] decodes a [statement_new_object] value from [decoder] *)

val decode_statement_method_call : Pbrt.Decoder.t -> Ast_types.statement_method_call
(** [decode_statement_method_call decoder] decodes a [statement_method_call] value from [decoder] *)

val decode_statement_assertion : Pbrt.Decoder.t -> Ast_types.statement_assertion
(** [decode_statement_assertion decoder] decodes a [statement_assertion] value from [decoder] *)

val decode_statement_release : Pbrt.Decoder.t -> Ast_types.statement_release
(** [decode_statement_release decoder] decodes a [statement_release] value from [decoder] *)

val decode_statement_fold : Pbrt.Decoder.t -> Ast_types.statement_fold
(** [decode_statement_fold decoder] decodes a [statement_fold] value from [decoder] *)

val decode_statement_unfold : Pbrt.Decoder.t -> Ast_types.statement_unfold
(** [decode_statement_unfold decoder] decodes a [statement_unfold] value from [decoder] *)

val decode_statement : Pbrt.Decoder.t -> Ast_types.statement
(** [decode_statement decoder] decodes a [statement] value from [decoder] *)

val decode_statement_sequence : Pbrt.Decoder.t -> Ast_types.statement_sequence
(** [decode_statement_sequence decoder] decodes a [statement_sequence] value from [decoder] *)

val decode_statement_if_then_else : Pbrt.Decoder.t -> Ast_types.statement_if_then_else
(** [decode_statement_if_then_else decoder] decodes a [statement_if_then_else] value from [decoder] *)

val decode_statement_while_loop : Pbrt.Decoder.t -> Ast_types.statement_while_loop
(** [decode_statement_while_loop decoder] decodes a [statement_while_loop] value from [decoder] *)

val decode_statement_hold : Pbrt.Decoder.t -> Ast_types.statement_hold
(** [decode_statement_hold decoder] decodes a [statement_hold] value from [decoder] *)

val decode_method_ : Pbrt.Decoder.t -> Ast_types.method_
(** [decode_method_ decoder] decodes a [method_] value from [decoder] *)

val decode_class_ : Pbrt.Decoder.t -> Ast_types.class_
(** [decode_class_ decoder] decodes a [class_] value from [decoder] *)

val decode_program : Pbrt.Decoder.t -> Ast_types.program
(** [decode_program decoder] decodes a [program] value from [decoder] *)
