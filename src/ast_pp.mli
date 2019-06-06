(** ast.proto Pretty Printing *)


(** {2 Formatters} *)

val pp_type_ : Format.formatter -> Ast_types.type_ -> unit 
(** [pp_type_ v] formats v *)

val pp_class_field : Format.formatter -> Ast_types.class_field -> unit 
(** [pp_class_field v] formats v *)

val pp_argument : Format.formatter -> Ast_types.argument -> unit 
(** [pp_argument v] formats v *)

val pp_variable : Format.formatter -> Ast_types.variable -> unit 
(** [pp_variable v] formats v *)

val pp_value : Format.formatter -> Ast_types.value -> unit 
(** [pp_value v] formats v *)

val pp_expression_operator : Format.formatter -> Ast_types.expression_operator -> unit 
(** [pp_expression_operator v] formats v *)

val pp_expression_comparer : Format.formatter -> Ast_types.expression_comparer -> unit 
(** [pp_expression_comparer v] formats v *)

val pp_expression : Format.formatter -> Ast_types.expression -> unit 
(** [pp_expression v] formats v *)

val pp_expression_operation : Format.formatter -> Ast_types.expression_operation -> unit 
(** [pp_expression_operation v] formats v *)

val pp_expression_comparison : Format.formatter -> Ast_types.expression_comparison -> unit 
(** [pp_expression_comparison v] formats v *)

val pp_expression_field_reference : Format.formatter -> Ast_types.expression_field_reference -> unit 
(** [pp_expression_field_reference v] formats v *)

val pp_predicate_check : Format.formatter -> Ast_types.predicate_check -> unit 
(** [pp_predicate_check v] formats v *)

val pp_concrete_access_check : Format.formatter -> Ast_types.concrete_access_check -> unit 
(** [pp_concrete_access_check v] formats v *)

val pp_concrete_operator : Format.formatter -> Ast_types.concrete_operator -> unit 
(** [pp_concrete_operator v] formats v *)

val pp_concrete : Format.formatter -> Ast_types.concrete -> unit 
(** [pp_concrete v] formats v *)

val pp_concrete_operation : Format.formatter -> Ast_types.concrete_operation -> unit 
(** [pp_concrete_operation v] formats v *)

val pp_concrete_if_then_else : Format.formatter -> Ast_types.concrete_if_then_else -> unit 
(** [pp_concrete_if_then_else v] formats v *)

val pp_concrete_unfolding_in : Format.formatter -> Ast_types.concrete_unfolding_in -> unit 
(** [pp_concrete_unfolding_in v] formats v *)

val pp_formula : Format.formatter -> Ast_types.formula -> unit 
(** [pp_formula v] formats v *)

val pp_predicate : Format.formatter -> Ast_types.predicate -> unit 
(** [pp_predicate v] formats v *)

val pp_contract : Format.formatter -> Ast_types.contract -> unit 
(** [pp_contract v] formats v *)

val pp_statement_declaration : Format.formatter -> Ast_types.statement_declaration -> unit 
(** [pp_statement_declaration v] formats v *)

val pp_statement_assignment : Format.formatter -> Ast_types.statement_assignment -> unit 
(** [pp_statement_assignment v] formats v *)

val pp_statement_field_assignment : Format.formatter -> Ast_types.statement_field_assignment -> unit 
(** [pp_statement_field_assignment v] formats v *)

val pp_statement_new_object : Format.formatter -> Ast_types.statement_new_object -> unit 
(** [pp_statement_new_object v] formats v *)

val pp_statement_method_call : Format.formatter -> Ast_types.statement_method_call -> unit 
(** [pp_statement_method_call v] formats v *)

val pp_statement_assertion : Format.formatter -> Ast_types.statement_assertion -> unit 
(** [pp_statement_assertion v] formats v *)

val pp_statement_release : Format.formatter -> Ast_types.statement_release -> unit 
(** [pp_statement_release v] formats v *)

val pp_statement_fold : Format.formatter -> Ast_types.statement_fold -> unit 
(** [pp_statement_fold v] formats v *)

val pp_statement_unfold : Format.formatter -> Ast_types.statement_unfold -> unit 
(** [pp_statement_unfold v] formats v *)

val pp_statement : Format.formatter -> Ast_types.statement -> unit 
(** [pp_statement v] formats v *)

val pp_statement_sequence : Format.formatter -> Ast_types.statement_sequence -> unit 
(** [pp_statement_sequence v] formats v *)

val pp_statement_if_then_else : Format.formatter -> Ast_types.statement_if_then_else -> unit 
(** [pp_statement_if_then_else v] formats v *)

val pp_statement_while_loop : Format.formatter -> Ast_types.statement_while_loop -> unit 
(** [pp_statement_while_loop v] formats v *)

val pp_statement_hold : Format.formatter -> Ast_types.statement_hold -> unit 
(** [pp_statement_hold v] formats v *)

val pp_method_ : Format.formatter -> Ast_types.method_ -> unit 
(** [pp_method_ v] formats v *)

val pp_class_ : Format.formatter -> Ast_types.class_ -> unit 
(** [pp_class_ v] formats v *)

val pp_program : Format.formatter -> Ast_types.program -> unit 
(** [pp_program v] formats v *)
