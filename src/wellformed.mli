exception Malformed of string
exception Invalid_field_reference of Ast_types.expression_field_reference
exception Invalid_access_check of Ast_types.concrete_access_check
exception Invalid_field_assignment of Ast_types.statement_field_assignment
exception Invalid_new_object of Ast_types.statement_new_object
exception Class_undefined of Ast.id
exception Field_undeclared of Ast_types.class_ * Ast.id
exception Predicate_undefined of Ast_types.class_ option * Ast.id
exception Method_undefined of Ast_types.class_ * Ast.id
exception Variable_undeclared of Ast.id
exception Type_mismatch of { left : Ast_types.type_; right : Ast_types.type_;
          }
exception Class_mismatch of { left : Ast_types.class_;
            right : Ast_types.class_;
          }
exception Unexpected_nonid_value of Ast_types.value
exception Unexpected_nonid_expression of Ast_types.expression
exception Method_call_arguments_length_mismatch of Ast_types.method_ *
            Ast_types.statement_method_call
exception Fold_arguments_length_mismatch of Ast_types.predicate *
            Ast_types.statement_fold
exception Unfold_arguments_length_mismatch of Ast_types.predicate *
            Ast_types.statement_unfold
exception Unfolding_in_arguments_length_mismatch of Ast_types.predicate *
            Ast_types.concrete_unfolding_in
val eqId : Ast.id -> Ast.id -> bool
val eqType : Ast_types.type_ -> Ast_types.type_ -> bool
val eqClass : Ast_types.class_ -> Ast_types.class_ -> bool
val getExpressionId : Ast_types.expression -> Ast.id
val check : bool -> exn -> unit
val checkSome : 'a option -> exn -> unit
val getSome : 'a option -> exn -> 'a
val checkFold : ('a -> unit) -> 'a Core.List.t -> unit
val string_of_type_ : Ast_types.type_ -> string
val checkTypeMatch : Ast_types.type_ -> Ast_types.type_ -> unit
val checkClassMatch : Ast_types.class_ -> Ast_types.class_ -> unit
type 'a context = (Ast.id, 'a) Core.String.Table.t_
val findExn : 'a context -> Ast.id -> exn -> 'a
val createContext : unit -> 'a context
val class_context : Ast_types.class_ context
val setClass : Ast.id -> Ast_types.class_ -> unit
val getClass : Ast.id -> Ast_types.class_
val getField : Ast.id -> Ast.id -> Ast_types.class_field
val getFieldType : Ast.id -> Ast.id -> Ast_types.type_
val getPredicate : Ast.id -> Ast.id -> Ast_types.predicate
val getPredicateArguments : Ast.id -> Ast.id -> Ast_types.argument list
val getPredicateFormula : Ast.id -> Ast.id -> Ast_types.formula
val getMethod : Ast.id -> Ast.id -> Ast_types.method_
val variable_context : Ast_types.type_ context
val setVariableType : Ast.id -> Ast_types.type_ -> unit
val getVariableType : Ast.id -> Ast_types.type_
val synthesizeType : Ast_types.expression -> Ast_types.type_
val inferPredicateClass : Ast_types.predicate_check -> Ast_types.class_
val checkExpression : Ast_types.expression -> unit
val checkConrete : Ast_types.concrete -> unit
val checkImpreciseFormula : Ast_types.concrete -> unit
val checkFormula : Ast_types.formula -> unit
val checkContract : Ast_types.contract -> unit
val checkStatement : Ast_types.statement -> unit
val checkPredicate : Ast_types.predicate -> unit
val checkMethod : Ast_types.method_ -> unit
val checkClass : Ast_types.class_ -> unit
val checkProgram : Ast_types.program -> unit
