exception Malformed of string
exception Invalid_field_reference_base of Ast_types.expression * Ast.id
exception Class_undefined of Ast.id
exception Class_mismatch of Ast_types.class_ * Ast_types.class_
exception Field_undeclared of Ast_types.class_ * Ast.id
exception Predicate_undefined of Ast_types.class_ option * Ast.id
exception Method_undefined of Ast_types.class_ * Ast.id
exception Variable_undeclared of Ast.id
exception Type_mismatch of Ast_types.type_ * Ast_types.type_
exception Method_call_arguments_length_mismatch of Ast_types.method_ *
            Ast_types.statement_method_call
exception Fold_arguments_length_mismatch of Ast_types.predicate *
            Ast_types.statement_fold
exception Unfold_arguments_length_mismatch of Ast_types.predicate *
            Ast_types.statement_unfold
exception Unfolding_in_arguments_length_mismatch of Ast_types.predicate *
            Ast_types.formula_concrete_unfolding_in
val eqId : Ast.id -> Ast.id -> bool
val eqType : Ast_types.type_ -> Ast_types.type_ -> bool
val eqClass : Ast_types.class_ -> Ast_types.class_ -> bool
val getVariableId : Ast_types.variable -> Ast.id
val getValueId : Ast_types.value -> Ast.id
val check : bool -> exn -> unit
val checkSome : 'a option -> exn -> unit
val getSome : 'a option -> exn -> 'a
val checkFold : ('a -> unit) -> 'a Core.List.t -> unit
val string_of_type_ : Ast_types.type_ -> string
val checkTypeMatch : Ast_types.type_ -> Ast_types.type_ -> unit
val checkClassMatch : Ast_types.class_ -> Ast_types.class_ -> unit
module Context :
  sig
    type 'a t = (string, 'a) Core.String.Table.t_
    val create : unit -> 'a t
    val find : 'a t -> Ast.id -> 'a option
    val set : 'a t -> Ast.id -> 'a -> unit
    val findExn : 'a t -> Ast.id -> exn -> 'a
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val iter : 'a t -> f:('a -> unit) -> unit
    val filter : 'a t -> f:('a -> bool) -> 'a t
  end
val class_context : Ast_types.class_ Context.t
val setClass : Ast.id -> Ast_types.class_ -> unit
val getClass : Ast.id -> Ast_types.class_
val getField : Ast.id -> Ast.id -> Ast_types.class_field
val getFieldType : Ast.id -> Ast.id -> Ast_types.type_
val getPredicate : Ast.id -> Ast.id -> Ast_types.predicate
val getImplicitPredicate : Ast.id -> Ast_types.predicate
val getPredicateArguments : Ast.id -> Ast.id -> Ast_types.argument list
val getPredicateFormula : Ast.id -> Ast.id -> Ast_types.formula
val getMethod : Ast.id -> Ast.id -> Ast_types.method_
val variable_context : Ast_types.type_ Context.t
val setVariableType : Ast.id -> Ast_types.type_ -> unit
val getVariableType : Ast.id -> Ast_types.type_
val synthesizeType : Ast_types.expression -> Ast_types.type_
val checkExpression : Ast_types.expression -> unit
val checkConreteFormula : Ast_types.formula_concrete -> unit
val checkImpreciseFormula : Ast_types.formula_imprecise -> unit
val checkFormula : Ast_types.formula -> unit
val checkContract : Ast_types.contract -> unit
val checkStatement : Ast_types.statement -> unit
val checkPredicate : Ast_types.predicate -> unit
val checkMethod : Ast_types.method_ -> unit
val checkClass : Ast_types.class_ -> unit
val checkProgram : Ast_types.program -> unit
