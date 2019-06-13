type id = string
exception Unexpected_nonid_value of Ast_types.value
exception Unexpected_nonid_expression of Ast_types.expression
val eqId : id -> id -> bool
val eqType : Ast_types.type_ -> Ast_types.type_ -> bool
val eqClass : Ast_types.class_ -> Ast_types.class_ -> bool
val getExpressionId : Ast_types.expression -> id
