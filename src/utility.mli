open Ast_types

val unimplemented : unit -> 'a
val ( << ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val id_of_string : string -> id
val join_statements : statement list -> statement
