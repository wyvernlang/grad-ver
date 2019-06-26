(** {1 Generic Utilities} *)

exception Unimplemented
val ( << ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

val debug       : ?focus:bool -> ?hide:bool -> string -> unit
val debugList   : ?focus:bool -> ?hide:bool -> string list -> unit
val message     : ?focus:bool -> ?hide:bool -> string -> unit
val messageList : ?focus:bool -> ?hide:bool -> string list -> unit
