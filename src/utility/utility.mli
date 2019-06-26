(** {1 Generic Utilities} *)

exception Unimplemented
val ( << ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val debug : string -> ?focus:bool -> ?hide:bool -> unit
