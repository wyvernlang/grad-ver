open Ast_types

exception Unimplemented
let unimplemented () = raise Unimplemented

let ( << ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c = fun f g x -> f (g x)
let ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c = fun g f x -> f (g x)

let id_of_string : string -> id = fun str -> {string=str}

let rec join_statements : statement list -> statement =
  function
  | [] -> Skip
  | s::ss -> Sequence{ prev=s; next=join_statements ss }
