open Core
open Ast

exception Unimplemented

let ( << ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c = fun f g x -> f (g x)
let ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c = fun g f x -> f (g x)

let debug_on = false
let debug msg = if debug_on then print_endline @@ "[>] "^msg
