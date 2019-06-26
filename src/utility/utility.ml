open Core
open Ast

exception Unimplemented

let ( << ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c = fun f g x -> f (g x)
let ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c = fun g f x -> f (g x)

let debug_on = true
let debug msg ?(focus=false) ?(hide=false) =
  if debug_on && (not hide) then
    if focus then
      print_endline @@ "\n|||> "^msg
    else
      print_endline @@ "\n|> "^msg
