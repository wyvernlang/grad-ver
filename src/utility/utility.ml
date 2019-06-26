open Core
open Ast

exception Unimplemented

let ( << ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c = fun f g x -> f (g x)
let ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c = fun g f x -> f (g x)

(* toggle debug *)
let debug_on = true

let debug ?(focus=false) ?(hide=false) msg =
  if debug_on && (not hide) then
    if focus then
      print_endline @@ "\n|||> "^msg
    else
      print_endline @@ "\n|> "^msg

let debugList ?(focus=false) ?(hide=false) msgs =
  if debug_on && (not hide) then
    let header = if focus then "\n===|>\n" else "\n|>\n" in
    let indent = String.of_char_list @@ List.init (String.length header) (fun _ -> ' ') in
    print_endline @@ header ^ List.fold_left msgs ~init:"" ~f:(fun str msg -> str^indent^msg^"\n")
