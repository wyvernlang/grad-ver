open Core
open Ast

exception Unimplemented

let ( << ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c = fun f g x -> f (g x)
let ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c = fun g f x -> f (g x)

(*--------------------------------------------------------------------------------------------------------------------------*)
(* generic messages *)
(*--------------------------------------------------------------------------------------------------------------------------*)

let focus_header focus header =if focus then "=== "^header^" " else header^" "

let generic_message ?(focus=false) ?(hide=false) toggle header msg =
  if toggle && (not hide) then
    print_string @@ "\n"^focus_header focus header^msg^"\n"

let generic_messageList ?(focus=false) ?(hide=false) toggle header msgs =
  if toggle then
    let header = focus_header focus header in
    let item_header = String.of_char_list @@ List.init (String.length header) ~f:(fun _ -> ' ') in
    print_string @@ "\n"^header;
    List.iter msgs ~f:(fun msg -> generic_message ~focus:focus ~hide:hide toggle item_header msg);
    print_endline ""

(* message toggles *)
let toggle_message = true
let toggle_debug   = true

(* headers *)
let header_debug   = "[$]"
let header_message = "[>]"

(* debug *)
let debug     ?(focus=false) ?(hide=false) = generic_message     ~focus:focus ~hide:hide toggle_debug header_debug
let debugList ?(focus=false) ?(hide=false) = generic_messageList ~focus:focus ~hide:hide toggle_debug header_debug

(* message *)
let message     ?(focus=false) ?(hide=false) = generic_message     ~focus:focus ~hide:hide toggle_message header_message
let messageList ?(focus=false) ?(hide=false) = generic_messageList ~focus:focus ~hide:hide toggle_message header_message
