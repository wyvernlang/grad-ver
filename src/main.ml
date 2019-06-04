open Core
open Ast_types

let _ =
  print_endline "hello world";
  raise @@ Failure "what's wrong?"
