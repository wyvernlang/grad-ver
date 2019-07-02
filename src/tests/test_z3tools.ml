open Core
open OUnit2
open Z3
open Z3.Arithmetic

let basic ctxt : unit =
  let cfg = [ "model","true" ; "proof","false" ] in
  let ctx = mk_context cfg in
  let int_sym = Symbol.mk_int ctx 42 in
  let str_sym = Symbol.mk_string ctx "mySymbol" in
  let bool_sort = Boolean.mk_sort ctx in
  let int_sort = Integer.mk_sort ctx in
  let real_sort = Real.mk_sort ctx in
  Printf.printf "int symbol: %s" @@ Symbol.to_string int_sym;
  Printf.printf "string symbol: %s" @@ Symbol.to_string str_sym;
  Printf.printf "bool sort: %s" @@ Sort.to_string bool_sort;
  Printf.printf "int sort: %s" @@ Sort.to_string int_sort;
  Printf.printf "real sort: %s" @@ Sort.to_string real_sort

let suite () : test =
  "z3tools" >::: [
    "basic:" >:: basic
  ]
