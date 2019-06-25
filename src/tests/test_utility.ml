open OUnit2
open Core
open Utility
open Functools

let header    : string = "+============================================================================+"
let subheader : string = "+----------------------------------------------------------------------------+"

let makeEqualityTest ~(cmp:'a -> 'a -> bool) ~(sexp_of_t:'a -> Sexplib.Sexp.t) =
  let string_of_t = Sexp.to_string_hum ~indent:4 @< sexp_of_t in
  fun (t:'a) (tCorrect:'a) -> fun (ctxt:test_ctxt) ->
    assert_equal
      ~cmp
      ~printer:(fun t -> "\n"^subheader^"\n"^string_of_t t^"\n"^subheader^"\n")
      tCorrect t

let todo_if ctxt ~do_skip:b msg test =
  if b then todo msg else test ctxt
