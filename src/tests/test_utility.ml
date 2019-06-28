open OUnit2
open Core
open Utility
open Functools
open Sexplib

let header    : string = "+============================================================================+"
let subheader : string = "+----------------------------------------------------------------------------+"

let makeEqualityTest ~(cmp:'a -> 'a -> bool) ~(sexp_of_t:'a -> Sexp.t) : 'a -> 'a -> test_fun =
  let string_of_t = Sexp.to_string_hum ~indent:4 @< sexp_of_t in
  fun (t:'a) (tCorrect:'a) -> fun (ctxt:test_ctxt) ->
    assert_equal
      ~cmp
      ~printer:(fun t -> "\n"^subheader^"\n"^string_of_t t^"\n"^subheader^"\n")
      tCorrect t

exception Unit_test_failure of string

let makeUnitTest ~(sexp_of_t:'a -> Sexp.t) (f:'a -> unit) (x:'a) : test_fun =
  let string_of_t = Sexp.to_string_hum ~indent:4 @< sexp_of_t in
  try
    fun (ctxt:test_ctxt) -> f x
  with
  | _ -> raise @@ Unit_test_failure (string_of_t x)

let makeExceptionTest ~(ex:exn) ~(sexp_of_t:'a -> Sexp.t) (f:'a -> unit) (x:'a) : test_fun =
  let string_of_t = Sexp.to_string_hum ~indent:4 @< sexp_of_t in
  fun (ctxt:test_ctxt) -> assert_raises ~msg:(string_of_t x) ex (fun () -> f x)

let makeTruthTest ~(sexp_of_t:'a -> Sexp.t) (f:'a -> bool) (x:'a) : test_fun =
  let string_of_t = Sexp.to_string_hum ~indent:4 @< sexp_of_t in
  fun (ctxt:test_ctxt) -> assert_bool (string_of_t x) (f x)

let todo_if ctxt ~do_skip:b msg test =
  if b then todo msg else test ctxt
