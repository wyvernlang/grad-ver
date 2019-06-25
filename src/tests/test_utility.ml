open OUnit2
open Core
open Utility
open Functools

let header    : string = "=============================================================================="
let subheader : string = "------------------------------------------------------------------------------"

let makeEqualityTest sexp_of_t msg t tCorrect =
  let string_of_t = Sexp.to_string_hum ~indent:4 @< sexp_of_t in
  fun ctxt ->
  assert_equal
    ~msg:(header^"\n"^msg^"\n")
    ~printer:(fun t -> "\n"^subheader^"\n"^string_of_t t^"\n"^subheader^"\n")
    t tCorrect
