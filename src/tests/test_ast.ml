open OUnit2
open Core
open Ast_types
open Ast
open Utility
open Functools

open Test_utility

let makeProgramTest : Ast.program -> Ast.program -> test_fun =
  makeEqualityTest ~cmp:(=) ~sexp_of_t:sexp_of_program

module Equality =
struct
  let suite : test =
    "equality" >::: [
      "inequal" >:: makeEqualityTest ~cmp:(!=) ~sexp_of_t:sexp_of_program
        { classes=[]; statement=Skip }
        { classes=[]; statement=Assertion{ concrete=Expression(Value(Bool(false))) } }
    ]
end

module Wrapping =
struct
  let suite : test =
    "wrapping" >::: [
        (* TODO *)
    ]
end


let suite : test =
  "ast" >::: [
    Equality.suite;
    Wrapping.suite
  ]
