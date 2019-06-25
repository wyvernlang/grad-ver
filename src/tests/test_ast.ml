open OUnit2
open Core
open Ast_types
open Ast
open Utility
open Functools

open Test_utility

let makeProgramTest = makeEqualityTest sexp_of_program

let wrap_tmp = makeProgramTest
    "just to make sure equality works the way I think it does"
    { classes=[]; statement=Skip }
    { classes=[]; statement=Assertion{ concrete=Expression(Value(Bool(false))) } }

let suite : test =
  "ast" >:::
  [ "wrap_tmp" >:: wrap_tmp ]
