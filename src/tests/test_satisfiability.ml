open OUnit2
open Core
open Ast
open Wellformed
open Aliasing
open Functools
open Utility
open Satisfiability

open Test_utility

(* test constructor *)

let prgm : program = {
  classes=[];
  statement=Skip;
}

let makeSatisfiabilityTest : formula -> test_fun =
  let clsctx = failwith "TODO: construct from [prgm]" in
  let typctx = failwith "TODO: construct from [prgm]" in
  let scpctx = failwith "TODO: construct from [prgm]" in
  Test_utility.makeTruthTest ~sexp_of_t:(failwith "sexp_of_phi") (isSatisfiable clsctx typctx scpctx)

(* stock *)

(* TODO *)

(* suite *)

let suite () : test =
  "satisfiability" >::: [

  ]
