open OUnit2
open Core

open Utility
open Test_utility
open Functools
open Ast
open Aliasing
open Wellformed
open Framing

let program_stock = {
  classes=[];
  statement=Skip;
}

let clsctx_stock : ClassContext.t =
  ClassContext.construct program_stock
;;

let typctx_stock : TypeContext.t =
  let typctx = TypeContext.create () in
  TypeContext.constructStatement clsctx_stock typctx program_stock.statement;
  typctx
;;

let makeSelfFramingTest (phi:concrete) : test_fun =
  let clsctx = ClassContext.copy clsctx_stock in
  let typctx = TypeContext.copy typctx_stock in
  makeTruthTest ~sexp_of_t:sexp_of_formula (fun phi -> selfFrames clsctx typctx phi) (Concrete phi)

let expr_bool b : expression = Value(Bool b)
let phi_bool b : concrete = Expression(expr_bool b)

let suite : test =
  "framing" >::: [
    "trivial" >:: makeSelfFramingTest
      (phi_bool true)
  ]
