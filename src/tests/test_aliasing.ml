open OUnit2
open Core
open Ast
open Aliasing
open Functools
open Utility

open Test_utility

let makeAliasingContextTest : string -> AliasingContext.t -> AliasingContext.t -> test_fun =
  makeEqualityTest AliasingContext.sexp_of_t

let union_1 =
  let msg = "union with empty context" in
  let ctx : AliasingContext.t = { scope=Scope 0;
                                  parent=None;
                                  props=(ofObjectValueSetEltListList[
                                      [ObjectValueSetElt.Value(Bool true)]
                                    ]);
                                  children=[]; } in
  let ctx' : AliasingContext.t = { scope=Scope 0;
                                   parent=None;
                                   props=(ofObjectValueSetEltListList[
                                       [ObjectValueSetElt.Value(Bool true)]
                                     ]);
                                   children=[]; } in
  makeAliasingContextTest msg (AliasingContext.union ctx ctx') ctx

let construction_1 ctxt =
  todo "get merging to work first"
  (* makeAliasingContextTest
    "super simple construction" *)


let equality_1 = makeAliasingContextTest
    "super simple equality"
    { parent=None;
      scope=Scope 0;
      props=AliasPropSet.empty;
      children=[] }
    { parent=None;
      scope=Scope 0;
      props=AliasPropSet.empty;
      children=[] }

let tmp_1 ctxt =
  assert_equal (AliasPropSet.equal) (AliasPropSet.empty)

let suite : test =
  "aliasing" >:::
  [ "tmp 1" >:: tmp_1 ]
  (* [ "equality 1" >:: equality_1 *)
  (* ; "union 1" >:: union_1 *)
  (* ; "construction 1" >:: construction_1 *)
  (* ] *)
