open OUnit2
open Core
open Aliasing
open Functools
open Utility

open Test_utility

let makeAliasingContextTest = makeEqualityTest AliasingContext.sexp_of_t

let ctx : AliasingContext.t = { scope=Scope 0;
                                parent=None;
                                props=(AliasPropSet.singleton @@
                                       toAliasPropSetElt @@
                                       AliasProp.ofList[ ofObjectValueSetElt @@
                                                         ObjectValueSetElt.Value(Bool true) ] );
                                children=[]; }
let ctx' : AliasingContext.t = { scope=Scope 0;
                                parent=None;
                                props=(AliasPropSet.singleton @@
                                       toAliasPropSetElt @@
                                       AliasProp.ofList[ ofObjectValueSetElt @@
                                                         ObjectValueSetElt.Value(Bool true) ] );
                                children=[]; }
let union_1 = makeAliasingContextTest
    "union with empty context"
    (AliasingContext.union ctx ctx') ctx

let construction_1 ctxt =
  skip_if true "unimplemented"
(* let construction_1 = makeAliasingContextTest
    "super simple construction"
    let ctx = *)

let equality_1 = makeAliasingContextTest
    "super simple equality"
    { parent=None;
      scope=Scope 0;
      props=AliasPropSet.empty;
      children=[] }
    { parent=None;
      scope=Scope 1;
      props=AliasPropSet.empty;
      children=[] }

let suite : test =
  "aliasing" >:::
  [ "equality 1" >:: equality_1
  ; "construction 1" >:: construction_1
  ; "union 1" >:: union_1  ]
