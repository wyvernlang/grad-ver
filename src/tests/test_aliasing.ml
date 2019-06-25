open OUnit2
open Core
open Ast
open Aliasing
open Functools
open Utility

open Test_utility

let makeAliasingContextTest : AliasingContext.t -> AliasingContext.t -> test_fun =
  makeEqualityTest ~cmp:AliasingContext.equal ~sexp_of_t:AliasingContext.sexp_of_t

(*--------------------------------------------------------------------------------------------------------------------------*)
(* stock contexts *)
(*--------------------------------------------------------------------------------------------------------------------------*)

let ctx_empty : AliasingContext.t =
  { scope=Scope 0; parent=None; children=[];
    props=AliasPropSet.empty;
  }

let ctx_singleton : AliasingContext.t =
  { scope=Scope 0; parent=None; children=[];
    props=(ofObjectValueSetEltListList[ [ObjectValueSetElt.Value(Bool true)] ]);
  }

(*--------------------------------------------------------------------------------------------------------------------------*)
(* merging *)
(*--------------------------------------------------------------------------------------------------------------------------*)

module Merging =
struct
  let suite : test =
    "merging" >::: [
      "union" >::: [
        "singleton union empty = singleton" >:: makeAliasingContextTest
          (AliasingContext.union ctx_empty ctx_singleton) ctx_singleton;
      ];
      "intersection" >::: [
        (* TODO *)
      ]
    ]
end

(*--------------------------------------------------------------------------------------------------------------------------*)
(* construction *)
(*--------------------------------------------------------------------------------------------------------------------------*)

module Construction =
struct
  let suite : test =
    "construction" >::: [
        (* TODO *)
    ]
end

(*--------------------------------------------------------------------------------------------------------------------------*)
(* suite *)
(*--------------------------------------------------------------------------------------------------------------------------*)

let suite : test =
  "aliasing context" >::: [
    Merging.suite;
    Construction.suite;
  ]
