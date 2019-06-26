open OUnit2
open Core
open Ast
open Aliasing
open Functools
open Utility

open Test_utility

let makeAliasingContextTest : AliasingContext.t -> AliasingContext.t -> test_fun =
  makeEqualityTest ~cmp:AliasingContext.equal ~sexp_of_t:sexp_of_aliasingcontext

let ofId   (s:string) : ObjectValue.t = ObjectValue.of_objectvalue @@ Value(Object s)
let ofInt  (i:int)    : ObjectValue.t = ObjectValue.of_objectvalue @@ Value(Int(Int32.of_int_exn i))
let ofBool (b:bool)   : ObjectValue.t = ObjectValue.of_objectvalue @@ Value(Bool b)
(* let ofL  : objectvalue list -> aliasprop = AliasProp.of_list *)
(* let ofLL : objectvalue list list -> AliasPropSet.t = aliaspropset_of_objectvalue_list_list *)

(*--------------------------------------------------------------------------------------------------------------------------*)
(* propositional entailment *)
(*--------------------------------------------------------------------------------------------------------------------------*)

module Entailment =
struct
  let assert_entails     ps p ctxt = assert_bool "entails"            @@ AliasProp.entails ps p
  let assert_not_entails ps p ctxt = assert_bool "not entails" @@ not @@ AliasProp.entails ps p

  let suite : test =
    "aliasing propositional entailment" >::: [
      (* "empty does not entail aliased{o1,o2}" >:: assert_not_entails (ofLL[[]]) (ofL[ ofId"o1"; ofId"o2" ]) *)
    ]
end

(*--------------------------------------------------------------------------------------------------------------------------*)
(* merging *)
(*--------------------------------------------------------------------------------------------------------------------------*)

module Merging =
struct
  let empty : AliasingContext.t = {
    scope=Scope 0; parent=None; children=[];
    props=AliasPropSet.empty;
  }

  let single : AliasingContext.t = {
    scope=(Scope 0); parent=None; children=[];
    (* props=(ofLL[ [ofId"o1"; ofId"o2"] ]); *)
    props=AliasPropSet.singleton @@ AliasProp.of_list [ofId"o1"; ofId"o2"];
  }

  let _ = print_endline @@ "[!!]"^
                           Sexp.to_string @@ AliasPropSet.sexp_of_t single.props;;

  let suite : test =
    "merging" >::: [
      "union" >::: [
        (* "empty union empty = empty" >:: makeAliasingContextTest (AliasingContext.union empty empty) empty; *)
        "C union empty = C"         >:: makeAliasingContextTest (AliasingContext.union empty single) single;
        (* "C union C = C"             >:: makeAliasingContextTest (AliasingContext.union single single) single; *)
      ];
      "inter" >::: [
        (* "empty inter empty = empty" >:: makeAliasingContextTest (AliasingContext.inter empty empty) empty; *)
        (* "C inter empty = empty"     >:: makeAliasingContextTest (AliasingContext.inter single empty) empty; *)
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
    Entailment.suite;
    Merging.suite;
    Construction.suite;
  ]
