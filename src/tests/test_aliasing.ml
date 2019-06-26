open OUnit2
open Core
open Ast
open Aliasing
open Functools
open Utility

open Test_utility

let makeAliasingContextTest : AliasingContext.t -> AliasingContext.t -> test_fun =
  makeEqualityTest ~cmp:AliasingContext.equal ~sexp_of_t:sexp_of_aliasingcontext

let makeAliasPropSetTest : AliasPropSet.t -> AliasPropSet.t -> test_fun =
  makeEqualityTest ~cmp:AliasPropSet.equal ~sexp_of_t:AliasPropSet.sexp_of_t

let id   (s:string) : ObjectValue.t = ObjectValue.of_objectvalue @@ Value(Object s)
let int  (i:int)    : ObjectValue.t = ObjectValue.of_objectvalue @@ Value(Int(Int32.of_int_exn i))
let bool (b:bool)   : ObjectValue.t = ObjectValue.of_objectvalue @@ Value(Bool b)
let ofL  : ObjectValue.t list -> AliasProp.t = AliasProp.of_list
let ofLL : ObjectValue.t list list -> AliasPropSet.t = aliaspropset_of_objectvalue_list_list

let o1, o2, o3, o4, o5, o6, o7, o8, o9 = id"o1", id"o2", id"o3", id"o4", id"o5", id"o6", id"o7", id"o8", id"o9"

(*--------------------------------------------------------------------------------------------------------------------------*)
(* context equality *)
(*--------------------------------------------------------------------------------------------------------------------------*)

module Equality =
struct
  let suite : test =
    "equality" >::: [
      "AliasPropSet.from_list" >::: [
        "from_list[] = empty" >:: makeAliasPropSetTest
          (AliasPropSet.of_list[])
          (AliasPropSet.empty);

        "from_list[ aliased{o1,o2} ] = from_list[ aliased{o1,o2} ]" >:: makeAliasPropSetTest
          (AliasPropSet.of_list[ AliasProp.of_list[ o1;o2 ] ])
          (AliasPropSet.of_list[ AliasProp.of_list[ o1;o2 ] ])
      ]
    ]
end

(*--------------------------------------------------------------------------------------------------------------------------*)
(* propositional entailment *)
(*--------------------------------------------------------------------------------------------------------------------------*)

module Entailment =
struct
  let assert_entails     ps p ctxt = assert_bool "entails"            @@ AliasProp.entails ps p
  let assert_not_entails ps p ctxt = assert_bool "not entails" @@ not @@ AliasProp.entails ps p

  let suite : test =
    "(alias)propositional entailment" >::: [
      "{ } |- { } " >:: assert_entails
        (AliasPropSet.of_list[])
        (AliasProp.of_list[]);

      "{ } not|- aliased{o1,o2}" >:: assert_not_entails
        (AliasPropSet.of_list[])
        (AliasProp.of_list[ o1;o2 ]);

      "{ aliased{o1,o2} } |- aliased{ }" >:: assert_entails
        (AliasPropSet.of_list[ AliasProp.of_list[ o1;o2 ] ])
        (AliasProp.of_list[]);
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
    props=AliasPropSet.of_list[ AliasProp.of_list[ o1;o2 ] ];
  }

  let suite : test =
    "merging" >::: [

      "union" >::: [
        "empty union empty = empty" >:: makeAliasingContextTest (AliasingContext.union empty empty) empty;
        "C union C = C"             >:: makeAliasingContextTest (AliasingContext.union single single) single;
        "C union empty = C"         >:: makeAliasingContextTest (AliasingContext.union single empty) single;
      ];

      "inter" >::: [
        "empty inter empty = empty" >:: makeAliasingContextTest (AliasingContext.inter empty empty) empty;
        "C inter empty = empty"     >:: makeAliasingContextTest (AliasingContext.inter single empty) empty;
      ]
    ]
end

(*--------------------------------------------------------------------------------------------------------------------------*)
(* construction *)
(*--------------------------------------------------------------------------------------------------------------------------*)

module Construction =
struct
  (* < {}, {} > *)
  let empty = { scope=Scope 0; parent=None; children=[]; props=AliasPropSet.empty; }
  (* < props, {} > *)
  let toplevel props = { scope=Scope 0; parent=None; children=[]; props=props; }

  (* true *)
  let trivial = Expression(Value(Bool true))
  (* o = o' *)
  let alias o o' = Expression(Comparison{ comparer=Eq; left=o; right=o' })
  (* o != o' *)
  let nonalias o o' = Expression(Comparison{ comparer=Neq; left=o; right=o' })

  let suite : test =
    "construction" >::: [
      "trivial" >:: makeAliasingContextTest
        (AliasingContext.construct @@ Concrete(trivial))
        empty;
      "o = o'" >:: makeAliasingContextTest
        (AliasingContext.construct @@ Concrete(alias (Value(Object"o1")) (Value(Object"o2"))))
        (toplevel @@ AliasPropSet.of_list[ AliasProp.of_list[ o1;o2 ] ] )
    ]
end

(*--------------------------------------------------------------------------------------------------------------------------*)
(* suite *)
(*--------------------------------------------------------------------------------------------------------------------------*)

let suite : test =
  "aliasing context" >::: [
    Equality.suite;
    Entailment.suite;
    Merging.suite;
    Construction.suite;
  ]
