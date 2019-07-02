open OUnit2
open Core
open Ast
open Wellformed
open Aliasing
open Functools
open Utility

open Test_utility

let makeAliasingContextTest scpctx : AliasingContext.t -> AliasingContext.t -> test_fun =
  makeEqualityTest ~cmp:(AliasingContext.equal scpctx) ~sexp_of_t:sexp_of_aliasingcontext

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
  let suite () : test =
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

  let suite () : test =
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

  let suite () : test =
    "merging" >::: [

      "union" >::: [
        "empty union empty = empty" >::
        makeAliasingContextTest (ScopingContext.create ())
          (AliasingContext.union empty empty)
          empty;
        "C union C = C" >::
        makeAliasingContextTest (ScopingContext.create ())
          (AliasingContext.union single single)
          single;
        "C union empty = C" >::
        makeAliasingContextTest (ScopingContext.create ())
          (AliasingContext.union single empty)
          single;
      ];

      "inter" >::: [
        "empty inter empty = empty" >::
        makeAliasingContextTest (ScopingContext.create ())
          (AliasingContext.inter empty empty)
          empty;
        "C inter empty = empty" >:: makeAliasingContextTest (ScopingContext.create ())
          (AliasingContext.inter single empty)
          empty;
      ]
    ]
end

(*--------------------------------------------------------------------------------------------------------------------------*)
(* construction *)
(*--------------------------------------------------------------------------------------------------------------------------*)

module Construction =
struct
  let empty_class id : class_ = { id=id; super="Object"; fields=[]; predicates=[]; methods=[]; }

  let clsctx : ClassContext.t =
    ClassContext.construct {
      classes=[ empty_class "A"; empty_class "B" ];
      statement=Skip
    }

  let typctx : TypeContext.t =
    let typctx = TypeContext.create () in
    TypeContext.constructStatement clsctx typctx @@ Sequence{statements=[
        Declaration{ type_=Class"A"; id="o1" };
        Declaration{ type_=Class"A"; id="o2" };
      ]};
    typctx

  let o1 : expression = Value(Object "o1")
  let o2 : expression = Value(Object "o2")
  let o1_elt : ObjectValueSet.Elt.t = ObjectValue.of_objectvalue @@ Value(Object "o1")
  let o2_elt : ObjectValueSet.Elt.t = ObjectValue.of_objectvalue @@ Value(Object "o2")

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

  let suite () : test =
    "construction" >::: [
      "trivial" >:: makeAliasingContextTest (ScopingContext.create ())
        (AliasingContext.construct clsctx typctx (ScopingContext.create ())
           (Concrete(trivial)))
        empty;

      "o = o'" >:: makeAliasingContextTest (ScopingContext.create ())
        (AliasingContext.construct clsctx typctx (ScopingContext.create ())
           (Concrete(alias o1 o2)))
        (toplevel @@ AliasPropSet.of_list[ AliasProp.of_list[ o1_elt;o2_elt ] ]);

      "if true then o = o' else o = o'" >::
      let scpctx = (ScopingContext.create ()) in
      makeAliasingContextTest scpctx
        (* formula to test *)
        (AliasingContext.construct clsctx typctx (ScopingContext.create ()) @@ Concrete
           begin
             If_then_else{
               condition=Value(Bool true);
               then_=(Expression(Comparison{ comparer=Eq; left=o1; right=o2 }), Scope 1);
               else_=(Expression(Comparison{ comparer=Eq; left=o1; right=o2 }), Scope 2);
             }
           end)
        (* correct aliasing context *)
        begin
          ScopingContext.add scpctx (Scope 1)
            { parent=Some (Scope 1); scope=Scope 1; children=[];
              props=AliasPropSet.singleton(AliasProp.of_list[ o1_elt;o2_elt ]) };
          ScopingContext.add scpctx (Scope 2)
            { parent=Some (Scope 2); scope=Scope 2; children=[];
              props=AliasPropSet.singleton(AliasProp.of_list[ o1_elt;o2_elt ]) };
          { parent=None; scope=Scope 0; props=AliasPropSet.empty;
            children=[ Condition(Value(Bool true)), Scope 1;
                       Condition(Value(Bool false)), Scope 2 ]; }
        end
    ]
end

(*--------------------------------------------------------------------------------------------------------------------------*)
(* suite *)
(*--------------------------------------------------------------------------------------------------------------------------*)

let suite () : test =
  "aliasing context" >::: [
    Equality.suite ();
    Entailment.suite ();
    Merging.suite ();
    Construction.suite ();
  ]
