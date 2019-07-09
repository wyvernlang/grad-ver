open OUnit2
open Core
open Ast
open Wellformed
open Aliasing
open Functools
open Utility
open Satisfiability

open Test_utility

(*--------------------------------------------------------------------------------------------------------------------------*)
(* stock *)
(*--------------------------------------------------------------------------------------------------------------------------*)

(*--------------------------------------------------------------------------------------------------------------------------*)
(* expressions *)

let bool   b     : expression = Value(Bool b)
let int    i     : expression = Value(Int i)
let obj    id    : expression = Value(Object id)
let fldref id f  : expression = Field_reference{ base=obj id; field=f }
let comp   c e f : expression = Comparison{ comparer=c; left=e; right=f }
let oper   o e f : expression = Operation { operator=o; left=e; right=f }

(*--------------------------------------------------------------------------------------------------------------------------*)
(* concretes *)

let expr e         : concrete = Expression e
let operConc o a b : concrete = Operation{ operator=o; left=a; right=b }
let con = operConc And
let sep = operConc Sep
let acc      id f  : concrete = Access_check{ base=obj id; field=f }

(*--------------------------------------------------------------------------------------------------------------------------*)
(* test constructor *)

let prgm : program = {
  classes=[
    {
      id         = "A";
      super      = "Object";
      fields     = [
        { type_=Int; id="i" };
        { type_=Bool; id="b" };
      ];
      predicates = [];
      methods    = [];
    }
  ];
  statement=Sequence{statements=[
      Declaration{ type_=Class "A"; id="x" };
      Declaration{ type_=Class "A"; id="y" };
      Declaration{ type_=Class "A"; id="z" };
    ]};
}

let makeSatisfiabilityTest (phi:concrete) : test_fun =
  let clsctx = ClassContext.construct prgm in
  let typctx = TypeContext.create () in
  TypeCheck.check typctx prgm;
  let scpctx = ScopingContext.create () in
  (* let alictx = AliasingContext.construct clsctx typctx scpctx @@ Concrete phi in *)
  Test_utility.makeTruthTest
    ~sexp_of_t:sexp_of_formula
    (isSatisfiable clsctx typctx scpctx)
    (Concrete phi)

let makeUnsatisfiabilityTest (phi:concrete) : test_fun =
  let clsctx = ClassContext.construct prgm in
  let typctx = TypeContext.create () in
  TypeCheck.check typctx prgm;
  let scpctx = ScopingContext.create () in
  (* let alictx = AliasingContext.construct clsctx typctx scpctx @@ Concrete phi in *)
  Test_utility.makeTruthTest
    ~sexp_of_t:sexp_of_formula
    (not @< isSatisfiable clsctx typctx scpctx)
    (Concrete phi)

(* suite *)

let suite () : test =
  "satisfiability" >::: [
    "SAT: true" >:: makeSatisfiabilityTest @@
    expr@@bool true;

    "UNSAT: false" >:: makeUnsatisfiabilityTest @@
    expr@@bool false;

    "SAT: true ^ true" >:: makeSatisfiabilityTest @@
    Operation{ operator=And; left=expr@@bool true; right=expr@@bool true };

    "SAT: true * true" >:: makeSatisfiabilityTest @@
    Operation{ operator=Sep; left=expr@@bool true; right=expr@@bool true };

    "SAT: x.b" >:: makeSatisfiabilityTest @@
    expr @@ fldref "x" "b";

    "SAT: x.i = 1" >:: makeSatisfiabilityTest @@
    expr@@comp Eq (fldref "x" "i") (int 1);

    "SAT: x = y" >::
    makeSatisfiabilityTest @@
    expr@@comp Eq (obj "x") (obj "y");

    "SAT: (x = y) * acc(x.i)" >::
    makeSatisfiabilityTest @@
    sep
      (expr@@comp Eq (obj "x") (obj "y"))
      (acc "x" "i");

    "UNSAT: (x = y) * acc(x.i) * acc(y.i)" >::
    makeUnsatisfiabilityTest @@
    sep
      (expr@@comp Eq (obj "x") (obj "y"))
      (sep
         (acc "x" "i")
         (acc "y" "i"));

    "SAT: acc(x.i) * acc(y.i)" >::
    makeSatisfiabilityTest @@
    sep
      (acc "x" "i")
      (acc "y" "i");

    "UNSAT: acc(x.i) * acc(x.i)" >::
    makeUnsatisfiabilityTest @@
    sep
      (acc "x" "i")
      (acc "x" "i");

    (* TODO: fails, looks like formula And doesn't work the way its suppose to... *)
    (* "SAT: (x = y) * acc(x.i) * acc(y.i)" >::
    makeSatisfiabilityTest @@
    sep
      (expr@@comp Eq (obj "x") (obj "y"))
      (con
         (acc "x" "i")
         (acc "y" "i")); *)

  ]
