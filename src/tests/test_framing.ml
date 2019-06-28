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
  classes=[
    {
      id          = "A";
      super       = "Object";
      fields      = [ { type_=Int; id="f" } ];
      predicates  = [];
      methods     = [];
    }
  ];
  statement=Sequence{ statements=[
      Declaration{ type_=Class"A"; id="x" };
      Declaration{ type_=Class"A"; id="y" };
    ]};
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

let makeNotSelfFramingTest (phi:concrete) : test_fun =
  let clsctx = ClassContext.copy clsctx_stock in
  let typctx = TypeContext.copy typctx_stock in
  makeFalseTest ~sexp_of_t:sexp_of_formula (fun phi -> selfFrames clsctx typctx phi) (Concrete phi)

let expr_of_bool b : expression = Value(Bool b)
let expr_of_int  i : expression = Value(Int (Int32.of_int_exn i))
let expr_of_id  id : expression = Value(Object id)
let phi_of_bool  b : concrete   = Expression(expr_of_bool b)
let phi_of_expr  e : concrete   = Expression e

let suite : test =
  "framing" >::: [
    "self-frames: true" >:: makeSelfFramingTest
      (phi_of_bool true);

    "self-frames: acc(x.f)" >:: makeSelfFramingTest
      (Access_check({ base=expr_of_id"x"; field="f" }));

    "doesn't self-frame: x.f=1" >:: makeNotSelfFramingTest
      (phi_of_expr @@ Comparison{
          comparer = Eq;
          left  = Field_reference{ base=expr_of_id"x"; field="f" };
          right = expr_of_int 1;
        });

    "self-frames: acc(x.f) * x.f=1" >:: makeSelfFramingTest
      begin
        Operation{
          operator = Sep;
          left  = (Access_check({ base=expr_of_id"x"; field="f" }));
          right = (phi_of_expr @@ Comparison{
              comparer = Eq;
              left  = Field_reference{ base=expr_of_id"x"; field="f" };
              right = expr_of_int 1;
            });
        }
      end;

    "self-frames: x=y * acc(x.f) * acc(x.y)" >:: makeSelfFramingTest
      begin
        Operation{
          operator = Sep;
          left  = (phi_of_expr @@ Comparison{ comparer=Eq; left=expr_of_id"x"; right=expr_of_id"y" });
          right = Operation{
              operator = Sep;
              left  = (Access_check{ base=expr_of_id"x"; field="f" });
              right = (Access_check{ base=expr_of_id"y"; field="f" });
            }
        }
      end;

    (* TODO: fails (aliasing) *)
    "self-frames: x=y * acc(x.f) * y.f=1" >:: makeSelfFramingTest
      begin
        Operation{
          operator = Sep;
          left  = (phi_of_expr @@ Comparison{ comparer=Eq; left=expr_of_id"x"; right=expr_of_id"y" });
          right = Operation{
              operator = Sep;
              left  = (Access_check{ base=expr_of_id"x"; field="f" });
              right = (phi_of_expr @@ Comparison{
                  comparer = Eq;
                  left  = (Field_reference{ base=expr_of_id"y"; field="f" });
                  right = (expr_of_int 1);
                });
            }
        }
      end;

    "self-frames: if x=null then true else acc(x.f) * x.f=1" >:: makeSelfFramingTest
      begin
        (* TODO *)
        phi_of_bool true
      end

  ]
