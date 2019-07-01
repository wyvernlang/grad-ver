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

(* expressions *)

(*-------------------------------------------------------------------------------------------------------------------------*)

(* constructors *)
let expr_of_bool b : expression = Value(Bool b)
let expr_of_int  i : expression = Value(Int (Int32.of_int_exn i))
let expr_of_id  id : expression = Value(Object id)
let expr_eq   e e' : expression = Comparison{ comparer=Eq; left=e; right=e' }
(* stock *)
let x = expr_of_id"x"
let y = expr_of_id"y"
let xdotf : expression = Field_reference{ base=x; field="f" }
let ydotf : expression = Field_reference{ base=y; field="f" }
let one = expr_of_int 1
let two = expr_of_int 2
let three = expr_of_int 3

(*-------------------------------------------------------------------------------------------------------------------------*)
(* formulas *)

(* constructors *)
let phi_of_bool  b : concrete   = Expression(expr_of_bool b)
let phi_of_expr  e : concrete   = Expression e
let phi_eq    e e' : concrete   = phi_of_expr @@ expr_eq e e'
let phi_sep   p p' : concrete   = Operation{ operator=Sep; left=p; right=p' }
(* stock *)
let acc_xdotf : concrete = Access_check{ base=x; field="f" }
let acc_ydotf : concrete = Access_check{ base=y; field="f" }


(*-------------------------------------------------------------------------------------------------------------------------*)
(* written examples *)
(*-------------------------------------------------------------------------------------------------------------------------*)
(* the following examples are from svlrp.pdf *)

(* constants *)
let phi_true = phi_of_bool true
let phi_false = phi_of_bool false
let expr_zero = expr_of_int 0
let expr_one = expr_of_int 1

(* variables *)
let x = expr_of_id"x"
let y = expr_of_id"y"
let xf : expression = Field_reference{ base=x; field="f" }
let yf : expression = Field_reference{ base=y; field="f" }
let acc_xf = Access_check{ base=x; field="f" }
let acc_yf = Access_check{ base=y; field="f" }

(* constructors *)
let eq_expr l r : expression = Comparison{ comparer=Eq; left=l; right=r }
let eq_phi l r : concrete = phi_of_expr @@ eq_expr l r
let sep_phi l r : concrete = Operation{ operator=Sep; left=l; right=r }
let and_phi l r : concrete = Operation{ operator=And; left=l; right=r }
let join_sep_phi : concrete list -> concrete = List.fold_left ~init:phi_true ~f:sep_phi
let (-*-) = sep_phi
let (-^-) = and_phi

(* casts *)

(*--------------------------------------------------------------------------------------------------------------------------*)

let examples = [

  "example 1 (self-framing)" >:: makeSelfFramingTest
    begin
      eq_phi x y -*- acc_xf -*- acc_yf
    end;

  "example 2 (self-framing)" >:: makeSelfFramingTest
    begin
      acc_xf -*-
      (If_then_else{
          condition = eq_expr xf expr_one;
          then_ = (phi_true, Scope 1);
          else_ = (acc_xf, Scope 2);
        })
    end

]

(*-------------------------------------------------------------------------------------------------------------------------*)
(* suite *)
(*-------------------------------------------------------------------------------------------------------------------------*)

let suite () : test =
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

    "self-frames: x=y * acc(x.f) * y.f=1" >:: makeSelfFramingTest
      begin
        Operation{
          operator = Sep;
          left  = (phi_of_expr @@ Comparison{ comparer=Eq; left=expr_of_id"x"; right=expr_of_id"y" });
          right = Operation{
              operator = Sep;
              (* left  = phi_of_bool true; *)
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
        If_then_else{
          condition = (expr_eq x y);
          then_ = ((phi_of_bool true), Scope 0);
          else_ = ((phi_sep
                      acc_xdotf
                      (phi_eq xdotf one)),
                  Scope 0);
        }
      end
  ] @ examples
