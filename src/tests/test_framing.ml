open OUnit2
open Core

open Utility
open Test_utility
open Functools
open Ast
open Aliasing
open Wellformed
open Framing

(* utilities *)

(*-------------------------------------------------------------------------------------------------------------------------*)

(* constructors *)
let expr_of_bool b : expression = Value(Bool b)
let expr_of_int  i : expression = Value(Int (Int32.of_int_exn i))
let expr_of_id  id : expression = Value(Object id)
let eq_expr   e e' : expression = Comparison{ comparer=Eq; left=e; right=e' }
let neq_expr  e e' : expression = Comparison{ comparer=Neq; left=e; right=e' }
(* stock *)
let one = expr_of_int 1
let two = expr_of_int 2
let three = expr_of_int 3

(*-------------------------------------------------------------------------------------------------------------------------*)
(* formulas *)

(* variables *)
let x = expr_of_id"x"
let y = expr_of_id"y"
let z = expr_of_id"z"
let l = expr_of_id"l"
let xf : expression = Field_reference{ base=x; field="f" }
let yf : expression = Field_reference{ base=y; field="f" }
let zf : expression = Field_reference{ base=z; field="f" }
let acc_xf = Access_check{ base=x; field="f" }
let acc_yf = Access_check{ base=y; field="f" }
let lhead : expression = Field_reference { base=l; field="head" }
let ltail : expression = Field_reference { base=l; field="tail" }
let acc_lhead = Access_check { base=l; field="head" }
let acc_ltail = Access_check { base=l; field="head" }

(* constructors *)
let phi_of_bool  b : concrete   = Expression(expr_of_bool b)
let phi_of_expr  e : concrete   = Expression e
let phi_eq    e e' : concrete   = phi_of_expr @@ eq_expr e e'
let phi_sep   p p' : concrete   = Operation{ operator=Sep; left=p; right=p' }
(* stock *)
let acc_xf : concrete = Access_check{ base=x; field="f" }
let acc_yf : concrete = Access_check{ base=y; field="f" }

(* constants *)
let true_phi = phi_of_bool true
let false_phi = phi_of_bool false
let zero_expr = expr_of_int 0
let one_expr = expr_of_int 1
let null_expr : expression = Value null_value


(* constructors *)
let eq_expr l r : expression = Comparison{ comparer=Eq; left=l; right=r }
let eq_phi l r : concrete = phi_of_expr @@ eq_expr l r
let neq_phi l r : concrete = phi_of_expr @@ neq_expr l r
let sep_phi l r : concrete = Operation{ operator=Sep; left=l; right=r }
let and_phi l r : concrete = Operation{ operator=And; left=l; right=r }
let join_sep_phi : concrete list -> concrete = List.fold_left ~init:true_phi ~f:sep_phi
let (-*-) = sep_phi
let (-^-) = and_phi

(*--------------------------------------------------------------------------------------------------------------------------*)

let program_stock = {
  classes=[
    {
      id          = "A";
      super       = "Object";
      fields      = [ { type_=Int; id="f" } ];
      predicates  = [];
      methods     = [];
    };
    {
      id = "List";
      super = "Object";
      fields = [
        { type_=Int; id="head" };
        { type_=Class"List"; id="tail" };
      ];
      predicates = [
        { id="List"; arguments=[ { type_=Class"List"; id="l" } ];
          formula=Concrete
              begin
                neq_phi l null_expr -*-
                acc_lhead -*-
                acc_ltail -*-
                (If_then_else{ (* TODO: figure out scopes *)
                    condition=eq_expr ltail null_expr;
                    then_=true_phi, Scope (-1);
                    else_=Predicate_check{ predicate="List"; arguments=[ l ]; class_=Some "List" }, Scope (-1)
                  })
              end
        }
      ];
      methods = [];
    }
  ];
  statement=Sequence{ statements=[
      Declaration{ type_=Class"A"; id="x" };
      Declaration{ type_=Class"A"; id="y" };
      Declaration{ type_=Class"A"; id="z" };
      Declaration{ type_=Class"List"; id="l" };
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
  makeFalseTest ~sexp_of_t:sexp_of_formula
    (fun phi -> selfFrames clsctx typctx phi)
    (Concrete phi)

(*-------------------------------------------------------------------------------------------------------------------------*)
(* written examples *)
(*-------------------------------------------------------------------------------------------------------------------------*)
(* the following examples are from svlrp.pdf *)

let examples = [

  "example 1 (self-framing)" >:: makeSelfFramingTest
    begin
      eq_phi x y -*- acc_xf -*- acc_yf
    end;

  "example 2 (self-framing)" >:: makeSelfFramingTest
    begin
      acc_xf -*-
      (If_then_else{
          condition = eq_expr xf one_expr;
          then_ = (true_phi, Scope 1);
          else_ = (acc_xf, Scope 2);
        })
    end;

  "example 3 (self-framing)" >:: makeSelfFramingTest
    begin
      acc_xf -*- eq_phi x y -*- eq_phi yf one_expr
    end;

  (* TODO: fails *)
  (* "example 4 (self-framing)" >:: makeSelfFramingTest
     begin
      Predicate_check{ predicate="List"; arguments=[ l ]; class_=Some "List" } -*-
      Unfolding_in{
        predicate_check={ predicate="List"; arguments=[ l ]; class_=Some "List" };
        formula=(eq_phi lhead one_expr, Scope 3) (* TODO: figure out correct scope *)
      }
     end *)

  "example 5 (self-framing)" >:: makeSelfFramingTest
    begin
      If_then_else{
        condition=eq_expr x null_expr;
        then_=true_phi, Scope 1;
        else_=acc_xf -*- eq_phi xf one_expr, Scope 2;
      }
    end;

  (* TODO: fails --- the ite's with the same condition should merge *)
  (* "example 6 (self-framing)" >:: makeSelfFramingTest
     begin
      let phi1 : concrete = If_then_else{ condition=eq_expr xf one_expr;
                                          then_=eq_phi x y, Scope 1;
                                          else_=true_phi, Scope 4 } in
      let phi2 : concrete = If_then_else{ condition=eq_expr xf one_expr;
                                          then_=eq_phi yf one_expr, Scope 3;
                                          else_=true_phi, Scope 5 } in
      acc_xf -*- phi1 -*- phi2
     end; *)

  "example 7 (not self-framing)" >:: makeNotSelfFramingTest
    begin
      If_then_else{
        condition=eq_expr x y;
        then_=acc_xf, Scope 1;
        else_=eq_phi xf one_expr, Scope 2;
      }
    end;

  "example 8 (not self-framing)" >:: makeNotSelfFramingTest
    begin
      acc_xf -*-
      Expression(BOr{ left=eq_expr x y; right_enscoped=eq_expr x z, Scope 1 }) -*-
      Expression(BOr{ left=eq_expr yf one_expr; right_enscoped=eq_expr zf one_expr, Scope 2 })
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

    (* TODO: fails with: `Worker stops running: Killed by signal -5` *)
    (* "self-frames: if x=null then true else acc(x.f) * x.f=1" >:: makeSelfFramingTest
       begin
        If_then_else{
          condition = (eq_expr x y);
          then_ = ((phi_of_bool true), Scope 0);
          else_ = ((phi_sep
                      acc_xf
                      (phi_eq xf one)),
                  Scope 0);
        }
       end *)
  ] @ examples
