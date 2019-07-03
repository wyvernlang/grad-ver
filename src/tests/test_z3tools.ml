open Core
open OUnit2
open Z3
open Z3.Arithmetic

let basic = "basic" >:: fun ctxt ->
    let cfg = [ "model","true" ; "proof","false" ] in
    let ctx = mk_context cfg in
    let int_sym = Symbol.mk_int ctx 42 in
    let str_sym = Symbol.mk_string ctx "mySymbol" in
    let bool_sort = Boolean.mk_sort ctx in
    let int_sort = Integer.mk_sort ctx in
    let real_sort = Real.mk_sort ctx in
    Printf.printf "\n";
    Printf.printf "int symbol:    %s\n" @@ Symbol.to_string int_sym;
    Printf.printf "string symbol: %s\n" @@ Symbol.to_string str_sym;
    Printf.printf "bool sort:     %s\n" @@ Sort.to_string bool_sort;
    Printf.printf "int sort:      %s\n" @@ Sort.to_string int_sort;
    Printf.printf "real sort:     %s\n" @@ Sort.to_string real_sort

let bools = "bools" >:: fun ctxt ->
    let cfg = [ "model","true" ; "proof","false" ] in
    let ctx = mk_context cfg in
    let t : Expr.expr = Boolean.mk_val ctx true in
    let f : Expr.expr = Boolean.mk_val ctx false in
    let s : Solver.solver = Solver.mk_solver ctx None in
    Solver.add s [ t; f; ];
    Printf.printf "\n";
    Printf.printf "solver: %s\n" @@ Solver.to_string s;
    Printf.printf "check:  %s\n" @@ Solver.string_of_status @@ Solver.check s [ t; f; ]

let formula1 = "formula 1" >:: fun ctxt ->
    (* let cfg = [ "model","true" ; "proof","false" ] in *)
    let ctx = mk_context [] in
    let p = Boolean.mk_const_s ctx "p" in
    let np = Boolean.mk_not ctx p in
    let s = Solver.mk_simple_solver ctx in
    Solver.add s [ p; np; ];
    Printf.printf "\n";
    Printf.printf "solver: %s\n" @@ Solver.to_string s;
    Printf.printf "check:  %s\n" @@ Solver.string_of_status @@ Solver.check s []

    let suite () : test =
      "z3tools" >::: [
        (* basic; *)
        (* bools; *)
        formula1;
      ]
