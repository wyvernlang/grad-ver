
open Core

module R = Rules
module I = Idf

(*
 * Sample program (psuedo):
 *
 * class C { int x; }
 *
 * int main()
 *   requires x > 0;
 *   ensures x > 1;
 * {
 *    C z = new C();
 *    int a = 1;
 *    z.x = a;
 *
 *    int y = z.x;
 *    x = x + y;
 * }
 *
 *)

let program =
  let open Ast in
  let id = identifier in
  { classes = [{ name = id "C"
              ;  super = id "Top"
              ;  fields = [Int, id "x"]
              ;  abspreds = []
              ;  methods = []
              }]
  ; stmts = [ NewObj (id "z", id "C")
            ; Assign (Int, id "a", Val (Num 1))
            ; Fieldasgn (id "z", id "x", id "a")
            ; Assign (Int, id "y", FieldAccess (Var (id "z"), id "x"))
            ; Assign (Int, id "x", Binop (Var (id "x"), Plus, Var (id "y")))
            ]
  }

let precondition =
  let open Ast in
  let id = identifier in
  Cmpf (Var (id "x"), Gt, Val (Num 0))

let postcondition =
  let open Ast in
  let id = identifier in
  Cmpf (Var (id "x"), Gt, Val (Num 1))

