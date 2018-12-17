
open Core
open Functools

module R = Rules
module I = Idf

(*
 * Sample program (psuedo):
 *
 * class C { int x; }
 *
 * int main()
 *   requires x > 0;
 *   ensures x > 2;
 * {
 *    C z = null;
 *    z = new C();
 *    int a = 1;
 *    z.x = a;
 *
 *    C w = z;
 *
 *    int y = w.x;
 *    int y2 = z.x;
 *    assert (y == y2);
 *
 *    x = x + y + y2;
 * }
 *
 *)
let program1 =
  let open Ast in
  let id = identifier in
  { classes = [{ name = id "C"
              ;  super = id "Top"
              ;  fields = [Int, id "x"]
              ;  abspreds = []
              ;  methods = []
              }]
  ; stmts = [ Assign (Cls (id "C"), id "z", Val Nil)
            ; NewObj (id "z", id "C")
            ; Assign (Int, id "a", Val (Num 1))
            ; Fieldasgn (id "z", id "x", id "a")
            ; Assign (Cls (id "C"), id "w", Var (id "z"))
            ; Assign (Int, id "y", FieldAccess (Var (id "z"), id "x"))
            ; Assign (Int, id "y2", FieldAccess (Var (id "w"), id "x"))
            ; Assert (Cmpf (Var (id "y"), Eq, Var (id "y2")))
            ; Assign (Int, id "x", Binop (Var (id "x"), Plus, Var (id "y")))
            ]
  }

let precondition1 =
  let open Ast in
  let id = identifier in
  Cmpf (Var (id "x"), Gt, Val (Num 0))

let postcondition1 =
  let open Ast in
  let id = identifier in
  Cmpf (Var (id "x"), Gt, Val (Num 1))

let run program precondition postcondition = begin
  let module F = Formula in
  let _ = Hashtbl.set Wellformed.varctx "x" Ast.Int in
  let _ = Wellformed.processProgram program in
  let _ = prerr_endline "Program confirmed well-formed" in
  let s =
    List.fold_right ~f:(fun s acc -> Ast.Seq(s, acc)) ~init:Ast.Skip
    program.Ast.stmts
  in
  try
    let phi = Rules.wlp s (Rules.convertFormula postcondition) in
    let _ = prerr_endline @@
      "Inferred weakest precondition: " ^ F.pp_formula phi
    in
    let _ = if F.checkAcc phi then () else raise F.Unsat in
    if Sat.Z3.valid precondition phi then () else raise F.Unsat;
    prerr_endline "SAFE"
  with F.Heap.Unknown t -> prerr_endline @@
          "Error tracking heap aliases: unknown cell " ^ F.pp_term t
     | F.Unsat -> prerr_endline @@ "UNSAFE"
end

