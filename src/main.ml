
open Core
open Functools

module R = Rules
module I = Idf

(* run : program -> formula -> formula -> (string * type) list -> unit
 *)
let run program precondition postcondition inputs = begin
  let module F = Formula in
  (* Because we don't have real parameter instantiation yet, we need to set
   * any initial variables by hand.
   *)
  let _ =
    Core.List.map ~f:(fun (s,t) -> Hashtbl.set Wellformed.varctx s t) inputs in
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
    let _ = prerr_endline @@
      "Checking that " ^ Ast.pp_formula precondition ^ " ==> " ^
      F.pp_formula phi ^ " is valid"
    in
    if Sat.Z3.valid (Rules.convertFormula precondition) phi
      then ()
      else raise F.Unsat;
    prerr_endline "SAFE"
  with F.Heap.Unknown t -> prerr_endline @@
          "Internal error tracking heap aliases: unknown cell " ^ F.pp_term t
     | F.Unsat -> prerr_endline @@ "UNSAFE"
end

(*
 * Sample program (psuedo):
 *
 * class C { int x; }
 *
 * int main()
 *   requires x > 0;
 *   ensures x > 2;
 * {
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

let inputs1 = ["x", Ast.Int]

(*
 * Sample program (psuedo):
 *
 * class C { int x; }
 *
 * int main()
 *   requires z != NULL * acc(z.x) * x > 0;
 *   ensures x > 2;
 * {
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

let program2 =
  let open Ast in
  let id = identifier in
  { classes = [{ name = id "C"
              ;  super = id "Top"
              ;  fields = [Int, id "x"]
              ;  abspreds = []
              ;  methods = []
              }]
  ; stmts = [ Assign (Cls (id "C"), id "w", Var (id "z"))
            ; Assign (Int, id "y", FieldAccess (Var (id "z"), id "x"))
            ; Assign (Int, id "y2", FieldAccess (Var (id "w"), id "x"))
            ; Assert (Cmpf (Var (id "y"), Eq, Var (id "y2")))
            ; Assign (Int, id "x", Binop (Var (id "x"), Plus, Var (id "y")))
            ]
  }

let precondition2 =
  let open Ast in
  let id = identifier in
  Sep
  ( Sep
    ( Cmpf (Var (id "z"), Neq, Val Nil)
    , Access (Var (id "z"), id "x")
    )
  , Cmpf (Var (id "x"), Gt, Val (Num 0))
  )

let postcondition2 =
  let open Ast in
  let id = identifier in
  Cmpf (Var (id "x"), Gt, Val (Num 1))

let inputs2 = ["x", Ast.Int; "z", Ast.Cls (Ast.identifier "C")]

let _ =
  prerr_endline "Program 1:";
  run program1 precondition1 postcondition1 inputs1;
  prerr_endline ""

(* This will fail with an internal error, as I don't have a way to set an
 * initial heap. However, the inferred precondition should be correct.
 *
 * I don't think the rules (or at least, my implementation thereof) have a
 * mechanism for "collapsing" inferred access predicates when we find out that
 * two things are actually aliased, so this will give a duplicate acc(z.x).
 *)
let _ =
  prerr_endline "Program 2:";
  run program2 precondition2 postcondition2 inputs2;
  prerr_endline ""


