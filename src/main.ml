
open Core
open Functools

module R = Rules
module I = Idf

(* TODO: set up the initial pipeline
 *)

(* check : program -> formula -> formula -> unit
 *)
let check program = begin
  let module F = Formula in
  (* Because we don't have real parameter instantiation yet, we need to set
   * any initial variables by hand.
   *)
  let _ = Wellformed.init program in
  (*
  let s =
    List.fold_right ~f:(fun s acc -> Ast.Seq(s, acc)) ~init:Ast.Skip
    program.Ast.stmts
  in
    *)
  try
    (*
    let module WLP = R.MakeWLP(Sat.Z3) in
    let phi = WLP.staticWLP s (Rules.convertFormula postcondition) in
    let _ = prerr_endline @@
      "Inferred weakest precondition: " ^ F.pp_formula phi
    in
    let _ = prerr_endline @@
      "Checking that " ^ F.pp_formula phi ^ " is self-framed"
    in
    if Idf.selfFramed phi
      then ()
      else raise F.Unsat;
    let _ = prerr_endline @@
      "Checking that " ^ Ast.pp_formula precondition ^ " ==> " ^
      F.pp_formula phi ^ " is valid"
    in
    if WLP.verify (Rules.convertFormula precondition) phi
      then ()
      else raise F.Unsat;
      *)
    prerr_endline "SAFE"
  with F.Unsat -> prerr_endline @@ "UNSAFE"
     | Virtheap.Unknown t ->
          prerr_endline @@ "internal error tracking heap aliases"
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
 *    int a;
 *    a = 1;
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

open Ast
let id = identifier

let fbody = [ Declare (Cls (id "C"), id "z")
            ; NewObj (id "z", id "C")
            ; Declare (Int, id "a")
            ; Assign (id "a", Val (Num 1))
            ; Fieldasgn (id "z", id "x", id "a")
            ; Declare (Cls (id "C"), id "w")
            ; Assign (id "w", Var (id "z"))
            ; Declare (Int, id "y")
            ; Assign (id "y", FieldAccess (Var (id "z"), id "x"))
            ; Declare (Int, id "y2")
            ; Assign (id "y2", FieldAccess (Var (id "w"), id "x"))
            ; Assert (Cmpf (Var (id "y"), Eq, Var (id "y2")))
            ; Declare (Int, id "x")
            ; Assign ( id "x", Binop (Var (id "x"), Plus, Var (id "y")))
            ]

let precondition1 =
  let open Ast in
  let id = identifier in
  Cmpf (Var (id "x"), Gt, Val (Num 0))

let postcondition1 =
  let open Ast in
  let id = identifier in
  Cmpf (Var (id "x"), Gt, Val (Num 2))

let mthd =
  { name=id "f"
  ; out_type=Int
  ; args=[(Int, id "x")]
  ; dynamic={ requires=Concrete (Cmpf(Val(Num 1), Eq, Val (Num 1)))
            ; ensures=Concrete (Cmpf(Val(Num 1), Eq, Val (Num 1)))}
  ; static={ requires=Concrete precondition1
           ; ensures=Concrete postcondition1
           }
  ; body=List.fold_right ~f:(fun s acc -> Seq(s,acc)) ~init:Skip fbody
  }

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

let program =
  let open Ast in
  let id = identifier in
  { classes = [{ name = id "C"
              ;  super = id "Top"
              ;  fields = [Int, id "x"]
              ;  abspreds = []
              ;  methods = [mthd]
              }]
  ; stmts = [ Declare (Cls (id "C"), id "w")
            ; Assign (id "w", Var (id "z"))
            ; Declare (Int, id "y")
            ; Assign (id "y", FieldAccess (Var (id "z"), id "x"))
            ; Declare (Int, id "y2")
            ; Assign (id "y2", FieldAccess (Var (id "w"), id "x"))
            ; Assert (Cmpf (Var (id "y"), Eq, Var (id "y2")))
            ; Declare (Int, id "x")
            ; Assign (id "x", Binop (Var (id "x"), Plus, Var (id "y")))
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
  prerr_endline ""

