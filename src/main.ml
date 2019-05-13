
open Core
open Functools

module R = Rules
module I = Idf

(* TODO: set up the initial pipeline
 *)

(* checkStatic : program -> unit
 *)
let checkStatic program = begin
  let module F = Formula in
  let _ = Wellformed.init program in
  let _ = Wellformed.processStms program.Ast.stmts in
  let s =
    List.fold_right ~f:(fun s acc -> Ast.Seq(s, acc)) ~init:Ast.Skip
    program.Ast.stmts
  in
  try
    let module WLP = R.MakeWLP(Sat.Z3) in
    let next (type a) : a F.t -> unit = function
      | (F.Static phi) as f ->
          begin
          let _ = prerr_endline @@
            "Inferred weakest precondition: " ^ F.pp_phi f
          in
          let _ = prerr_endline @@
            "Checking that " ^ F.pp_formula phi ^ " is self-framed"
          in
          if Idf.selfFramed phi
            then ()
            else raise F.Unsat;
          let _ = prerr_endline @@
            "Checking that " ^ F.pp_formula phi ^ " is valid"
          in
          if WLP.sat (F.Static phi)
            then ()
            else raise F.Unsat;
          prerr_endline "SAFE"
          end
      | (F.Gradual phi) as f ->
          begin
          let _ = prerr_endline @@
            "Inferred weakest precondition: " ^ F.pp_phi f
          in
          let _ = prerr_endline @@
            "Checking that " ^ F.pp_phi f ^ " is satisfiable"
          in
          if WLP.sat (F.Gradual phi)
            then ()
            else raise F.Unsat;
          prerr_endline "SAFE"
          end
   in
   WLP.gradualWLP s (F.Static F.True) {Rules.k=next}
   with F.Unsat -> prerr_endline @@ "UNSAFE"
      | Virtheap.Unknown t ->
          prerr_endline @@ "internal error tracking heap aliases"
end

open Ast
let id = identifier

let withdrawBody = [ Declare (Int, id "b")
                   ; Assign (id "b", FieldAccess (Var (id "inp"), id "balance"))
                   ; Declare (Int, id "rest")
                   ; Assign (id "rest", Binop (Var (id "b"), Minus, Var (id "amt")))
                   ; Fieldasgn (id "inp", id "balance", id "rest")
                   ]

let withdrawPre =
  Sep (Access (Var (id "inp"), id "balance"),
  Cmpf (FieldAccess (Var (id "inp"), id "balance"), Ge, Var (id "amt")))

let withdrawPost =
  Sep (Access (Var (id "inp"), id "balance"),
  Cmpf (FieldAccess (Var (id "inp"), id "balance"), Ge, Val (Num 0)))

let withdraw =
  { name=id "withdraw"
  ; out_type=Any
  ; args=[(Cls (id "C"), id "inp"); (Int, id "amt")]
  ; dynamic={ requires=Concrete (Cmpf(Val(Num 1), Eq, Val (Num 1)))
            ; ensures=Concrete (Cmpf(Val(Num 1), Eq, Val (Num 1)))}
  ; static={ requires=Concrete withdrawPre
           ; ensures=Concrete withdrawPost
           }
  ; body=List.fold_right ~f:(fun s acc -> Seq (s,acc)) ~init:Skip withdrawBody
  }

(*
 * Sample program (psuedo):
 *
 * class C {
 *   int balance;
 *
 *   void withdraw(C inp, int amt)
 *     requires acc(inp.balance) * inp.balance >= amt
 *     ensures acc(inp.balance) * inp.balance >= 0
 *   {
 *     inp.balance -= amt;
 *   }
 * }
 *
 * int main() {
 *   C bank;
 *   bank.balance = 100;
 *   bank.withdraw (bank, 50);
 *   bank.withdraw (bank, 30);
 * }
 *
 *)

let bankprg =
  { classes = [{ name = id "C"
              ;  super = id "Top"
              ;  fields = [Int, id "balance"]
              ;  abspreds = []
              ;  methods = [withdraw]
              }]
  ; stmts = [ Declare (Cls (id "C"), id "bank")
            ; Declare (Int, id "init")
            ; Declare (Int, id "wd1")
            ; Declare (Int, id "wd2")
            ; Declare (Any, id "_")
            ; NewObj (id "bank", id "C")
            ; Assign (id "init", Val (Num 100))
            ; Assign (id "wd1", Val (Num 30))
            ; Assign (id "wd2", Val (Num 50))
            ; Fieldasgn (id "bank", id "balance", id "init")
            ; Call { target=id "_"
                   ; base=id "bank"
                   ; methodname=id "withdraw"
                   ; args=[id "bank"; id "wd1"]
                   }
            ; Call { target=id "_"
                   ; base=id "bank"
                   ; methodname=id "withdraw"
                   ; args=[id "bank"; id "wd2"]
                   }
            ]
  }

let _ =
  prerr_endline @@ pp_formula withdrawPre;
  prerr_endline "Verifying Bank Program:";
  checkStatic bankprg;
  prerr_endline ""

