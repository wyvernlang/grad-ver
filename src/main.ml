
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
    let module WLP = R.MakeWLP(Sat.Z3) in
    let phi = WLP.wlp s (Rules.convertFormula postcondition) in
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
    prerr_endline "SAFE"
  with Virtheap.Unknown t -> prerr_endline @@
          "Internal error tracking heap aliases: unknown cell " ^ F.pp_term t
     | F.Unsat -> prerr_endline @@ "UNSAFE"
end

