
open Core

module A = Ast
module S = Sat
module F = Formula

let verifyStmts (module S : Sat.S) prec postc stmts =
  let s = List.fold_right ~f:(fun s acc -> A.Seq(s, acc)) ~init:A.Skip stmts in
  let phi = Rules.wlp s (Rules.convertFormula postc) in
  let _ = if F.checkAcc phi then () else raise F.Unsat in
  if S.valid prec phi then () else raise F.Unsat

