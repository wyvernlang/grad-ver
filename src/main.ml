open Core

open Utility
open Functools
open Z3tools
open Ast_types
open Ast_pp
open Ast
open Wellformed
open Aliasing
open Framing
open Satisfiability

(* let prgm = {
  classes = [];
  statement = Sequence { statements = [
      Declaration{ type_=Int; id="x" };
      Assignment{ id="x"; value=Value(Int 0l)};
      Assignment{ id="x"; value=Value(Bool true)};
    ] }
}

let _ =
  checkProgram prgm;
  Printf.printf "\n\n=========================\nprogram is wellformed :)\n=========================\n\n";
  let fmt = Format.std_formatter in
  Ast_pp.pp_program fmt prgm;
  Out_channel.newline stdout *)
