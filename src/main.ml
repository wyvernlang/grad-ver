open Core
open Ast_types
open Ast_pp
open Utility
open Wellformed

let prgm = {
  classes = [];
  statement = Sequence { statements = [
      Declaration{ type_=Int; id="x" };
      Assignment{ id="y"; value=Value(Int 0l)};
    ] }
}

let _ =
  (* checkProgram prgm; *)
  Ast_pp.pp_program Format.std_formatter prgm;
  print_endline "program is wellformed"
