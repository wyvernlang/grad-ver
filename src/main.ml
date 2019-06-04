open Core
open Ast_types
open Ast_pp
open Utility
open Wellformed

let prgm = {
  classes=[];
  statement=join_statements [
      Declaration{ type_=Int; id={string="x"}};
      Assignment{ id={string="y"}; value=Value(Int{value=0l})};
      (* Assignment{ id={string="x"}; value=Value(Objectid{string="A"})} *)
    ]
}

let _ =
  (* checkProgram prgm; *)
  Ast_pp.pp_program Format.std_formatter prgm;
  print_endline "program is wellformed"
