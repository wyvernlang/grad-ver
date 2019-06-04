open Core
open Ast_types
open Utility
open Wellformed

let prgm = {
  classes =
    [];
  statement = join_statements [
    Declaration{ type_=Int; id={string="id"} };
    Assignment{ id={string="x"}; value=Value(Int{value=0l}) }
  ]
}

let _ =
  checkProgram prgm
