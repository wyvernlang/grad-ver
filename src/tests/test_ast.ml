open OUnit2
open Core
open Ast_types
open Ast

let string_of_program (prgm:Ast.program) : string =
  string_of_sexp @@ sexp_of_program @@ prgm

let makeWrapTest (rawPrgm:Ast_types.program) (correctPrgm:Ast.program) =
  fun ctxt ->
  assert_equal
    ~msg:("wrap result: "^(string_of_sexp @@ sexp_of_program @@ wrap rawPrgm))
    ~printer:(fun prgm -> string_of_sexp @@ sexp_of_program prgm)
    (wrap rawPrgm) correctPrgm

(* just to make sure equality works the way I think it does *)
let wrap_tmp ctxt =
  let prgm1 = { classes=[]; statement=Skip } in
  let prgm2 = { classes=[]; statement=Assertion{ concrete=Expression(Value(Bool(false))) } } in
  assert_equal
    ~msg:("comparing:\n - "^string_of_program prgm1^"\n - "^string_of_program prgm2)
    prgm1 prgm2

let suite : test =
  "ast" >:::
  [ "wrap_" >:: wrap_tmp ]
