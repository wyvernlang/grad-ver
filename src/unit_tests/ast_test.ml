open OUnit2
open Core
open Ast_types
open Ast

let hello_world ctxt =
  assert_equal
    ~msg:"hello = hello"
    "hello" "hello"
  (* assert_equal
    ~msg:"hello = world"
    "hello" "world" *)

let wrap1 ctxt =
  let raw_ast : Ast_types.program =
    let phi : Ast_types.concrete = Expression (Value (Bool true)) in
    { classes=[];
      statement=Assertion{ concrete=phi } }
  in
  let correct_ast : Ast.program =
    let phi : Ast.concrete = Expression (Value (Bool true)) in
    { classes=[];
      statement=Assertion{ concrete=phi } }
  in
  assert_equal
    ~msg:("wrapping: "^(string_of_sexp @@ sexp_of_program correct_ast))
    ~printer:(fun prgm -> string_of_sexp @@ sexp_of_program prgm)
    (* ~cmp:syneqProgram *)
    (* { classes=[] ; statement=Skip } correct_ast *)
    (wrapAST raw_ast) correct_ast

let suite = "ast" >::: [ "wrap1" >:: wrap1 ]
