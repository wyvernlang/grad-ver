open OUnit2
open Core
open Ast_types
open Ast
open Utility
open Functools
open Wellformed

open Test_utility

(* make tests *)

let makeProgramTest : Ast.program -> Ast.program -> test_fun =
  makeEqualityTest ~cmp:(=) ~sexp_of_t:sexp_of_program

let makeClassContextTest : ClassContext.t -> ClassContext.t -> test_fun =
  makeEqualityTest ~cmp:ClassContext.equal ~sexp_of_t:ClassContext.sexp_of_t

let makeTypeContextTest : TypeContext.t -> TypeContext.t -> test_fun =
  makeEqualityTest ~cmp:TypeContext.equal ~sexp_of_t:TypeContext.sexp_of_t

let makeTypeCheckingTest sexp_of_t : ('a -> unit) -> 'a -> test_fun =
  makeUnitTest ~sexp_of_t

let makeNotTypeCheckingTest (ex:exn) sexp_of_t : ('a -> unit) -> 'a -> test_fun =
  makeExceptionTest ~ex ~sexp_of_t

(* program utilities *)

let empty_prgm : program = { classes=[]; statement=Skip; }

let single_cls_prgm cls : program = { empty_prgm with classes=[cls]; }

let expr_int (i:int) : expression = Value(Int(Int32.of_int_exn i))
let expr_bool (b:bool) : expression = Value(Bool b)

module ClassContextConstruction =
struct
  let empty () = ClassContext.create ()

  let empty_class : class_ = {
    id = "Empty";
    super = "Object";
    fields = [];
    predicates = [];
    methods = []
  }

  let simple_class : class_ = {
    id = "Simple";
    super = "Object";
    fields = [ { type_=Int; id="x" } ];
    predicates = [];
    methods = [];
  }

  let single cls = let ctx = empty () in Hashtbl.set ctx ~key:cls.id ~data:cls; ctx

  (* TODO: a problem is that somehow the hashtable for the first test is getting the class from the second test... *)
  let suite : test =
    "class context construction" >::: [
      (* "construct(class Empty {})" >:: makeClassContextTest
        (ClassContext.construct @@ single_cls_prgm empty_class)
        begin
          let cls = empty_class in
          let ctx : ClassContext.t = ClassContext.create () in
          Hashtbl.set ctx ~key:cls.id ~data:cls;
          ctx
        end; *)

      (* "construct(class Simple { Int x })" >:: makeClassContextTest
        (ClassContext.construct @@ single_cls_prgm simple_class)
        (single simple_class) *)
    ]
end

module TypeContextConstruction =
struct

  let empty () : TypeContext.t = TypeContext.create ()
  let single id typ : TypeContext.t = let ctx = empty () in TypeContext.setIdType ctx id typ; ctx
  let list (decls:((id*type_) list)) : TypeContext.t =
    let ctx = empty () in
    List.iter decls ~f:(fun ((id, typ):id*type_) -> TypeContext.setIdType ctx id typ);
    ctx

  let construct stmt : TypeContext.t =
    let clsctx = ClassContext.create () in
    let typctx = empty () in
    TypeContext.constructStatement clsctx typctx stmt;
    typctx

  let suite : test =
    "type context construction" >::: [
      (* "Skip" >:: makeTypeContextTest
        (construct Skip)
        (empty ());

      "Int x" >:: makeTypeContextTest
        (construct @@ Declaration{ type_=Int; id="x" })
        (single "x" Int);

      "Int x; x := 1" >:: makeTypeContextTest
        (construct @@ Sequence{ statements=[
             Declaration{ type_=Int; id="x" };
             Assignment{ id="x"; value=expr_int 1 }
           ]})
        (single "x" Int);

      "if (true) then { Int x } else { Bool x }" >:: makeTypeContextTest
        (construct @@ If_then_else{
            condition=expr_bool true;
            then_=Declaration{ type_=Int; id="x" };
            else_=Declaration{ type_=Bool; id="x" };
          })
        (empty ()); *)
    ]
end

module TypeChecking =
struct
  let makeCheckingStatementTest = makeTypeCheckingTest sexp_of_statement
      (TypeCheck.checkStatement (ClassContext.create ()) (TypeContext.create ()))

  let makeNotCheckingStatementTest ex = makeNotTypeCheckingTest ex sexp_of_statement
      (TypeCheck.checkStatement (ClassContext.create ()) (TypeContext.create ()))

  let checkStatement (stmt:statement) =
    let clsctx = ClassContext.create () in
    let typctx = TypeContext.create () in
    TypeCheck.checkStatement clsctx typctx stmt

  let suite : test =
    "type checking" >::: [
      "Int x; x := 1" >:: makeCheckingStatementTest
        (Sequence{ statements=[
          Declaration{ type_=Int; id="x" };
          Assignment{ id="x"; value=expr_int 1 }
        ]});

      "Int x; x := true" >:: makeNotCheckingStatementTest
        (Type_mismatch_assignment ("z", Int, Bool))
        (Sequence{ statements=[
             Declaration{ type_=Int; id="z" };
             Assignment{ id="z"; value=expr_bool true }
           ]});

      "if (true) { Skip } else { Skip }" >:: makeCheckingStatementTest
        (If_then_else{
          condition=expr_bool true;
          then_=Skip;
          else_=Skip;
        });

      "if (1) { Skip } else { Skip }" >:: makeNotCheckingStatementTest
        (Nonbool_if_condition (expr_int 1))
        (If_then_else{
          condition=expr_int 1;
          then_=Skip;
          else_=Skip;
        });

    ]
end

let suite : test =
  "wellformed" >::: [
    ClassContextConstruction.suite;
    TypeContextConstruction.suite;
    TypeChecking.suite;
  ]
