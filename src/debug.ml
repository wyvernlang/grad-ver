open Core
(* open Sexplib.Sexp *)
(* open Sexplib.Std *)
open Ast

type t = X of { x:int }  [@@deriving sexp]
(* type t = { x:int } [@@deriving sexp] *)

let _ =
  (* print_endline @@ Sexp.to_string @@ sexp_of_t @@ X {x=1} *)
  (* print_endline @@ string_of_sexp @@ sexp_of_concrete @@ Expression(Value(Bool(true))); *)

  let phi = Expression(Value(Bool(true))) in
  let conc = Concrete(phi) in
  let impr = Imprecise(phi) in
  print_endline @@ Sexp.to_string_hum @@ sexp_of_class_
    { id="class id";
      super="class super";
      fields=[{ type_=Int; id="field id" }];
      predicates=[{
          id="predicate id";
          arguments=[{ type_=Int; id="argument id" }];
          formula=conc }];
      methods=[{
          type_=Int;
          id="method id";
          arguments=[{ type_=Int; id="argument id" }];
          dynamic={ requires=impr; ensures=impr };
          static={ requires=impr; ensures=impr };
          body=Skip;
        }] };
