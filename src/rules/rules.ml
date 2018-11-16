
open Core
open Functools

module A = Ast

(* TODO: This should probably be refactored into its own module *)
type context = Z3.context

let varctx = String.Table.create ()

let mk_div' ctx = function
  | [e1; e2] -> Z3.Arithmetic.mk_div ctx e1 e2
  | _ -> assert false

let rec liftOp = function
  | A.Plus -> Z3.Arithmetic.mk_add
  | A.Minus -> Z3.Arithmetic.mk_sub
  | A.Times -> Z3.Arithmetic.mk_mul
  | A.Div -> mk_div'

let constructVar ctx v =
  match Hashtbl.find varctx (A.name v) with
  | Some s -> s
  | None ->
      let v_symb =
        Z3.Expr.mk_const ctx (Z3.Symbol.mk_string ctx (A.name v))
        (Z3.Arithmetic.Integer.mk_sort ctx)
      in
      Hashtbl.set varctx (A.name v) v_symb;
      v_symb

let rec lift ctx e = match e with
| A.Binop (e1, op, e2) -> (liftOp op) ctx [lift ctx e1 ; lift ctx e2]
  (* TODO: We need a nice way to encode nil/classes into Z3/SAT bindings. *)
| A.Val (A.Num i) -> Z3.Arithmetic.Integer.mk_numeral_i ctx i
| A.Var v -> constructVar ctx v

(* XXX - I'm not sure I'm using Z3 correctly at all *)
let rec wlp s (ctx : context) phi = match s with
| A.Skip -> phi
| A.Seq (s1, s2) -> wlp s1 ctx (wlp s2 ctx phi)
| A.Assign (t, v, e) ->
    (* This should be some Z3 encoding of an access predicate *)
    let acc = raise (Failure "TODO") in
    let phi' = Z3.Expr.substitute_vars phi [lift ctx e] in
    Z3.Boolean.mk_ite ctx
    (Z3.Boolean.mk_implies ctx phi' acc)
    phi' @@
    (* TODO: This should be a proper separation term *)
    Z3.Boolean.mk_and ctx [acc;phi']
    (* TODO: Check that WLP(T x := e, phi) * x = e ==> phi *)
    (* TODO: Check that WLP(T x := e, phi) is satisfiable *)
| A.Fieldasgn (x, f, y) ->
    (* This should be some Z3 encoding of an access predicate *)
    let acc = raise (Failure "TODO") in
    (* We need a way to encode arbitrary field access as a variable to be
     * substituted. Maybe something that's otherwise invalid, like x#f.
     *)
    let phi' = raise (Failure "TODO") in
    Z3.Boolean.mk_ite ctx
    (Z3.Boolean.mk_implies ctx phi' acc)
    phi' @@
    (* TODO: This should be a proper separation term *)
    Z3.Boolean.mk_and ctx [acc;phi']
    (* TODO: Check that WLP(x.f := y, phi) * x.f = y ==> phi *)
    (* TODO: Check that WLP(x.f := y, phi) is satisfiable *)


