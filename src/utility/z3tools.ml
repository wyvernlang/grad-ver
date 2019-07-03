open Core
open Z3
open Utility

open Ast
open Wellformed
open Aliasing

module Z3Context =
struct

  type t = {
    context : context;
    solver : Solver.solver;
  }

  let create () : t =
    let cfg = [] in
    let ctx = mk_context [] in
    let slv = ctx in
    {
      context = ctx;
      solver  = slv;
    }

  let isSatisfiable z3ctx : bool =
    Solver.check z3ctx.solver []

  let addZ3Expr z3ctx expr : unit =
    match Solver.add z3ctx.solver [expr] with
    | SATISFIABLE   -> true
    | UNSATISFIABLE -> false
    | UNKNOWN       -> false (* TODO: could treat this as true *)

end
