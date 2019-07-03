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

  val create : unit -> t

  val isSatisfiable : t -> bool

  val addZ3Expr : t -> Expr.expr -> unit

end
