(** {1 Satisfiability} *)

open Core
open Utility
open Functools
open Ast
open Wellformed
open Aliasing
open Z3tools
open Z3

(** {2 Satisfiability Context} *)

module SatisfiabilityContext :
sig
  type t = {
    clsctx : ClassContext.t;
    typctx : TypeContext.t;
    scpctx : ScopingContext.t;
    z3ctx : Z3Context.t;
    z3exs : Expr.expr list; (** not directly added to the Z3Context so that they can be managed functionally
                                alongside nesting *)
    acc_z3exs : Expr.expr list; (** kept separate so that they can be reset across '^'s in formulas,
                                    where overlapping accesses are allowed *)
  }

  (** accessors *)
  val getZ3Exs : t -> Expr.expr list

  (** mutators *)
  val addZ3Ex : t -> Expr.expr -> t
  val addAccZ3Ex : t -> Expr.expr -> t
  val removeAccZ3Exs : t -> t
  val addZ3ExIfSatisfiable : t -> z3ex -> t option

  (* satisfiability *)
  val isSatisfiable : t -> bool
  val isSatisfiableWith : t -> z3ex -> bool
  val checkSatisfiability : t -> t option (** [Some satctx] indicates a satisfiabile result.
                                              [None] indicates an unsatisfiable result. *)
end
module SatContext = SatisfiabilityContext

(** {2 Z3 expressions} *)

(** Converts an Ast.expression to a Z3 expression *)
val z3ex_of_expression : SatContext.t -> expression -> Expr.expr

(* Cheecks the satisfiability of a concrete formula while converting it into corresponding Z3 expressions,
   which are added to the SatContext.
   [Some satctx] indicates a satisfiable result.
   [None] indicates an unsatisfiable result. *)
val processConcrete : SatContext.t -> concrete -> SatContext.t option

(** {2 Satisfiability} *)

(** Checks the satisfiability of the given SatContext processed with the given concrete formula. *)
val isSatisfiableConcrete : SatContext.t -> concrete -> bool

(** Checks the satisfiablility of the given formula, within the given contexts. *)
val isSatisfiable : ClassContext.t -> TypeContext.t -> ScopingContext.t -> formula -> bool
