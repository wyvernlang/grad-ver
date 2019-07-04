open Core
open Z3
open Z3tools
open Utility
open Functools

open Wellformed
open Aliasing
open Framing
open Ast
(*
val toZ3Expr : ScopingContext.t -> formula -> Expr.expr

(** Checks whether the given formula is satifiable.
    The ScopingContext is required to access the aliasing contexts of nested scopes *)
val isSatisfiable : ScopingContext.t -> formula -> bool *)
