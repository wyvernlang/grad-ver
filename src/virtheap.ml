
(* Heap objects are tracked using unique IDs.
 *
 * We can ensure that the ID of any given heap object is unique by hiding a
 * global counter behind the module interface and incrementing it only when
 * we know we're allocating more.
 *
 * The heap itself is tracked via an association list of (t, c) where t is the
 * term (either `Var s` or `Field (e, f)`) and c is the associated object on
 * the heap.
 *)

open Core
open Functools

module LM = List.Assoc
module F = Formula

type term = Formula.term

exception Unknown of term
let counter = ref 0

type cell = int
type t = (term * cell) list

module Access = struct
  type t = cell * string
  include Comparable.Make(
    struct type t = int * string [@@deriving compare, sexp] end
  )
end

module Cell = Int

let fresh () =
  let x = !counter in
  counter := !counter + 1; x

let cellId i = i
let cellEq = Int.equal

let empty = []

let add ctx s = LM.add ctx ~equal:F.termEq s @@ fresh ()

let lmlookup = LM.find ~equal:F.termEq

let add_new ctx s =
  match lmlookup ctx s with
  | None -> add ctx s
  | Some _ -> ctx

let alias ctx s1 s2 =
  match lmlookup ctx s2 with
  | Some i -> LM.add ctx ~equal:F.termEq s1 i
  | None ->
      let i = fresh () in
      LM.add (LM.add ctx ~equal:F.termEq s1 i) ~equal:F.termEq s2 i

let lookup ctx s =
  match lmlookup ctx s with
  | Some i -> i
  | None -> raise @@ Unknown s

let same ctx s1 s2 =
  match lmlookup ctx s1, lmlookup ctx s2 with
  | Some a, Some b -> cellEq a b
  | None, _ -> raise @@ Unknown s1
  | _, None -> raise @@ Unknown s2

