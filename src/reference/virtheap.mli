exception Unknown of Formula.term
type cell
type t

module Access : Core.Comparable.S with type t = cell * string
module Cell : Core.Comparable.S with type t = cell

val cellId : cell -> int
val cellEq : cell -> cell -> bool

val empty : t

val add : t -> Formula.term -> t
val add_new : t -> Formula.term -> t
val alias : t -> Formula.term -> Formula.term -> t

val lookup : t -> Formula.term -> cell

