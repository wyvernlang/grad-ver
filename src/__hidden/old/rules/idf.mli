
module type IMPLIES = sig
  type 'a t1
  type t2
  val (=>) : 'a t1 Formula.t -> t2 Formula.t -> bool
end

module MakeIDF(S : Sat.S) : sig
  module Precise : IMPLIES with type 'a t1 = Formula.precise
                            and type t2 = Formula.precise

  module Imprecise : IMPLIES with type 'a t1 = 'a
                              and type t2 = Formula.imprecise
end

val rmField : Formula.formula -> Formula.term * string -> Formula.formula

val rmVar : Formula.formula -> Formula.term -> Formula.formula

val minFramePhi : Formula.formula -> Formula.formula

val selfFramed : Formula.formula -> bool

