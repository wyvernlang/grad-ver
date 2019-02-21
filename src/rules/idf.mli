
module MakeIDF(S : Sat.S) : sig val (=>) : Formula.t -> Formula.t -> bool end

val minFramePhi : Formula.t -> Formula.t

val selfFramed : Formula.t -> bool

