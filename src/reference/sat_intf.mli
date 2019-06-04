
(* This should be made more general *)
module type S = sig
  val sat : Formula.formula -> bool
  val valid : Formula.formula -> bool
  val implies : Formula.formula -> Formula.formula -> bool
end

