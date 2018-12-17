
(* This should be made more general *)
module type S = sig
  type t

  val sat : Formula.t -> bool
  val valid : Formula.t -> Formula.t -> bool
end

