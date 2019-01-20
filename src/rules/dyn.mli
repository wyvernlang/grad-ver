
type expop = Formula.expop =
  | Plus
  | Minus
  | Times
  | Div

type cmpop = Formula.cmpop =
  | Neq
  | Eq
  | Lt
  | Gt
  | Le
  | Ge

type term = Formula.term =
  | Var of string
  | Num of int
  | Null
  | Cls
  | Field of term * string
  | Binop of term * expop * term

type formula = Formula.t =
  | True
  | Cmp of term * cmpop * term
    (* a1 = a2 *)
  | Alias of term * term
  | NotEq of term * term
  | Alpha of term list
  | Access of term * string
  | Sep of formula * formula

val dynFootprint : Virtheap.t -> formula -> Virtheap.Access.Set.t
val collectAliases : Virtheap.t -> formula -> Virtheap.t
val frames : formula -> term -> bool
val fuseAccs : Formula.t -> Formula.t -> formula
val checkAcc : formula -> bool

