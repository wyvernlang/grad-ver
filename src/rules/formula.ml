(* Contains all the logic for access predicates, etc.
 *
 * TODO: Conversion to Z3, checking of access predicates
 *)

open Core
open Functools

module A = Ast

let abspred = Failure "Abstract predicates unimplemented!"

type expop = A.expop =
  | Plus
  | Minus
  | Times
  | Div

type cmpop = A.cmpop =
  | Neq
  | Eq
  | Lt
  | Gt
  | Le
  | Ge

(* This doesn't actually need to be GADT-ified. The fact that Var and Field are
 * still _ -> 'a constructors bothers me a lot, but I'm not quite sure how to
 * resolve that beyond not exposing these constructors (but in that case, why
 * bother with GADTs at all?)
 *)
(* TODO: method calls *)
type _ term =
  | Var : string -> 'a term
  | Num : int -> int term
  | Null : A.cls term
  | Cls : A.cls term
  | Field : A.cls term * string -> 'a term
  | Binop : int term * expop * int term -> int term
  (* These constructors are a filthy hack -- we need to unify the types of all
   * the arguments to, say, an abstract predicate invocation, but OCaml doesn't
   * support type families (or general type-level programming like Haskell), so
   * We instead accomplish this by temporarily erasing the type of the
   * underlying term, then restoring it with a pattern match later on.
   *)
  | ReflI : int term -> unit term
  | ReflC : A.cls term -> unit term

let rec termEq : type v.(v term -> v term -> bool) = fun t1 t2 ->
  match t1, t2 with
  | Var s1, Var s2 -> String.equal s1 s2
  | Num n1, Num n2 -> Int.equal n1 n2
  | Null, Null -> true
  | Field (b1, f1), Field (b2, f2) -> termEq b1 b2 && String.equal f1 f2
  | Binop (l1, op1, r1), Binop (l2, op2, r2) ->
      termEq l1 l2 && phys_equal op1 op2 && termEq r1 r2
  | ReflI t1', ReflI t2' -> termEq t1' t2'
  | ReflC t1', ReflC t2' -> termEq t1' t2'
  | Cls, Cls -> false
  | _ -> false

type access = A.cls term * string

let rec accessEq (t1, s1) (t2, s2) = termEq t1 t2 && String.equal s1 s2

type t = True
       | Cmp of int term * cmpop * int term
         (* a1 = a2 *)
       | Alias of access * access
       | NotEq of A.cls term * A.cls term
       | Alpha of unit term list
       | Access of access
       | Sep of t * t

let intVar s : string term = Var s
let clsVar s : A.cls term = Var s

let intConst i : int term = Num i

let rec acc (type v) (t : v term) = match t with
| Var _ -> True
| Num _ -> True
| Null -> True
| Cls -> True
| Field (e, f) -> Access (e, f)
| Binop _ -> True
| ReflI _ -> True
| ReflC c -> acc c

let rec staticFootprint s = match s with
| True | Cmp _ | NotEq _ | Alias _ -> []
| Alpha _ -> raise abspred
| Access a -> [a]
| Sep (t1, t2) ->
    Listutil.union ~eq:accessEq (staticFootprint t1) (staticFootprint t2)

(* XXX - name is misleading, we should be cleaner than just returning the
 *       alias association list
 *)
let rec dynFootprint ctx s = match s with
| True | Cmp _ | NotEq _ -> ctx, []
| Alpha _ -> raise abspred
| Alias (a1, a2) ->
    Listutil.changeWhen ~p:(List.exists ~f:(accessEq a2) @< snd)
    ~f:(fun (a,als) -> (a, Listutil.union ~eq:accessEq [a1] als)) ctx, []
| Sep (s1, s2) ->
    let lctx, r1 = dynFootprint ctx s1 in
    let rctx, r2 = dynFootprint lctx s2 in
    rctx, Listutil.union ~eq:accessEq r1 r2
| Access a ->
    match List.find ~f:(List.exists ~f:(accessEq a) @< snd) ctx with
    | Some (a', ls) -> ctx, [a']
    | None -> (a,[a])::ctx, [a]

let aliases heap ft = match List.find ~f:(accessEq ft @< fst) heap with
| Some (a, als) -> als
  (* misformed footprint/heap pair *)
| None -> assert false

(* XXX - instead of having all these auxiliary methods for substitution, it
 *       might be cleaner to have v/t' be real existential types
 *)

let rec substI' v t t' = match t with
| Var v' -> if String.equal v' @@ A.name v then t' else t
| Binop (l, oper, r) -> Binop (substI' v l t', oper, substI' v r t')
| Num _ -> t
  (* TODO: field substitution *)
| Field (e, f) -> t

let rec substC' v t t' = match t with
| Var v' -> if String.equal v' @@ A.name v then t' else t
| Field (e, f) -> t
| Null -> t
| Cls -> t

let rec substI (s : t) v (t : int term) = match s with
| True -> True
| Cmp (l, oper, r) -> Cmp (substI' v l t, oper, substI' v r t)
| Alpha _ -> raise abspred
| Alias _ -> s
| NotEq _ -> s
| Access _ -> s
| Sep (s1, s2) -> Sep (substI s1 v t, substI s2 v t)

let rec substC (s : t) v (t : A.cls term) = match s with
| True -> True
| Cmp _ -> s
| Alpha _ -> raise abspred
| Alias ((e1, f1),(e2, f2)) ->
    Alias ((substC' v e1 t, f1),(substC' v e2 t, f2))
| NotEq (e1, e2) -> NotEq (substC' v e1 t, substC' v e2 t)
| Access _ -> s
| Sep (s1, s2) -> Sep (substC s1 v t, substC s2 v t)

let accesses phi (e : A.cls term) =
  let heap, footprint = dynFootprint [] phi in
  match e with
  | Var _ | Null | Cls -> true
  | Field (e, f) ->
      let acc = (e,f) in
      List.exists ~f:(accessEq acc) @@
      List.concat_map ~f:(aliases heap) footprint

let rec sat _ = raise @@ Failure "TODO"

