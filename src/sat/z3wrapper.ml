
open Core

open Z3
module A = Arithmetic
module B = Boolean

module F = Formula
module H = F.Heap

type context = Z3.context

let mk_div' ctx = function
  | [e1; e2] -> A.mk_div ctx e1 e2
  | _ -> assert false

let mk_neq' ctx e1 e2 =
  B.mk_not ctx (B.mk_eq ctx e1 e2)

let liftOp = function
  | F.Plus -> A.mk_add
  | F.Minus -> A.mk_sub
  | F.Times -> A.mk_mul
  | F.Div -> mk_div'

let liftCmp = function
  | F.Eq -> B.mk_eq
  | F.Neq -> mk_neq'
  | F.Gt -> A.mk_gt
  | F.Ge -> A.mk_ge
  | F.Lt -> A.mk_lt
  | F.Le -> A.mk_le

let rec v2s e = match e with
| F.Var v -> v
| F.Field (e, v) -> v2s e ^ "." ^ v
| _ -> assert false

let constructVar ctx e =
  raise @@ Failure "TODO"

let rec liftExpr heap vars ctx e = match e with
| F.Var v ->
    begin
      match Map.find vars v with
      | Some v -> v
      | None -> raise F.Unsat
    end, vars
| F.Num n -> A.Integer.mk_numeral_i ctx n, vars
| F.Null -> A.Integer.mk_numeral_i ctx (-1), vars
| F.Cls -> raise @@ Failure "unimplemented"
| F.Field (e', f) ->
    begin
      try
        let c = H.lookup heap e in
        A.Integer.mk_numeral_i ctx (H.cellId c), vars
      with H.Unknown _ ->
        let s = v2s e in
        match Map.find vars s with
        | Some v -> v, vars
        | None ->
            let v = A.Integer.mk_numeral_s ctx s in
            v, Map.set vars s v
    end
| F.Binop (e1, op, e2) ->
    let e1', vars' = liftExpr heap vars ctx e1 in
    let e2', vars'' = liftExpr heap vars' ctx e2 in
    liftOp op ctx [e1'; e2'], vars''

let rec liftFormula heap vars ctx s = match s with
| F.True -> vars, B.mk_true ctx
| F.Alias (e1, e2) ->
    let s1 = v2s e1 in
    let s2 = v2s e2 in
    let v1 = match Map.find vars s1 with
             | Some v -> v
             | None ->
                 A.Integer.mk_numeral_i ctx (H.cellId @@ H.lookup heap e1)
    in
    let v2 = match Map.find vars s2 with
             | Some v -> v
             | None ->
                 A.Integer.mk_numeral_i ctx (H.cellId @@ H.lookup heap e2)
    in
    Map.set (Map.set vars s1 v1) s2 v2, B.mk_eq ctx v1 v2
| F.NotEq (e1, e2) ->
    let s1 = v2s e1 in
    let s2 = v2s e2 in
    let v1 = match Map.find vars s1 with
             | Some v -> v
             | None ->
                 A.Integer.mk_numeral_i ctx (H.cellId @@ H.lookup heap e1)
    in
    let v2 = match Map.find vars s2 with
             | Some v -> v
             | None ->
                 A.Integer.mk_numeral_i ctx (H.cellId @@ H.lookup heap e2)
    in
    Map.set (Map.set vars s1 v1) s2 v2, B.mk_not ctx (B.mk_eq ctx v1 v2)
| F.Cmp (e1, op, e2) ->
    let e1', vars' = liftExpr heap vars ctx e1 in
    let e2', vars'' = liftExpr heap vars' ctx e2 in
    vars'', liftCmp op ctx e1' e2'
| F.Alpha _ -> raise F.abspred
| F.Access (e, f) ->
    begin
      try
        let c = H.lookup heap (F.Field (e, f)) in
        vars, A.Integer.mk_numeral_i ctx (H.cellId c)
      with H.Unknown _ ->
        let s = v2s (F.Field (e, f)) in
        match Map.find vars s with
        | Some v -> vars, v
        | None ->
            let v = A.Integer.mk_numeral_s ctx s in
            Map.set vars s v, v
    end
| F.Sep (s1, s2) ->
    let vars', s1' = liftFormula heap vars ctx s1 in
    let vars'', s2' = liftFormula heap vars' ctx s2 in
    vars'', B.mk_and ctx [s1';s2']

let sat phi =
  let heap = F.collectAliases H.empty phi in
  let (acc, nat) = F.splitAccs phi in
  let ctx = mk_context ["well_sorted_check", "true"] in
  let _ ,f = liftFormula heap String.Map.empty ctx nat in
  let s = Solver.mk_simple_solver ctx in
  match Solver.check s [f] with
  | Solver.SATISFIABLE -> true
  | _ -> raise F.Unsat

(* TODO: make sure that all accessibility predicates in phi are in pre *)
let valid pre phi =
  try
    let heap = F.collectAliases H.empty phi in
    let (_, nat) = F.splitAccs phi in
    let (_, nat') = F.splitAccs pre in
    let ctx = mk_context ["well_sorted_check", "true"] in
    let _ , pref = liftFormula heap String.Map.empty ctx nat in
    let _, phif = liftFormula heap String.Map.empty ctx nat' in
    let f = B.mk_implies ctx pref phif in
    let s = Solver.mk_simple_solver ctx in
    match Solver.check s [f] with
    | Solver.UNSATISFIABLE -> true
    | _ -> false
  with F.Unsat -> false

