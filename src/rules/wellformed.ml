
open Core
open Functools

module A = Ast

exception WellFormed

let clsctx = String.Table.create ()

(* XXX - This table is a hack that stores the types of every variable declared
 *       in the main statement sequence, disregarding individual methods. To
 *       fix this, we should either annotate the AST itself or have one such
 *       table/mapping for each statement sequence. We need this because WLP
 *       rules traverse backwards, but some of the conversion rules require
 *       type information (which must be propagated forwards).
 *)
let varctx = String.Table.create ()

let pp_type = function
  | A.Int -> "Int"
  | A.Any -> "NULL"
  | A.Cls c -> "Class(" ^ A.name c ^ ")"
  | A.Top -> "T"

let tyEq t t' = match t, t' with
| A.Int, A.Int -> true
| A.Cls c, A.Cls c' -> A.matchIdent c c'
| A.Cls _, A.Any -> true
| A.Any, A.Cls _ -> true
| A.Top, A.Top -> true
| _ -> false

let lookup ctx s f =
  match Hashtbl.find ctx s with
  | None -> raise WellFormed
  | Some x -> f x

let rec synthtype e = match e with
| A.Binop (e1, _, e2) ->
    begin
      match synthtype e1, synthtype e2 with
      | A.Int, A.Int -> A.Int
      | _ -> raise WellFormed
    end
| A.FieldAccess (e, f) ->
    begin
      match synthtype e with
      | A.Int | A.Top | A.Any -> raise WellFormed
      | A.Cls c ->
          lookup clsctx (A.name c) (fun c' ->
            match List.find ~f:(A.matchIdent f @< snd) c'.A.fields with
            | None -> raise WellFormed
            | Some (t,_) -> t
          )
    end
| A.Val (A.Num n) -> A.Int
| A.Val A.C -> A.Top
| A.Val A.Nil -> A.Any
| A.Var v -> lookup varctx (A.name v) (fun x -> x)

let rec checkFormula phi = match phi with
| A.Cmpf (e1, A.Neq, e2) | A.Cmpf (e1, A.Eq, e2) ->
    let t1 = synthtype e1 in
    let t2 = synthtype e2 in
    if tyEq t1 t2 then () else raise WellFormed
| A.Cmpf (e1, _, e2) ->
    begin
      match synthtype e1, synthtype e2 with
      | A.Int, A.Int -> ()
      | _ -> raise WellFormed
    end
| A.Sep (s1, s2) -> checkFormula s1; checkFormula s2
| A.Access (e, f) ->
    begin
      match synthtype e with
      | A.Int | A.Top | A.Any -> raise WellFormed
      | A.Cls c ->
          lookup clsctx (A.name c) (fun c' ->
            if List.exists ~f:(A.matchIdent f @< snd) c'.A.fields
              then ()
              else raise WellFormed
          )
    end
| A.Alpha _ -> raise @@ Failure "abstract predicates unimplemented"

let rec checkPhi phi = match phi with
| A.Grad _ -> raise @@ Failure "gradual formulas unimplemented"
| A.Concrete p -> checkFormula p

let processStms =
  let rec go () s = match s with
  | A.Skip -> ()
  | A.Seq (s1, s2) -> go () s1; go () s2
  | A.Assign (t, s, e) ->
      let t' = synthtype e in
      if not (tyEq t t')
        then raise WellFormed
        else Hashtbl.set varctx (A.name s) t
  | A.Fieldasgn (x, f, y) ->
      begin
        match Hashtbl.find varctx (A.name x) with
        | None | Some A.Top | Some A.Int | Some A.Any -> raise WellFormed
        | Some (A.Cls c) ->
            lookup clsctx (A.name c) (fun c' ->
              match List.find ~f:(A.matchIdent f @< snd) c'.A.fields with
              | None -> raise WellFormed
              | Some (t,_) ->
                  lookup varctx (A.name y)
                  (fun t' -> if tyEq t t' then () else raise WellFormed)
            )
      end
  | A.NewObj (x, c) ->
      lookup varctx (A.name x)
      (function | A.Int | A.Top | A.Any -> raise WellFormed
                | A.Cls c' -> if A.matchIdent c c' then ()
                                                   else raise WellFormed)
  | A.Assert phi -> checkFormula phi
  | A.IfThen _ -> raise @@ Failure "TODO: statement wellformed"
  | A.Call _ -> raise @@ Failure "TODO: statement wellformed"
  | A.Release _ -> raise @@ Failure "TODO: statement wellformed"
  | A.Hold _ -> raise @@ Failure "TODO: statement wellformed"
  in
  List.fold_left ~f:go ~init:()

let processProgram prg =
  let _ =
    List.map ~f:(fun c -> Hashtbl.set clsctx (A.name c.A.name) c) prg.A.classes
    (* TODO: Check methods *)
  in
  processStms prg.A.stmts

