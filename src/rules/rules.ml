
open Core
open Functools

module A = Ast
module F = Formula

let rec liftTerm e = match e with
| A.Var s -> F.Var (A.name s)
| A.Val (A.Num n) -> F.Num n
| A.Val A.Nil -> F.Null
| A.Val A.Cls -> F.Cls
| A.Binop (e1, oper, e2) -> F.Binop (liftTerm e1, oper, liftTerm e2)
| A.FieldAccess (e,f) -> F.Field (liftTerm e, A.name f)

let rec wlp s phi = match s with
| A.Skip -> phi
| A.Seq (s1, s2) -> wlp s1 (wlp s2 phi)
| A.Assign (t, v, e) ->
    begin
      match t with
      | A.Int -> F.substVar phi v @@ liftTerm e
      | A.Cls c ->
          let e' = liftTerm e in
          if F.accesses phi e' then
            F.substVar phi v e'
          else
            F.Sep (F.acc e', F.substVar phi v e')
      | A.Top -> assert false
    end
| A.Fieldasgn (x,f,y) ->
    let vx = F.Var (A.name x) in
    let vy = F.Var (A.name y) in
    begin
      if F.accesses phi (F.Field (vx, A.name f)) then
        F.substAcc phi (vx, A.name f) vy
      else
        F.Sep (F.acc @@ F.Field (vx, A.name f),
               F.substAcc phi (vx, A.name f) vy)
    end
| A.NewObj (x, c) ->
    raise @@ Failure "TODO"

