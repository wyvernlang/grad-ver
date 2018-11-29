
open Core
open Functools

module A = Ast
module F = Formula

let rec liftTermI e = match e with
| A.Var s -> F.Var (A.name s)
| A.Val (A.Num n) -> F.Num n
| A.Val _ -> assert false
| A.Binop (e1, oper, e2) -> F.Binop (liftTermI e1, oper, liftTermI e2)
| A.FieldAccess (e,f) -> F.Field (liftTermC e, A.name f)

and liftTermC e = match e with
| A.Var s -> F.Var (A.name s)
| A.Val A.Nil -> F.Null
| A.Val A.Cls -> F.Cls
| A.Val _ -> assert false
| A.Binop _ -> assert false
| A.FieldAccess (e,f) -> F.Field (liftTermC e, A.name f)

let rec wlp s phi = match s with
| A.Skip -> phi
| A.Seq (s1, s2) -> wlp s1 (wlp s2 phi)
| A.Assign (t, v, e) ->
    begin
      match t with
      | A.Int -> F.substI phi v @@ liftTermI e
      | A.Cls c ->
          let e' = liftTermC e in
          if F.accesses phi e' then
            F.substC phi v e'
          else
            F.Sep (F.acc e', F.substC phi v e')
      | A.Top -> assert false
    end
| A.Fieldasgn (x,f,y) ->
    let vx = F.Var (A.name x) in
    let vy = F.Var (A.name y) in
    begin
      raise @@ Failure "what type is x.f?"
    end
| A.NewObj (x, c) ->
    raise @@ Failure "TODO"

