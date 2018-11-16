
module PB = Ast_pb
module AT = Ast_types
module A = Ast

let readPbuf fname =
  let bytes =
    let ic = open_in fname in
    let len = in_channel_length ic in
    let bytes = Bytes.create len in
    really_input ic bytes 0 len;
    close_in ic;
    bytes
  in PB.decode_program (Pbrt.Decoder.of_bytes bytes)

open Core
open Functools

let convertIdent (s : AT.identifier) = A.identifier s.AT.name

let convertType : AT.type_ -> A.typ = function
  | AT.Cls s -> A.Cls (convertIdent s)
  | AT.Int -> A.Int
  | AT.Top -> A.Top

let convertExpOp (op : AT.expop) : A.expop = op

let convertCmpOp (op : AT.cmpop) : A.cmpop = op

let convertVl = function
  | AT.Nil -> A.Nil
  | AT.Num i -> A.Num (Int32.to_int_exn i)
  | AT.Cls -> A.Cls

let rec convertExp =
  let open AT in function
  | Binop b ->
      A.Binop (convertExp b.left, convertExpOp b.oper, convertExp b.right)
  | Fieldaccess fa ->
      A.FieldAccess (convertExp fa.base, convertIdent fa.fieldname)
  | Val v -> A.Val (convertVl v)
  | Var x -> A.Var (convertIdent x)

let rec convertFormula =
  let open AT in function
  | Cmpf cf ->
      A.Cmpf (convertExp cf.left, convertCmpOp cf.oper, convertExp cf.right)
  | Alpha a -> A.Alpha (convertIdent a.clsname, convertExp a.arg)
  | Access a ->
      A.Access (List.map ~f:convertExp a.base, convertIdent a.fieldname)
  | Sep s -> A.Sep (convertFormula s.left, convertFormula s.right)

let convertPhi = function
  | AT.Concrete f -> A.Concrete (convertFormula f)
  | AT.Grad f -> A.Grad (convertFormula f)

let convertContract s = let open A in
  { requires = convertPhi s.AT.requires;
    ensures = convertPhi s.AT.ensures;
  }

let rec convertStmt =
  let open AT in function
  | Skip -> A.Skip
  | Seq s -> A.Seq (convertStmt s.prev, convertStmt s.next)
  | Assign a ->
      A.Assign (convertType a.t, convertIdent a.name, convertExp a.value)
  | Ifthen i ->
      A.IfThen { A.left = convertIdent i.left
               ; A.cmp = convertCmpOp i.oper
               ; A.right = convertIdent i.right
               ; A.thenClause = convertStmt i.thenclause
               ; A.elseClause = convertStmt i.elseclause
               }
  | Fieldasgn f ->
      A.Fieldasgn ( convertIdent f.base
                  , convertIdent f.fieldname
                  , convertIdent f.source
                  )
  | New_obj n -> A.NewObj (convertIdent n.name, convertIdent n.classname)
  | Call c -> A.Call { A.base = convertIdent c.base
                     ; A.target = convertIdent c.target
                     ; A.methodname = convertIdent c.methodname
                     ; A.args = List.map ~f:convertIdent c.args
                     }
  | Assert a -> A.Assert (convertFormula a)
  | Release a -> A.Release (convertFormula a)
  | Hold h -> A.Hold (convertFormula h.invariant, convertStmt h.body)

let convertAbsPred (apd : AT.abs_pred_defn) : A.absPred =
  let open AT in
  { A.name = convertIdent apd.name
  ; A.args = List.map ~f:convertIdent apd.args
  ; A.body = convertPhi apd.body
  }

let convertMethod (m : AT.method_) : A.methd =
  let open AT in
  { A.name = convertIdent m.name
  ; A.out_type = convertType m.out_type
  ; A.args = List.map ~f:(fun a->(convertType a.t, convertIdent a.name)) m.args
  ; A.dynamic = convertContract m.dynamic
  ; A.static = convertContract m.static
  ; A.body = convertStmt m.body
  }

let convertClass (c : AT.class_) : A.cls =
  let open AT in
  { A.name = convertIdent c.name
  ; A.super = convertIdent c.super
  ; A.fields = List.map ~f:(fun a -> (convertType a.t, convertIdent a.name)) c.fields
  ; A.abspreds = List.map ~f:convertAbsPred c.abspreds
  ; A.methods = List.map ~f:convertMethod c.methods
  }

let convertProgram p =
  let open AT in
  { A.classes = List.map ~f:convertClass p.classes
  ; A.stmts = List.map ~f:convertStmt p.stmts
  }

