
module PB = Ast_pb

type ident = string

let matchIdent = String.equal
let identifier s = s
let name s = s
module IdentMap = Core.String.Map

type typ =
  | Cls of ident
  | Int
  | Top
  | Any

type expop = Ast_types.expop =
  | Plus
  | Minus
  | Times
  | Div

type cmpop = Ast_types.cmpop =
  | Neq
  | Eq
  | Lt
  | Gt
  | Le
  | Ge

type vl =
  | Nil
  | Num of int
  | C
  | Result

and expr =
  | Binop of expr * expop * expr
  | FieldAccess of expr * ident
  | Val of vl
  | Var of ident
  | Old of ident

and formula =
  | Cmpf of expr * cmpop * expr
  | Alpha of ident * expr list
  | Access of expr * ident
  | Sep of formula * formula

type phi =
  | Concrete of formula
  | Grad of formula

type contract = {
  requires : phi;
  ensures : phi;
}

type methodCall = {
  target : ident;
  base : ident;
  methodname : ident;
  args : ident list;
}

type ifthen = {
  left : ident;
  cmp : cmpop;
  right : ident;
  thenClause : stmt;
  elseClause : stmt;
}

and stmt =
  | Skip
  | Seq of stmt * stmt
  | Declare of typ * ident
  | Assign of ident * expr
  | IfThen of ifthen
  | Fieldasgn of ident * ident * ident
  | NewObj of ident * ident
  | Call of methodCall
  | Assert of formula
  | Release of formula
  | Hold of formula * stmt

type absPred = {
  name : ident;
  args : ident list;
  body : phi;
}

type methd = {
  name : ident;
  out_type : typ;
  args : (typ * ident) list;
  dynamic : contract;
  static : contract;
  body : stmt;
}

type cls = {
  name : ident;
  super : ident;
  fields : (typ * ident) list;
  abspreds : absPred list;
  methods : methd list;
}

type program = {
  classes : cls list;
  stmts : stmt list;
}

let pp_type = function
  | Int -> "Int"
  | Any -> "NULL"
  | Cls c -> "Class(" ^ c ^ ")"
  | Top -> "T"

let rec pp_binop = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"

let rec pp_cmpop = function
  | Eq -> "=="
  | Neq -> "<>"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="

let rec pp_exp = function
  | Binop (e1, op, e2) -> pp_exp e1 ^ pp_binop op ^ pp_exp e2
  | FieldAccess (e, f) -> pp_exp e ^ "." ^ f
  | Var v -> v
  | Val Nil -> "NULL"
  | Val C -> "C"
  | Val (Num n) -> Core.Int.to_string n

let rec pp_formula = function
  | Cmpf (e1, op, e2) -> pp_exp e1 ^ pp_cmpop op ^ pp_exp e2
  | Access (e, f) -> "acc(" ^ pp_exp e ^ "." ^ f ^ ")"
  | Sep (s1, s2) -> pp_formula s1 ^ " * " ^ pp_formula s2
  | Alpha _ -> raise @@ Failure "abstract predicates TODO"

let rec pp_stmt = function
  | Skip -> ""
  | Seq (s1, s2) -> pp_stmt s1 ^ ";\n" ^ pp_stmt s2
  | Declare (t, v) -> pp_type t ^ " " ^ v
  | Assign (v, e) -> v ^ " = " ^ pp_exp e
  | Fieldasgn (x, f, y) -> x ^ "." ^ f ^ " = " ^ y
  | NewObj (x, c) -> x ^ " = " ^ c
  | Assert a -> "assert " ^ pp_formula a
  | Release _ | Hold _ | IfThen _ -> raise @@ Failure "TODO"
  | Call m -> m.base ^ "." ^ m.methodname ^ "(" ^ "..." ^ ")"
