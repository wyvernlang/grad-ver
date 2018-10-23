
type ident = string [@@deriving protobuf { protoc }]

type typ = Int [@key 1]
         | Top [@key 2]
         | Class of ident [@key 3]
         [@@deriving protobuf { protoc }]

type cls = { name : ident [@key 1]
           ; fields : (typ * ident) list [@key 2]
           } [@@deriving protobuf]

type expop = Plus [@key 1]
           | Minus [@key 2]
           | Times [@key 3]
           | Div [@key 4]
           [@@deriving protobuf { protoc }]

type cmpop = NEQ [@key 1]
           | EQ [@key 2]
           | LT [@key 3]
           | GT [@key 4]
           [@@deriving protobuf { protoc }]

type exp = Var of ident [@key 1]
         | Val of int [@key 2]
         | Binop of exp * expop * exp [@key 3]
         | Field of exp * ident [@key 4]
         [@@deriving protobuf { protoc }]

type stmt = Skip [@key 1]
          | Seq of stmt * stmt [@key 2]
          | Decl of typ * ident [@key 3]
          | Assign of ident * exp [@key 4]
          [@@deriving protobuf { protoc }]

type program = cls list * stmt [@@deriving protobuf { protoc }]

let f = program_from_protobuf

