
type ident = string [@@deriving protobuf { protoc }]

type typ = Int [@key 1]
         | Top [@key 2]
         | Class of ident [@key 3]
         [@@deriving protobuf { protoc }]

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

type dummy = DUMMY [@key 1] [@@deriving protobuf]

type formula = True_ [@key 1]
             | Cmp of exp * cmpop * exp [@key 2]
               (* For some reason, ppx_deriving_protobuf can't serialize
                * exp list unless we put it in a tuple with something.
                *)
             | Alpha of dummy * exp list [@key 3]
             | Access of exp * ident [@key 4]
             | Sep of formula * formula [@key 5]
             [@@deriving protobuf { protoc }]

type phi = Concrete of formula [@key 1]
           (* Gradual e is ? * e *)
         | Gradual of formula [@key 2]
         [@@deriving protobuf { protoc }]

type contract = { requires : contract [@key 1]
                ; ensures : contract [@key 2]
                } [@@deriving protobuf { protoc }]

type stmt = Skip [@key 1]
          | Seq of stmt * stmt [@key 2]
          | Decl of typ * ident [@key 3]
          | Assign of ident * exp [@key 4]
          | If of ident * cmpop * ident * stmt * stmt [@key 5]
            (* x.f = y *)
          | FieldAssign of ident * ident * ident [@key 6]
          | New of ident * ident [@key 7]
            (* y := z.m(xs) *)
          | MethodCall of ident * ident * ident list [@key 8]
          | Assert of formula [@key 9]
          | Release of formula [@key 10]
          | Hold of formula * stmt list [@key 11]
          [@@deriving protobuf { protoc }]

type methd = { name : ident [@key 1]
             ; outty : typ [@key 2]
             ; inp : (ident * typ) list [@key 3]
             ; dynamic : contract [@key 4]
             ; static : contract [@key 5]
             ; body : stmt [@key 6]
             } [@@deriving protobuf { protoc }]

type cls = { name : ident [@key 1]
           ; fields : (typ * ident) list [@key 2]
           ; methods : methd list [@key 3]
           } [@@deriving protobuf { protoc }]

type program = cls list * stmt [@@deriving protobuf { protoc }]

let f = program_from_protobuf

