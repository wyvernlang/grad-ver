
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

type vl = Null [@key 1]
        | Num of int [@key 2]
        | Cls [@key 3] (* TODO: do we even need any internals? *)
        [@@deriving protobuf { protoc }]

type exp = Var of ident [@key 1]
         | Val of vl [@key 2]			(** Need to fix val? *)
         | Binop of exp * expop * exp [@key 3]
         | FieldAcc of exp * ident [@key 4]
         [@@deriving protobuf { protoc }]

type formula = True_ [@key 1]
             | Cmp of exp * cmpop * exp [@key 2]
               (* For some reason, ppx_deriving_protobuf can't serialize
                * exp list unless we put it in a tuple with something.
                *)
             | Alpha of ident * exp list [@key 3]
             | Access of exp * ident [@key 4]
             | Sep of formula * formula [@key 5]
             [@@deriving protobuf { protoc }]

type phi = Concrete of formula [@key 1]
           (* Gradual e is ? * e *)
         | Gradual of formula [@key 2]
         [@@deriving protobuf { protoc }]

type contract = { requires : phi [@key 1]
                ; ensures : phi [@key 2]
                } [@@deriving protobuf { protoc }]

type stmt = Skip [@key 1]
          | Seq of stmt * stmt [@key 2]		(** stmt list instead? *)
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
          | Hold of formula * stmt [@key 11]
          [@@deriving protobuf { protoc }]

type apfdef = ident * ident list * phi [@@deriving protobuf { protoc }]

type methd = { name : ident [@key 1]
             ; outtype : typ [@key 2]
             ; args : (typ * ident) list [@key 3]
             ; dynamic : contract [@key 4]
             ; static : contract [@key 5]
             ; body : stmt [@key 6]
             } [@@deriving protobuf { protoc }]

type cls = { name : ident [@key 1]
					 ; super : ident [@key 2]
           ; fields : (typ * ident) list [@key 3]
					 ; apfdefs : apfdef list [@key 4]  
           ; methods : methd list [@key 5]
           } [@@deriving protobuf { protoc }]

type program = cls list * stmt [@@deriving protobuf { protoc }]

let f = program_from_protobuf

