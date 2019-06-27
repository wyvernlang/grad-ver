open Core

open Ast

open Utility
open Functools

(* for all the classes *)
module ClassContext =
struct
  (* [id] is name of class, [class_] is class itself *)
  type t = (id, class_) Hashtbl.t

  let to_string : t -> string = Sexp.to_string @< Hashtbl.sexp_of_t sexp_of_id sexp_of_class_

  let constructClass ctx cls : unit =
    Hashtbl.set ctx ~key:cls.id ~data:cls

  (* for each class, register the class which records its methods/predicates and argument types *)
  let construct (prgm:program) : t =
    let ctx = failwith "TODO" in
    List.iter prgm.classes ~f:(constructClass ctx);
    ctx

end

(* for each statement scope *)
module TypeContext =
struct
  type t = (id, type_) Hashtbl.t

  let to_string : t -> string = Sexp.to_string @< Hashtbl.sexp_of_t sexp_of_id sexp_of_type_

  (* for each class, iterate through the class's methods/predicates, constructing/checking their type contexts *)
  (* TODO *)
  let construct : ClassContext.t -> statement -> t = failwith "TODO"
end


let synthesizeType : TypeContext.t -> expression -> type_ = failwith "TODO"

let checkProgram : program -> unit = failwith "TODO"
