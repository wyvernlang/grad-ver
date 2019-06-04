open Core
open Functools
open Ast_types
open Utility

(****************************************************************************************************************************)
(* utilities *)

(* exceptions *)
(* TODO: move these to their respective relevant places in the code? *)

exception Malformed             of string
exception Class_not_defined     of string * id
exception Class_mismatch        of string * class_ * class_
exception Field_not_declared    of string * class_ * id
exception Predicate_not_defined of string * class_ * id
exception Method_not_defined    of string * class_ * id
exception Variable_not_declared of string * id
exception Type_mismatch         of string * type_ * type_

(* useful functions *)
(* move these to a utilities or separate Ast module? *)

let eqId    : id     -> id     -> bool = fun id  id'  -> id.string = id'.string
let eqType  : type_  -> type_  -> bool = fun typ typ' -> typ = typ'
let eqClass : class_ -> class_ -> bool = fun cls cls' -> eqId cls.id cls'.id

(****************************************************************************************************************************)
(* type *)

let checkTypeMatch : type_ -> type_ -> unit =
  fun typ typ' ->
  if eqType typ typ'
  then ()
  else raise @@ Type_mismatch ("checking type match", typ, typ')

(****************************************************************************************************************************)
(* context *)

module Context = struct
  type 'a t = (string, 'a) Core.String.Table.t_

  let create : unit -> 'a t = String.Table.create

  let find : 'a t -> id -> 'a option  = fun ctx id   -> Hashtbl.find ctx id.string
  let set  : 'a t -> id -> 'a -> unit = fun ctx id v -> Hashtbl.set  ctx id.string v

  let findExcept : 'a t -> id -> exn -> 'a =
    fun ctx id e ->
    match find ctx id with
    | Some a -> a
    | None   -> raise e
end

(*****************************************************************)
(* context of class definitions *)

let class_context : class_ Context.t = Context.create ();;

let setClass : id -> class_ -> unit = Context.set class_context
let getClass : id -> class_ = fun id -> Context.findExcept class_context id @@
  Class_not_defined ("referencing class", id)

let getField : id -> id -> class_field =
  fun clsid fldid ->
  let cls = getClass clsid in
  match List.find cls.fields ~f:(fun fld -> eqId fld.id fldid) with
  | Some fld -> fld
  | None     -> raise (Field_not_declared ("referencing field", cls, fldid))

let getFieldType : id -> id -> type_ =
  fun clsid fldid -> (getField clsid fldid).type_

let getPredicate : id -> id -> predicate =
  fun clsid predid ->
  let cls = getClass clsid in
  match List.find cls.predicates ~f:(fun pred -> eqId pred.id predid) with
  | Some pred -> pred
  | None      -> raise @@ Predicate_not_defined ("referencing predicate", cls, predid)

let getPredicateArguments : id -> id -> argument list =
  fun clsid predid -> (getPredicate clsid predid).arguments
let getPredicateFormula : id -> id -> formula =
  fun clsid predid -> (getPredicate clsid predid).formula

let getMethod : id -> id -> method_ =
  fun clsid methid ->
  let cls = getClass clsid in
  match List.find cls.methods ~f:(fun meth -> eqId meth.id methid) with
  | Some meth -> meth
  | None      -> raise @@ Method_not_defined ("referencing method", cls, methid)

(*****************************************************************)
(* context of variable declarations *)

let variable_context : type_ Context.t = Context.create ();;

let setVariableType : id -> type_ -> unit = Context.set variable_context
let getVariableType : id -> type_ = fun id -> Context.findExcept variable_context id @@
  Variable_not_declared ("referencing variable", id)

(****************************************************************************************************************************)
(* types *)

(* type synthesis *)
let rec synthesizeType : expression -> type_ =
  fun e ->
  match e with
  | Variable x ->
    begin
      match x with
      | Result -> unimplemented ()
      | Id id -> unimplemented ()
      | Old id -> unimplemented ()
      | This -> unimplemented ()
    end
  | Value v ->
    begin
      match v with
      | Int x -> unimplemented ()
      | Objectid id -> unimplemented ()
      | Null -> unimplemented ()
      | True -> unimplemented ()
      | False -> unimplemented ()
    end
  | Binaryoperation biop ->
    begin
      let ltyp  = synthesizeType biop.binaryoperationleft in
      let rtyp = synthesizeType biop.binaryoperationright in
      match biop.binaryoperator with
      | Add | Sub | Mul | Div ->
        if (eqType ltyp rtyp) && (eqType ltyp Int)
        then Int
        else raise @@ Malformed "type mismatch in binary operation"
    end
  | Binarycomparison bico ->
    begin
      let ltyp  = synthesizeType bico.binarycomparisonleft in
      let rtyp = synthesizeType bico.binarycomparisonright in
      if eqType ltyp rtyp
      then Top
      else raise @@ Malformed "type mismatch in binary comparison"
    end
  | Fieldreference fldref ->
    begin
      let baseType = synthesizeType fldref.base in
      match baseType with
      | Class typcls ->
        let clsfield = getField typcls.classid fldref.fieldid in
        clsfield.type_
      | _ -> raise @@ Malformed "attempted to reference field of non-object"
    end

(****************************************************************************************************************************)
(* check class *)

let rec checkClass : class_ -> unit =
  fun cls ->
  unimplemented ()

(****************************************************************************************************************************)
(* check field *)

let rec checkField : class_field -> unit =
  fun fld ->
  unimplemented ()

(****************************************************************************************************************************)
(* check predicate *)

let rec checkPredicate : predicate -> unit =
  fun pred ->
  unimplemented ()

(****************************************************************************************************************************)
(* check method *)

let rec checkMethodBody : method_ -> unit =
  fun meth ->
  unimplemented ()

(****************************************************************************************************************************)
(* check expression *)

let rec checkExpression : expression -> unit =
  fun expr ->
  unimplemented ()

(****************************************************************************************************************************)
(* check formula *)

let rec checkConreteFormula : formula_concrete -> unit =
  fun phi ->
  match phi with
  | Expression expr -> unimplemented ()
  | Predicatecheck predchk -> unimplemented ()
  | Accesscheck accchk ->
    begin
      match synthesizeType accchk.base with
      | Class cls -> ignore @@ getField accchk.fieldid cls.classid
      | _ -> raise @@ Malformed "attempted to access field of non-object"
    end
  | Logicaland logand -> unimplemented ()
  | Logicalseparate logsep -> checkConreteFormula logsep.separateleft; checkConreteFormula logsep.separateright
  | Ifthenelse ite -> unimplemented ()
  | Unfoldingin unin -> unimplemented ()

let checkImpreciseFormula : formula_imprecise -> unit =
  fun phi ->
  unimplemented ()

let rec checkFormula : formula -> unit =
  fun phi ->
  match phi with
  | Imprecise phi_impr -> checkImpreciseFormula phi_impr
  | Concrete  phi_conc -> checkConreteFormula   phi_conc

let checkContract : contract -> unit =
  fun ctrt ->
  unimplemented ()

(****************************************************************************************************************************)
(* statements *)

let rec checkStatement : statement -> unit =
  fun stmt ->
  match stmt with
  | Skip ->
    ()
  | Sequence seq ->
    checkStatement seq.prev;
    checkStatement seq.next
  | Declaration decl ->
    setVariableType decl.id decl.type_
  | Assignment asmt ->
    let typ  = synthesizeType asmt.value in
    let typ' = getVariableType asmt.id in
    if eqType typ typ'
    then ()
    else raise @@ Malformed "type mismatch in assignment"
  | Ifthenelse ite ->
    checkExpression ite.condition;
    checkStatement  ite.thenbody;
    checkStatement  ite.elsebody
  | Whileloop wl ->
    checkFormula    wl.invariant;
    checkExpression wl.condition;
    checkStatement  wl.body
  | Fieldassignment fasmt ->
    let basetyp = getVariableType fasmt.baseid in
    begin
      match basetyp with
      | Class cls ->
        let fldtyp = getFieldType fasmt.baseid fasmt.fieldid in
        let srctyp = getVariableType fasmt.sourceid in
        if eqType fldtyp srctyp
        then ()
        else raise @@ Malformed "type mismatch in field assignment"
      | _ -> raise @@ Malformed "attempted to reference field of non-object"
    end
  | Newobject newobj ->
    let vartyp = getVariableType newobj.id in
    let cls    = getClass newobj.classid in
    begin
      match vartyp with
      | Class typcls' ->
        let cls' = getClass typcls'.classid in
        if eqClass cls cls'
        then ()
        else raise @@ Class_mismatch ("attempted to assign variable of class", cls, cls')
      | _ -> raise @@ Malformed "type mismatch in creating new object instance"
    end
  | Methodcall methcall ->
    let meth   = getMethod methcall.baseid methcall.methodid in
    let vartyp = getVariableType methcall.targetid in
    checkTypeMatch  meth.type_ vartyp;
    checkFormula    meth.dynamic.requires;
    checkFormula    meth.dynamic.ensures;
    checkFormula    meth.static.requires;
    checkFormula    meth.static.ensures;
    checkMethodBody meth;
  | Methodcalldynamic methcalldyn ->
    unimplemented ()
  | Assertion asrt ->
    checkFormula asrt.formula
  | Release rls ->
    unimplemented ()
  | Hold hld ->
    checkFormula   hld.formula;
    checkStatement hld.body
  | Fold fld ->
    unimplemented ()
  | Unfold unfld ->
    unimplemented ()

(****************************************************************************************************************************)
(* program *)

let checkProgram : program -> unit =
  fun prgm ->
  List.fold_left ~init:() ~f:(fun () -> checkClass) prgm.classes;
  checkStatement prgm.statement
