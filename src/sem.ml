
open Ast2
open Printf

exception ErrNotImplemented

exception ErrCompiler of string

exception ErrCallNotEnoughArgs of Ast1.id
exception ErrCallTooManyArgs of Ast1.id
exception ErrExpectedArray of Ast1.lhs
exception ErrExpectedFunction of Lexing.position * defU
exception ErrExpectedRecord of Lexing.position * defU
exception ErrFieldNotFound of Lexing.position * id * def
exception ErrGlobalVar of Lexing.position * id
exception ErrLiteralArraySameType of Lexing.position
exception ErrReassignedVal of Lexing.position * id
exception ErrRecordOnlyVariables of Ast1.def
exception ErrType of Lexing.position * typ * typ
exception ErrUndefinedFunction of Ast1.id
exception ErrUndefinedRecord of Lexing.position * id
exception ErrUndefinedVariable of Ast1.id
exception ErrUnknownType of Ast1.typ
exception ErrUntypedVarDec of Ast1.def

(* dummy LLVM value *)
let llv = Llvm.undef @@ Llvm.i1_type @@ Llvm.global_context ()

(* -------------------------------------------------------------------------- *)

module ST = struct
  exception ErrDuplicate of Ast2.def * Ast2.def

  class scope = object
    val mutable defs = ([] : Ast2.def list)

    method lookup (id: Ast2.id) =
      let f (def: Ast2.def) = (def.id = id) in
      List.find_opt f defs

    method insert def = defs <- def :: defs; def
  end

  class table = object (self)
    val mutable scopes = ([] : scope list)

    method enterblock = scopes <- new scope :: scopes

    method leaveblock =
      try scopes <- List.tl scopes
      with Failure _ -> raise (ErrCompiler "st.leaveblock")

    (* returns None if could not find a <def> with <id> *)
    (* otherwise returns <def>                          *)
    method lookup id = List.find_map (fun scope -> scope#lookup id) scopes

    (* returns <def> or raises an exception *)
    method insert (def: Ast2.def) =
      match scopes with
      | [] -> raise (ErrCompiler "st.insert.noscope")
      | scope :: _ ->
        begin match scope#lookup def.id with
        | None             -> scope#insert def
        | Some originaldef -> raise @@ ErrDuplicate (def, originaldef)
        end
  end
end

(* -------------------------------------------------------------------------- *)

(* semantic state *)
class ss = object (self)
  inherit ST.table as super

  (* the return type of a function, if inside one *)
  val mutable ret : typ option = None
  method get_ret = ret
  method set_ret v = ret <- v
end

(* -------------------------------------------------------------------------- *)

let can_be_reassigned ({p; u; _}: lhs) =
  let ok = (true, ErrCompiler "can_be_reassigned") in
  match u with
  | Id {id; u; _} ->
    begin match u with
    | Val _ -> (false, ErrReassignedVal (p, id))
    | _ -> ok
    end
  | _ -> raise ErrNotImplemented

let rec typecheck p t1 t2 =
  let err = ErrType (p, t1, t2) in
  match (t1, t2) with
  | Void, Void | Bool, Bool | Int, Int | Float, Float | String, String -> ()
  | Array t1, Array t2 -> begin try typecheck p t1 t2 with _ -> raise err end
  | Record id1, Record id2 when id1 = id2 -> ()
  | _ -> raise err

let find_record ss p id = match ss#lookup id with
  | None -> raise @@ ErrUndefinedRecord (p, id)
  | Some (def: def) ->
    begin match def.u with
    | Rec fields -> (def, fields)
    | _ -> raise @@ ErrExpectedRecord (p, def.u)
    end

let lookup_record (fields: def list) id (p, rec_) =
  match List.find_opt (fun field -> field.id = id) fields with
  | None -> raise @@ ErrFieldNotFound (p, id, rec_)
  | Some (field: def) -> field

(* TODO move to Ast2 module *)
let rec typ_tostring = function
  | Void -> "Void"
  | Bool -> "Bool"
  | Int -> "Int"
  | Float -> "Float"
  | String -> "String"
  | Array typ -> "[" ^ typ_tostring typ  ^ "]"
  | Record def -> def.id

(* -------------------------------------------------------------------------- *)

let rec analyse ast1 =
  let ss = new ss in
  ss#enterblock;
  let ast2 =
    try Ok (List.map (fun def -> sem_top ss def) ast1)
    with e -> Error (handle_error e)
  in
  ss#leaveblock;
  ast2

(* -------------------------------------------------------------------------- *)

and sem_top ss def = match def with
  | Ast1.Val _ -> sem_var ss def

  | Ast1.Var (_, id, _, _) -> raise @@ ErrGlobalVar (fst id, snd id)

  | Ast1.Fun (p, (_, id), params, typ, block) ->
    let typ = match typ with
      | None -> Void
      | Some typ -> sem_typ ss typ
    in
    let (dummyU: defU) = Fun ([], typ, []) in
    let fun_ = {p = p; id = id; u = dummyU; llv = llv} in
    let _ = ss#insert fun_ in
    ss#enterblock;
    let f = function
      | Ast1.Val (p, id, Some _, Ast1.Dynamic _) as param -> sem_var ss param
      | _ -> raise (ErrCompiler "top.Fun")
    in
    let params = List.map f params in
    fun_.u <- Fun (params, typ, []);
    ss#set_ret (Some typ);
    let block = sem_block ss block in
    ss#set_ret None;
    fun_.u <- Fun (params, typ, block);
    ss#leaveblock;
    fun_

  | Ast1.Rec (p, (_, id), defs) ->
    let dummyU = Rec [] in
    let rec_ = {p = p; id = id; u = dummyU; llv = llv} in
    let f (def: Ast1.def) = match def with
      | Val _ -> sem_var ss def
      | Var _ -> sem_var ss def
      | _     -> raise (ErrRecordOnlyVariables def)
    in
    let _ = ss#insert rec_ in
    ss#enterblock;
    rec_.u <- Rec (List.map f defs);
    ss#leaveblock;
    rec_

(* -------------------------------------------------------------------------- *)

and sem_var ss def = match def with
  | Ast1.Val (p, id, typ, exp) ->
    let exp: exp = sem_exp ss exp in
    let typ = match typ with
      | None -> exp.typ
      | Some typ ->
        let typ = sem_typ ss typ in
        typecheck p typ exp.typ;
        typ
    in
    ss#insert {p = p; id = snd id; u = Val (typ, exp); llv = llv}

  | Ast1.Var (p, id, typ, exp) ->
    let exp = match typ, exp with
      | None, None -> raise (ErrUntypedVarDec def)
      | None, Some exp -> sem_exp ss exp
      | Some typ, None -> {p = p; typ = sem_typ ss typ; u = ZeroValue}
      | Some typ, Some exp ->
        let typ = sem_typ ss typ in
        let exp = sem_exp ss exp in
        typecheck p typ exp.typ;
        exp
    in
    ss#insert {p = p; id = snd id; u = Var (exp.typ, exp); llv = llv}

  | _ -> raise (ErrCompiler "var._")

(* -------------------------------------------------------------------------- *)

and sem_block ss block =
  let f = function
    | Ast1.V v -> V (sem_var ss v)
    | Ast1.S s -> S (sem_stmt ss s)
  in
  List.map f block

(* -------------------------------------------------------------------------- *)

and sem_typ ss typ = match typ with
  | Ast1.Id (_, "Void")   -> Void
  | Ast1.Id (_, "Bool")   -> Bool
  | Ast1.Id (_, "Int")    -> Int
  | Ast1.Id (_, "Float")  -> Float
  | Ast1.Id (_, "String") -> String
  | Ast1.Id (p, id)       -> let (def, _) = find_record ss p id in Record def
  | Ast1.Array typ        -> Array (sem_typ ss typ)

(* -------------------------------------------------------------------------- *)

and sem_stmt ss stmt = match stmt with
  | Asg (p, lhs, _, exp) ->
    let lhs = sem_lhs ss lhs in
    let exp = sem_exp ss exp in
    let ok, err = can_be_reassigned lhs in
    if not ok then raise err;
    typecheck p lhs.typ exp.typ;
    (* TODO: match op *)
    {p = lhs.p; u = Asg (lhs, exp)}

  | Call call ->
    let call = sem_call ss call in
    {p = call.p; u = Call call}

  | Return (p, exp) ->
    let (exp: exp) = match exp with
    | None -> {p = p; typ = Void; u = LiteralNil}
    | Some exp -> sem_exp ss exp
    in
    let ret = match ss#get_ret with
    | None -> raise (ErrCompiler "stmt.ret")
    | Some typ -> typ
    in
    typecheck p ret exp.typ;
    {p = p; u = Ret exp}

  | If (exp, block, elseif, else_) -> raise ErrNotImplemented
  | While (exp, block) -> raise ErrNotImplemented
  | For (id, range, block) -> raise ErrNotImplemented
  | Block block -> raise ErrNotImplemented

(* -------------------------------------------------------------------------- *)

and sem_exp ss exp = match exp with
  | Ast1.Dynamic typ ->
    let p = Lexing.dummy_pos in
    let typ = sem_typ ss typ in
    {p = p; typ = typ; u = Dynamic}

  | Binary (lexp, op, rexp) -> raise ErrNotImplemented
  | Unary (op, exp) -> raise ErrNotImplemented

  | Ast1.Literal Nil     p     -> {p = p; typ = Void;   u = LiteralNil     }
  | Ast1.Literal True    p     -> {p = p; typ = Bool;   u = LiteralTrue    }
  | Ast1.Literal False   p     -> {p = p; typ = Bool;   u = LiteralFalse   }
  | Ast1.Literal Int    (p, v) -> {p = p; typ = Int;    u = LiteralInt    v}
  | Ast1.Literal Float  (p, v) -> {p = p; typ = Float;  u = LiteralFloat  v}
  | Ast1.Literal String (p, v) -> {p = p; typ = String; u = LiteralString v}

  | Ast1.Literal ArrayL (p, exps) ->
    let exps = List.map (sem_exp ss) exps in
    let typ = (List.hd exps).typ in
    let same_type typ (exp: exp) = match typecheck p typ exp.typ with
      | exception _ -> false
      | _ -> true
    in
    if List.for_all (same_type typ) exps
      then {p = p; typ = Array typ; u = LiteralArray exps}
      else raise (ErrLiteralArraySameType p)

  | Ast1.Literal RecordL ((p, id), fields) ->
    let (rec_def, rec_fields) = find_record ss p id in
    let f ((p, id), exp) =
      let exp = sem_exp ss exp in
      let (field: def) = lookup_record rec_fields id (p, rec_def) in
      let field_typ = match field.u with
        | Val (typ, _) | Var (typ, _) -> typ
        | _ -> raise (ErrCompiler "sem_exp.LiteralRecord")
      in
      let (lhs: lhs) = {p = p; typ = field_typ; u = Id field} in
      let ok, err = can_be_reassigned lhs in
      if not ok then raise err;
      typecheck p lhs.typ exp.typ;
      (lhs, exp)
    in
    let fields = List.map f fields in
    {p = p; typ = Record rec_def; u = LiteralRecord fields}

  | Ast1.Lhs lhs ->
    let ({p; typ; _} as lhs: lhs) = sem_lhs ss lhs in
    {p = p; typ = typ; u = Lhs lhs}

  | Call call ->
    let call = sem_call ss call in
    {p = call.p; typ = call.typ; u = Call call}

(* -------------------------------------------------------------------------- *)

and sem_lhs ss lhs = match lhs with
  | Id (p, id2 as id1) -> (* id *)
    let (typ, u) = match ss#lookup id2 with
      | Some ({u = Val (typ, _); _} as def) -> (typ, Id def)
      | Some ({u = Var (typ, _); _} as def) -> (typ, Id def)
      | _ -> raise (ErrUndefinedVariable id1)
    in
    {p; typ; u}

  | Index (p, arr, idx) -> (* arr[idx] *)
    let arr = sem_exp ss arr in
    let idx = sem_exp ss idx in
    let typ = match arr.typ with
      | Array typ -> typ
      | _ -> raise (ErrExpectedArray lhs)
    in
    {p = p; typ = typ; u = Index (arr, idx)}

  | Field (p, exp, (_, field)) -> (* exp.field *)
    let exp = sem_exp ss exp in
    let (rec_def, rec_fields) = match exp.typ with
      | Record ({u = Rec fields; _} as def) -> (def, fields)
      | Record def -> raise @@ ErrExpectedRecord (p, def.u)
      | _ -> raise (ErrCompiler "sem_lhs.Field")
    in
    let field_def = lookup_record rec_fields field (p, rec_def) in
    let typ = match field_def.u with
      | Val (typ, _) | Var (typ, _) -> typ
      | _ -> raise (ErrCompiler "sem_lhs.Field")
    in
    {p = p; typ = typ; u = Field (exp, field_def)}

(* -------------------------------------------------------------------------- *)

and sem_call ss call = match call with
  | Function ((p, id2) as id1, args) ->
    let (fun_def, params, ret_typ) = match ss#lookup id2 with
      | None -> raise (ErrUndefinedFunction id1)
      | Some def ->
        begin match def.u with
        | Fun (params, ret_typ, _) -> (def, params, ret_typ)
        | _ -> raise @@ ErrExpectedFunction (p, def.u)
        end
    in
    let args = List.map (sem_exp ss) args in
    let rec args_vs_params = function
      | [], [] -> ()
      | _, [] -> raise @@ ErrCallTooManyArgs id1
      | [], _ -> raise @@ ErrCallNotEnoughArgs id1
      | (arg: exp) :: args, (param: def) :: params ->
        let param_typ = match param.u with
          | Val (typ, _) -> typ
          | _ -> raise (ErrCompiler "sem_call.function")
        in
        typecheck arg.p param_typ arg.typ;
        args_vs_params (args, params)
    in
    args_vs_params (args, params);
    let (u: call_u) = Fun (fun_def, args) in
    {p = p; typ = ret_typ; u = u}

  | Method (obj, id, args) -> raise ErrNotImplemented

  | Constructor (typ, args) -> raise ErrNotImplemented

(* -------------------------------------------------------------------------- *)

and handle_error e =
  let f = sprintf "error in line %d: %s" in
  let (ln, s) = match e with
    | ST.ErrDuplicate (def, {p; id; u; _}) ->
      let kind = match u with
        | Val _ | Var _ -> "variable"
        | Fun _ -> "function"
        | Rec _ -> "record"
      in
      let ln = def.p.pos_lnum in
      let msg = sprintf "redeclaration of %s '%s' from line %d" in
      ln, msg kind id p.pos_lnum

    | ErrCallNotEnoughArgs ({pos_lnum; _}, id) ->
      let msg = sprintf "too few arguments in call to '%s'" id in
      pos_lnum, msg
    | ErrCallTooManyArgs ({pos_lnum; _}, id) ->
      let msg = sprintf "too many arguments in call to '%s'" id in
      pos_lnum, msg
    | ErrExpectedArray lhs -> -33, "a"
    | ErrExpectedFunction ({pos_lnum; _}, def_u) ->
      (* TODO use def_u *)
      pos_lnum, "expected a function"
    | ErrExpectedRecord (p, typ) -> p.pos_lnum, "a"
    | ErrGlobalVar ({pos_lnum; _}, id) ->
      let msg = sprintf "global variable '%s' must be defined as 'val'" in
      pos_lnum, msg id
    | ErrLiteralArraySameType p ->
      let msg = "array literal elements must have the same type" in
      p.pos_lnum, msg
    | ErrUntypedVarDec def -> -1, "a"
    | ErrUnknownType typ -> -2, "a"
    | ErrReassignedVal (p, id) ->
      let msg = sprintf "cannot reassign to variable '%s' because it was defined as a 'val'" id in
      p.pos_lnum, msg
    | ErrType (p, t1, t2) ->
      let t1 = typ_tostring t1 in
      let t2 = typ_tostring t2 in
      let msg = sprintf "mismatching types: expected %s, got %s" t1 t2 in
      p.pos_lnum, msg
    | ErrUndefinedFunction (p, id) ->
      let msg = sprintf "undefined function '%s'" id in
      p.pos_lnum, msg
    | ErrUndefinedVariable id ->
      let (p, s) = id in
      let msg = sprintf "undefined variable '%s'" s in
      p.pos_lnum, msg
    | e -> -6, Printexc.to_string e
  in
  f ln s
