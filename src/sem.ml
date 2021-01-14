
open Ast2
open Printf

exception ErrNotImplemented

exception ErrCompiler of string

exception ErrGlobalVar of Lexing.position * id
exception ErrIndexingNonArray of Ast1.lhs
exception ErrInvalidType of Ast2.typ * Ast2.exp
exception ErrInvalidVariable of Ast1.id
exception ErrRecordOnlyVariables of Ast1.def
exception ErrRedeclaration of Ast1.def * Ast2.def
exception ErrUndefinedVariable of Ast1.id
(* exception ErrUninitializedVal of Ast1.def *)
exception ErrUnknownType of Ast1.typ
exception ErrUntypedVarDec of Ast1.def

(* dummy LLVM value *)
let llv = Llvm.undef @@ Llvm.i1_type @@ Llvm.global_context ()

(* -------------------------------------------------------------------------- *)

module Symtable = struct
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

    method down = scopes <- new scope :: scopes

    method up =
      try scopes <- List.tl scopes
      with Failure _ -> raise (ErrCompiler "symtable.up")

    (* returns None if could not find a <def> with <id> *)
    (* otherwise returns <def>                          *)
    method lookup id = List.find_map (fun scope -> scope#lookup id) scopes

    (* returns <def> or raises an exception *)
    method insert (def: Ast2.def) =
      match self#lookup def.id with
      | None                -> let scope = List.hd scopes in scope#insert def
      | Some originaldef    -> raise @@ ErrDuplicate (def, originaldef)
      | exception Failure _ -> raise (ErrCompiler "symtable.insert")
  end
end

(* -------------------------------------------------------------------------- *)

let rec analyse ast1 =
  let st = new Symtable.table in
  st#down;
  let ast2 =
    try Ok (List.map (fun def -> sem_top st def) ast1)
    with e -> Error (handle_error e)
  in
  st#up;
  ast2

(* -------------------------------------------------------------------------- *)

and sem_top st def = match def with
  | Ast1.Val _ -> sem_var st def

  | Ast1.Var (_, id, _, _) -> raise @@ ErrGlobalVar (fst id, snd id)

  | Ast1.Fun (p, (_, id), params, typ, block) ->
    let typ = match typ with
      | None -> Void
      | Some typ -> sem_typ st typ
    in
    let dummyU = Fun ([], []) in
    let fun_ = {p = p; id = id; typ = typ; u = dummyU; llv = llv} in
    let _ = st#insert fun_ in
    st#down;
    let f = function
      | Ast1.Val (p, id, Some _, Ast1.Dynamic _) as param -> sem_var st param
      | _ -> raise (ErrCompiler "top.Fun")
    in
    let params = List.map f params in
    fun_.u <- Fun (params, []);
    let block = sem_block st block in
    fun_.u <- Fun (params, block);
    st#up;
    fun_

  | Ast1.Rec (p, (_, id), defs) ->
    let dummyU = Rec [] in
    let rec_ = {p = p; id = id; typ = Record id; u = dummyU; llv = llv} in
    let f (def: Ast1.def) = match def with
      | Val _ -> sem_var st def
      | Var _ -> sem_var st def
      | _     -> raise (ErrRecordOnlyVariables def)
    in
    let _ = st#insert rec_ in
    st#down;
    rec_.u <- Rec (List.map f defs);
    st#up;
    rec_

(* -------------------------------------------------------------------------- *)

and sem_var st def = match def with
  | Ast1.Val (p, id, typ, exp) ->
    let exp: exp = sem_exp st exp in
    let typ = match typ with
      | None     -> exp.typ
      | Some typ -> sem_typ st typ
    in
    st#insert {p = p; id = snd id; typ = typ; u = Val exp; llv = llv}

  | Ast1.Var (p, id, typ, exp) ->
    let exp = match typ, exp with
      | None, None -> raise (ErrUntypedVarDec def)
      | None, Some exp -> sem_exp st exp
      | Some typ, None -> {p = p; typ = sem_typ st typ; u = ZeroValue}
      | Some typ, Some exp ->
        let typ = sem_typ st typ in
        let exp = sem_exp st exp in
        if not (typ = exp.typ)
          then raise @@ ErrInvalidType (typ, exp)
          else exp
    in
    let id = snd id in
    st#insert {p = p; id = id; typ = exp.typ; u = Var exp; llv = llv}

  | _ -> raise (ErrCompiler "var._")

(* -------------------------------------------------------------------------- *)

and sem_block st block =
  let f = function
    | Ast1.V v -> V (sem_var st v)
    | Ast1.S s -> S (sem_stmt st s)
  in
  st#down;
  let block = List.map f block in
  st#up;
  block

(* -------------------------------------------------------------------------- *)

and sem_typ st typ = match typ with
  | Ast1.Id (_, "Bool")   -> Bool
  | Ast1.Id (_, "Int")    -> Int
  | Ast1.Id (_, "Float")  -> Float
  | Ast1.Id (_, "String") -> String
  | Ast1.Id _             -> raise (ErrUnknownType typ)
  | Ast1.Array typ        -> Array (sem_typ st typ)

(* -------------------------------------------------------------------------- *)

and sem_stmt st stmt = match stmt with
  | Asg (lhs, _, exp) ->
    let lhs = sem_lhs st lhs in
    let exp = sem_exp st exp in
    (* TODO: match op *)
    (* TODO: match types *)
    {p = lhs.p; u = Asg (lhs, exp)}

  | Call call -> raise ErrNotImplemented
  | Return exp ->
    begin match exp with
    | None -> raise ErrNotImplemented
    | Some exp -> raise ErrNotImplemented
    end
  | If (exp, block, elseif, else_) -> raise ErrNotImplemented
  | While (exp, block) -> raise ErrNotImplemented
  | For (id, range, block) -> raise ErrNotImplemented
  | Block block -> raise ErrNotImplemented

(* -------------------------------------------------------------------------- *)

and sem_exp st exp = match exp with
  | Ast1.Dynamic typ ->
    let p = Lexing.dummy_pos in
    let typ = sem_typ st typ in
    {p = p; typ = typ; u = Dynamic}

  | Binary (lexp, op, rexp) -> raise ErrNotImplemented
  | Unary (op, exp) -> raise ErrNotImplemented

  | Ast1.Literal True    p     -> {p = p; typ = Bool;   u = LiteralTrue    }
  | Ast1.Literal False   p     -> {p = p; typ = Bool;   u = LiteralFalse   }
  | Ast1.Literal Int    (p, v) -> {p = p; typ = Int;    u = LiteralInt    v}
  | Ast1.Literal Float  (p, v) -> {p = p; typ = Float;  u = LiteralFloat  v}
  | Ast1.Literal String (p, v) -> {p = p; typ = String; u = LiteralString v}

  | Ast1.Lhs lhs ->
    let {p; typ; _} as lhs = sem_lhs st lhs in
    {p = p; typ = typ; u = Lhs lhs}

  | Call call -> raise ErrNotImplemented

(* -------------------------------------------------------------------------- *)

and sem_lhs st lhs = match lhs with
  | Id (p, id2 as id1) ->
    let def = match st#lookup id2 with
      | None     -> raise (ErrUndefinedVariable id1)
      | Some def -> def
    in
    let _ = match def.u with
      | Val _ | Var _ -> ()
      | _             -> raise (ErrInvalidVariable id1)
    in
    {p = p; typ = def.typ; u = Id (id2, def)}

  | Indexed (p, arr, index) ->
    let arr = sem_exp st arr in
    let index = sem_exp st index in
    let typ = match arr.typ with
      | Array typ -> typ
      | _         -> raise (ErrIndexingNonArray lhs)
    in
    {p = p; typ = typ; u = Indexed (arr, index)}

(* -------------------------------------------------------------------------- *)

and handle_error e =
  let f = sprintf "error in line %d: %s" in
  let (ln, s) = match e with
    | Symtable.ErrDuplicate (def, {p; id; u; _}) ->
      let kind = match u with
        | Val _ -> "variable"
        | Var _ -> "variable"
        | Fun _ -> "function"
        | Rec _ -> "record"
      in
      let ln = def.p.pos_lnum in
      let msg = sprintf "redeclaration of %s '%s' from line %d" in
      ln, msg kind id p.pos_lnum
    | ErrGlobalVar ({pos_lnum; _}, id) ->
      let ln = pos_lnum in
      let msg = sprintf "global variable '%s' must be defined as 'val'" in
      ln, msg id
    | ErrUntypedVarDec def          -> -1, "a"
    | ErrUnknownType typ            -> -1, "a"
    | ErrIndexingNonArray lhs       -> -1, "a"
    | ErrRedeclaration (def1, def2) -> -1, "a"
    | ErrUndefinedVariable id       -> -1, "a"
    | ErrInvalidVariable id         -> -1, "a"
    | ErrInvalidType (typ, exp)     -> -1, "a"
    | e                             -> -1, Printexc.to_string e
  in
  f ln s
