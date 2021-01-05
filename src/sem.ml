
open Ast2

exception ErrCompiler of string

exception ErrGlobalVar of Lexing.position * id
exception ErrUntypedVarDec of Ast1.def
exception ErrUnknownType of Ast1.typ
exception ErrIndexingNonArray of Ast1.lhs
exception ErrRedeclaration of Ast1.def * Ast2.def
exception ErrUninitializedVal of Ast1.def
exception ErrUndefinedVariable of Ast1.id
exception ErrInvalidVariable of Ast1.id
exception ErrInvalidType of Ast2.typ * Ast2.exp

let dummyV = Llvm.undef @@ Llvm.i1_type @@ Llvm.global_context ()

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
    method lookup id = List.find_map (fun scope -> scope#lookup id) scopes

    (* returns <def> or raises an exception *)
    method insert (def: Ast2.def) =
      match self#lookup def.id with
      | None                -> let scope = List.hd scopes in scope#insert def
      | Some x              -> raise @@ ErrDuplicate (def, x)
      | exception Failure _ -> raise (ErrCompiler "symtable.insert")
  end
end

(* -------------------------------------------------------------------------- *)

let st = new Symtable.table

let rec analyse ast1 =
  st#down;
  let ast2 = List.map sem_top ast1 in
  st#up;
  ast2

(* -------------------------------------------------------------------------- *)

and sem_top def = match def with
  | Ast1.Val _ -> sem_var def

  | Ast1.Var (_, id, _, _) -> raise @@ ErrGlobalVar (fst id, snd id)

  | Ast1.Fun (pos, (_, id), params, typ, block) ->
    st#down;
    let typ = match typ with None -> Void | Some typ -> sem_typ typ in
    let f = function
      | Ast1.Val (pos, id, Some _, Ast1.Dynamic _) as def -> sem_var def
      | _ -> raise (ErrCompiler "top.Fun")
    in
    let params = List.map f params in
    let block = sem_block block in
    st#up;
    {pos = pos; id = id; typ = typ; u = Fun (params, block); llv = dummyV}

(* -------------------------------------------------------------------------- *)

and sem_var def = match def with
  | Ast1.Val (pos, id, typ, exp) ->
    let exp: exp = sem_exp exp in
    let typ = match typ with None -> exp.typ | Some typ -> sem_typ typ in
    st#insert {pos = pos; id = snd id; typ = typ; u = Val exp; llv = dummyV}

  | Ast1.Var (pos, id, typ, exp) ->
    let exp = match typ, exp with
      | None, None -> raise (ErrUntypedVarDec def)
      | None, Some exp -> sem_exp exp
      | Some typ, None -> {pos = pos; typ = sem_typ typ; u = ZeroValue}
      | Some typ, Some exp ->
        let typ = sem_typ typ in
        let exp = sem_exp exp in
        if not (typ = exp.typ)
          then raise @@ ErrInvalidType (typ, exp)
          else exp
    in
    let id = snd id in
    st#insert {pos = pos; id = id; typ = exp.typ; u = Var exp; llv = dummyV}

  | _ -> raise (ErrCompiler "var._")

(* -------------------------------------------------------------------------- *)

and sem_block block =
  let f = function
    | Ast1.V v -> V (sem_var v)
    | Ast1.S s -> S (sem_stmt s)
  in
  st#down;
  let block = List.map f block in
  st#up;
  block

(* -------------------------------------------------------------------------- *)

and sem_typ typ = match typ with
  | Ast1.Id (_, "Bool")   -> Bool
  | Ast1.Id (_, "Int")    -> Int
  | Ast1.Id (_, "Float")  -> Float
  | Ast1.Id (_, "String") -> String
  | Ast1.Id _             -> raise (ErrUnknownType typ)
  | Ast1.Array typ        -> Array (sem_typ typ)

(* -------------------------------------------------------------------------- *)

and sem_stmt stmt = match stmt with
  | Asg (lhs, exp) ->
    let lhs = sem_lhs lhs in
    let exp = sem_exp exp in
    (* TODO: match types *)
    {pos = lhs.pos; u = Asg (lhs, exp)}

(* -------------------------------------------------------------------------- *)

and sem_exp exp = match exp with
  | Ast1.Dynamic typ ->
    let pos = Lexing.dummy_pos in
    let typ = sem_typ typ in
    {pos = pos; typ = typ; u = Dynamic}

  | Ast1.Literal True    p     -> {pos = p; typ = Bool;   u = LiteralTrue    }
  | Ast1.Literal False   p     -> {pos = p; typ = Bool;   u = LiteralFalse   }
  | Ast1.Literal Int    (p, v) -> {pos = p; typ = Int;    u = LiteralInt    v}
  | Ast1.Literal Float  (p, v) -> {pos = p; typ = Float;  u = LiteralFloat  v}
  | Ast1.Literal String (p, v) -> {pos = p; typ = String; u = LiteralString v}

  | Ast1.Lhs lhs ->
    let {pos; typ; _} as lhs = sem_lhs lhs in
    {pos = pos; typ = typ; u = Lhs lhs}

(* -------------------------------------------------------------------------- *)

and sem_lhs lhs = match lhs with
  | Id (pos, id2 as id1) ->
    let def = match st#lookup id2 with
      | None     -> raise @@ ErrUndefinedVariable id1
      | Some def -> def
    in
    let _ = match def.u with
      | Val _ | Var _ -> ()
      | _             -> raise @@ ErrInvalidVariable id1
    in
    {pos = pos; typ = def.typ; u = Id (id2, def)}

  | Indexed (pos, arr, index) ->
    let arr = sem_exp arr in
    let index = sem_exp index in
    let typ = match arr.typ with
      | Array typ -> typ
      | _         -> raise (ErrIndexingNonArray lhs)
    in
    {pos = pos; typ = typ; u = Indexed (arr, index)}
