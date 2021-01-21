
open Ast2

exception NotImplemented
exception ErrBackend of string (* internal error *)

exception ErrDynamicExp

let ctx = Llvm.global_context ()

(* -------------------------------------------------------------------------- *)

module IR = struct
  type state = {
    m: Llvm.llmodule;
    b: Llvm.llbuilder;
  }

  let tmp = "t"

  (* internal types *)
  let ptrT t  = Llvm.pointer_type t
  let ptrAnyT = ptrT (Llvm.i8_type ctx)

  (* elo types *)
  let void_et    = Llvm.void_type ctx
  let bool_et    = Llvm.i1_type ctx
  let int32_et   = Llvm.i32_type ctx
  let float32_et = Llvm.float_type ctx
  let string_et  = ptrAnyT
  (* arrayT *)

  let type_from = function
    | Void     -> void_et
    | Bool     -> bool_et
    | Int      -> int32_et
    | Float    -> float32_et
    | String   -> string_et
    | Array  _ -> raise NotImplemented
    | Record _ -> raise NotImplemented

  (* elo values *)
  let nil_ev     = Llvm.const_null void_et
  let true_ev    = Llvm.const_int bool_et 1
  let false_ev   = Llvm.const_int bool_et 0
  let int32_ev   = Llvm.const_int int32_et
  let float32_ev = Llvm.const_float float32_et
  let string_ev  = Llvm.const_string ctx (* TODO: bitcast to string_et *)

  let zero_value typ = Llvm.const_null (type_from typ)
end

(* -------------------------------------------------------------------------- *)

let rec compile ast2 =
  let m = Llvm.create_module ctx "main" in
  let b = Llvm.builder ctx in
  let irs: IR.state = {m = m; b = b} in
  List.iter (backend_top irs) ast2;
  m

and backend_top irs (def: def) = match def.u with
  | Val exp ->
    def.llv <- Llvm.declare_global (IR.type_from def.typ) def.id irs.m
    (* TODO: exp *)

  | Var exp -> raise (ErrBackend "top.var")

  | Fun (params, block) ->
    let retT = IR.type_from def.typ in
    let f ({typ; u; _}: def) = match u with
      | Val _ -> IR.type_from typ
      | _ -> raise (ErrBackend "top.fun._")
    in
    let paramsT = Array.of_list (List.map f params) in
    let funT = Llvm.function_type retT paramsT in
    def.llv <- Llvm.define_function def.id funT irs.m;
    Llvm.position_at_end (Llvm.entry_block def.llv) irs.b;
    backend_block irs block

  | _ -> raise NotImplemented

and backend_block irs block =
  let f = function
    | V v -> backend_local_var irs v
    | S s -> raise NotImplemented
  in
  List.iter f block

and backend_local_var irs def = match def.u with
  | Val exp ->
    def.llv <- backend_exp irs exp

  | Var exp ->
    let v = backend_exp irs exp in
    let p = Llvm.build_alloca (IR.type_from def.typ) IR.tmp irs.b in
    let _ = Llvm.build_store v p irs.b in
    def.llv <- p

  | _ -> raise (ErrBackend "var._")

and backend_exp irs {p; typ; u} = match u with
  | Dynamic           -> raise ErrDynamicExp
  | ZeroValue         -> IR.zero_value typ
  | LiteralNil        -> IR.nil_ev
  | LiteralTrue       -> IR.true_ev
  | LiteralFalse      -> IR.false_ev
  | LiteralInt    v   -> IR.int32_ev   v
  | LiteralFloat  v   -> IR.float32_ev v
  | LiteralString v   -> IR.string_ev  v
  | LiteralArray  _   -> raise NotImplemented
  | Lhs {p; typ; u} ->
    begin match u with
    | Id (_, def) ->
      begin match def.u with
      | Val _ -> def.llv
      | Var _ -> Llvm.build_load def.llv IR.tmp irs.b
      | _     -> raise (ErrBackend "exp.lhs._")
      end
    | Indexed (arr, idx) -> raise NotImplemented
    end
