
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
    let voidET    = Llvm.void_type ctx
    let boolET    = Llvm.i1_type ctx
    let int32ET   = Llvm.i32_type ctx
    let float32ET = Llvm.float_type ctx
    let stringET  = ptrAnyT
    (* arrayT *)

    let type_from = function
        | Void    -> voidET
        | Bool    -> boolET
        | Int     -> int32ET
        | Float   -> float32ET
        | String  -> stringET
        | Array _ -> raise NotImplemented

    (* elo values *)
    let voidEV    = Llvm.const_null voidET
    let trueEV    = Llvm.const_int boolET 1
    let falseEV   = Llvm.const_int boolET 0
    let int32EV   = Llvm.const_int int32ET
    let float32EV = Llvm.const_float float32ET
    let stringEV  = Llvm.const_string ctx (* TODO: bitcast to stringET *)

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

and backend_block irs block =
    let f = function
        | V v -> backend_local_var irs v
        | S s -> raise NotImplemented
        | _ -> raise (ErrBackend "block._")
    in
    List.iter f block

and backend_local_var irs def = match def.u with
    | Val exp ->
        def.llv <- backend_exp irs exp

    | Var exp ->
        let v = backend_exp irs exp in
        let p = Llvm.build_alloca (IR.type_from def.typ) IR.tmp irs.b in
        Llvm.build_store v p irs.b;
        def.llv <- p

    | _ -> raise (ErrBackend "var._")

and backend_exp irs {pos; typ; u} = match u with
    | Dynamic           -> raise ErrDynamicExp
    | ZeroValue         -> IR.zero_value typ
    | LiteralTrue       -> IR.trueEV
    | LiteralFalse      -> IR.falseEV
    | LiteralInt    v   -> IR.int32EV   v
    | LiteralFloat  v   -> IR.float32EV v
    | LiteralString v   -> IR.stringEV  v
    | Lhs {pos; typ; u} -> (match u with
        | Id (_, def) -> (match def.u with
            | Val _ -> def.llv
            | Var _ -> Llvm.build_load def.llv IR.tmp irs.b
            | _     -> raise (ErrBackend "exp.lhs._"))
        | Indexed (arr, idx) -> raise NotImplemented)
