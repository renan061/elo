
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
    let voidAT    = Llvm.void_type ctx
    let boolAT    = Llvm.i1_type ctx
    let int32AT   = Llvm.i32_type ctx
    let float32AT = Llvm.float_type ctx
    let stringAT  = ptrAnyT
    (* arrayT *)

    let type_from = function
        | Void    -> voidAT
        | Bool    -> boolAT
        | Int     -> int32AT
        | Float   -> float32AT
        | String  -> stringAT
        | Array _ -> raise NotImplemented

    let voidAV    = Llvm.const_null voidAT
    let trueAV    = Llvm.const_int boolAT 1
    let falseAV   = Llvm.const_int boolAT 0
    let int32AV   = Llvm.const_int int32AT
    let float32AV = Llvm.const_float float32AT
    let stringAV  = Llvm.const_string ctx (* TODO: bitcast to stringAT *)

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
    | LiteralTrue       -> IR.trueAV
    | LiteralFalse      -> IR.falseAV
    | LiteralInt    v   -> IR.int32AV   v
    | LiteralFloat  v   -> IR.float32AV v
    | LiteralString v   -> IR.stringAV  v
    | Lhs {pos; typ; u} -> (match u with
        | Id (_, def) -> (match def.u with
            | Val _ -> def.llv
            | Var _ -> Llvm.build_load def.llv IR.tmp irs.b
            | _     -> raise (ErrBackend "exp.lhs._"))
        | Indexed (arr, idx) -> raise NotImplemented)
