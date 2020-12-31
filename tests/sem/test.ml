
let tests = [(
(* -------------------------------------------------------------------------- *)
(* GLOBAL VARIABLES --------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
    (*
        TODO:
        "global variables - immutable array types"
        "global variables - mutable array types"
        "global variables - immutable record types"
        "global variables - mutable record types"
        "global variables - regular monitor types"
        "global variables - unlocked monitor types"
        "global variables - condition queue type"
    *)
    "global variables - primitive types", {|
        val a1 = true;
        val b1 = 1;
        val c1 = 2.0;
        val d1 = "3";
        val a2: Bool = false;
        val b2: Int = 4;
        val c2: Float = 5.0;
        val d2: String = "6";
    |}, {|
        DEF VAL a1 : BOOL = BOOL(true)
        DEF VAL b1 : INT = INT(1)
        DEF VAL c1 : FLOAT = FLOAT(2.)
        DEF VAL d1 : STRING = STRING("3")
        DEF VAL a2 : BOOL = BOOL(false)
        DEF VAL b2 : INT = INT(4)
        DEF VAL c2 : FLOAT = FLOAT(5.)
        DEF VAL d2 : STRING = STRING("6")
    (*-----------------------------------------------------------------*) |}); (
    "global variables - must be val", {|
        var a = 1;
    |}, {|
        error in line 2: global variable 'a' must be defined as 'val'
    (*-----------------------------------------------------------------*) |}); (
    "global variables - redeclaration", {|
        val x = true;
        val x = false;
    |}, {|
        error in line 3: redeclaration of variable 'x' from line 2
    |}); (
(* -------------------------------------------------------------------------- *)
(* FUNCTION DEFINITION ------------------------------------------------------ *)
(* -------------------------------------------------------------------------- *)
    "function definition - ok", {|
        function main(args: [String]) {
            val name = args[0];
            val options = args[1];
        }
    |}, {|
        FUNCTION main (DEF VAL args : [STRING] = DYNAMIC [STRING]) : VOID {
          DEF VAL name : STRING = INDEXED: STRING
            ARRAY ID([STRING]) args
            INDEX INT(0)
        }
    (*-----------------------------------------------------------------*) |}); (
    "function definition - parameters", {|
        function f {}
    |}, {|
        FUNCTION f () : VOID {}
    (*-----------------------------------------------------------------*) |}); (
    "function definition - no parameters", {|
        function f() {}
    |}, {|
        error in line -1: Elo.Parser.MenhirBasics.Error
    |})
]

(* -------------------------------------------------------------------------- *)

open Printf
open Elo
open Ast2

exception ErrPrinter

let rec repeat s n = if n = 0 then "" else s ^ repeat s (n - 1)
let tabs = repeat "  "
let map_concat l f sep = String.concat sep (List.map f l)

(* -------------------------------------------------------------------------- *)

let rec tostring_ast2 ast2 = map_concat ast2 tostring_top "\n"

and tostring_top (def: def) =
    let n = 0 in
    match def.u with
    | Val _ -> tostring_var n def
    | Fun (params, block) ->
        let typ = tostring_typ def.typ in
        let block = tostring_block block (n + 1) in
        (* TODO: assert param exps are Dynamic *)
        begin match params with
        | [] ->
            String.concat " " ["FUNCTION"; def.id; "() :"; typ; block]
        | _ ->
            let buf = Buffer.create 128 in
            let s = String.concat " " ["DEF FUNCTION"; def.id; ":"; typ] in
            Buffer.add_string buf s;
            Buffer.add_string buf "\n";
            let n = n + 1 in
            Buffer.add_string buf (tabs n);
            Buffer.add_string buf "PARAMETERS (";
            Buffer.add_string buf "\n";
            let params = List.map (tostring_var (n + 1)) params in
            Buffer.add_string buf (String.concat "\n" params);
            Buffer.add_string buf "\n";
            Buffer.add_string buf (tabs n);
            Buffer.add_string buf "TODO BLOCK\n";
            Buffer.contents buf
        end
    | _ -> raise ErrPrinter

and tostring_var n (def: def) =
    let typ = tostring_typ def.typ in
    let (vartype, exp) = match def.u with
        | Val exp -> ("VAL", tostring_exp n exp)
        | Var exp -> ("VAR", tostring_exp n exp)
        | _       -> raise ErrPrinter
    in
    let exp = "\n" ^ tabs (n + 1) ^ exp in
    String.concat " " ["DEF"; vartype; def.id; ":"; typ; "="; exp]

and tostring_block block n = match block with
    | [] -> "{}"
    | _ ->
        let f = function
            | Ast2.V v -> tostring_var  (n + 1) v
            | Ast2.S s -> tostring_stmt (n + 1) s
        in
        let g e = tabs n ^ f e in
        "{\n" ^ String.concat "\n" (List.map g block) ^ "\n}"

and tostring_typ = function
    | Void      -> "VOID"
    | Bool      -> "BOOL"
    | Int       -> "INT"
    | Float     -> "FLOAT"
    | String    -> "STRING"
    | Array typ -> "[" ^ tostring_typ typ ^ "]"

and tostring_stmt n (stmt: stmt) = match stmt.u with
    | Asg (lhs, exp) ->
        let lhs = tostring_lhs n lhs in
        let exp = tostring_exp n exp in
        String.concat " " ["ASG"; lhs; "="; exp]

and tostring_exp n (exp: exp) =
    let typ = tostring_typ exp.typ in
    match exp.u with
    | Dynamic         -> "DYNAMIC "   ^ typ
    | ZeroValue       -> "ZEROVALUE " ^ typ
    | LiteralTrue     -> typ ^ "(true)"
    | LiteralFalse    -> typ ^ "(false)"
    | LiteralInt    v -> typ ^ "("   ^ string_of_int   v ^   ")"
    | LiteralFloat  v -> typ ^ "("   ^ string_of_float v ^   ")"
    | LiteralString v -> typ ^ "(\"" ^                 v ^ "\")"
    | Lhs lhs         -> tostring_lhs n lhs

and tostring_lhs n lhs =
    let typ = tostring_typ lhs.typ in
    match lhs.u with
    | Id (id, _) ->
        "ID " ^ id ^ ": " ^ typ
    | Indexed (arr, idx) ->
        let n = n + 1 in
        let arr = tostring_exp n arr in
        let idx = tostring_exp n idx in
        "INDEXED: " ^ typ ^ "\n" ^
            tabs n ^ "ARRAY => " ^ arr ^ "\n" ^
            tabs n ^ "INDEX => " ^ idx

let handle_error e =
    let f = sprintf "error in line %d: %s" in
    let (ln, s) = match e with
        | Sem.Symtable.ErrDuplicate (def, {id; pos; _}) ->
            let ln = def.pos.pos_lnum in
            let msg = sprintf "redeclaration of variable '%s' from line %d" in
            ln, msg id pos.pos_lnum
        | Sem.ErrGlobalVar ({pos_lnum; _}, id) ->
            let ln = pos_lnum in
            let msg = sprintf "global variable '%s' must be defined as 'val'" in
            ln, msg id
        | Sem.ErrUntypedVarDec def          -> -1, "a"
        | Sem.ErrUnknownType typ            -> -1, "a"
        | Sem.ErrIndexingNonArray lhs       -> -1, "a"
        | Sem.ErrRedeclaration (def1, def2) -> -1, "a"
        | Sem.ErrUninitializedVal def       -> -1, "a"
        | Sem.ErrUndefinedVariable id       -> -1, "a"
        | Sem.ErrInvalidVariable id         -> -1, "a"
        | Sem.ErrInvalidType (typ, exp)     -> -1, "a"
        | e                                 -> -1, Printexc.to_string e
    in
    f ln s

let test () =
    let f (name, input, expected) =
        let output = try
            let lexbuf = Lexing.from_string input in
            let ast1 = Parser.program Scanner.scan lexbuf in
            let ast2 = Sem.analyse ast1 in
            tostring_ast2 ast2
            with e -> handle_error e
        in
        let splitout = String.split_on_char '\n' output in
        let expected = String.split_on_char '\n' expected in
        let expected = List.rev (List.tl (List.rev (List.tl expected))) in
        let f s =
            let buf = Buffer.create (String.length s - 8) in
            String.iteri (fun i c -> if i >= 8 then Buffer.add_char buf c) s;
            Buffer.contents buf
        in
        let expected = List.map f expected in

        let rec eq n = function
            | [], [] -> true, 0, "", ""
            | x, []  -> false, n, String.concat "\n" x, ""
            | [], y  -> false, n, "", String.concat "\n" y
            | x :: xs, y :: ys ->
                if x = y then eq (n + 1) (xs, ys) else false, n, x, y
        in
        let (ok, line, l1, l2) = eq 1 (splitout, expected) in
        if ok then
            printf "Sem ok <%s>\n" name
        else
            printf "Sem error <%s> => line %d\n---\n%s\n---\n%s\n---\n%s\n"
                name line l1 l2 output
    in
    List.iter f tests
;;

test ()