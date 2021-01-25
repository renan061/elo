
(*
  TODO: TYPES
  TYPE CHECKING
  GLOBAL VARIABLES
  FUNCTION DEFINITIONS
  RECORD DEFINITIONS
  STATEMENTS
*)

(*
  TODO

  uninitialized val
*)

(*
"type checking - Bool != Void"
"type checking - Bool != Int"
"type checking - Bool != Float"
"type checking - Bool != String"
"type checking - Bool != Array"
"type checking - Bool != Record"

"type checking - Int != Void"
"type checking - Int != Bool"
"type checking - Int != Float"
"type checking - Int != String"
"type checking - Int != Array"
"type checking - Int != Record"

"type checking - Float != Void"
"type checking - Float != Bool"
"type checking - Float != Int"
"type checking - Float != String"
"type checking - Float != Array"
"type checking - Float != Record"

"type checking - String != Void"
"type checking - String != Bool"
"type checking - String != Int"
"type checking - String != Float"
"type checking - String != Array"
"type checking - String != Record"

"type checking - Array != Void"
"type checking - Array != Bool"
"type checking - Array != Int"
"type checking - Array != Float"
"type checking - Array != String"
"type checking - Array != Record"

"type checking - Record != Void"
"type checking - Record != Bool"
"type checking - Record != Int"
"type checking - Record != Float"
"type checking - Record != String"
"type checking - Record != Array"

"type checking - Array [A] != Array [B]"
"type checking - Array [[A]] != Array [A]"
"type checking - Record A != Record B"
*)

let tests = [(
(* ------------------------------------------------------------------- *)
(* TYPE CHECKING ----------------------------------------------------- *)
(* ------------------------------------------------------------------- *)
  "type checking - matching types", {|
    val a: Void = nil;
    val b1: Bool = true;
    val b2: Bool = false;
    val c: Int = 10;
    val d: Float = 3.1415;
    val e: String = "string";
    val f1: [Int] = [1, 2, 3];
    val f2: [[String]] = [["hello", "world"]];
    record R {}
    val g = R{};
  |}, {|
    DEF VAL a : VOID =
      VOID(nil)
    DEF VAL b1 : BOOL =
      BOOL(true)
    DEF VAL b2 : BOOL =
      BOOL(false)
    DEF VAL c : INT =
      INT(10)
    DEF VAL d : FLOAT =
      FLOAT(3.1415)
    DEF VAL e : STRING =
      STRING("string")
    DEF VAL f1 : [INT] =
      [INT] [INT(1), INT(2), INT(3)]
    DEF VAL f2 : [[STRING]] =
      [[STRING]] [[STRING] [STRING("hello"), STRING("world")]]
    DEF RECORD R {}
    DEF VAL g : RECORD R =
      R {}
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Void != Int", {|
    val a: Void = 1;
  |}, {|
    error in line 2: mismatching types: expected Void, got Int
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Void != Float", {|
    val a: Void = 5.5;
  |}, {|
    error in line 2: mismatching types: expected Void, got Float
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Void != String", {|
    val a: Void = "";
  |}, {|
    error in line 2: mismatching types: expected Void, got String
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Void != Array", {|
    val a: Void = [1];
  |}, {|
    error in line 2: mismatching types: expected Void, got [Int]
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Void != Record", {|
    record R {}
    val a: Void = R{};
  |}, {|
    error in line 3: mismatching types: expected Void, got R
(* ------------------------------------------------------------------- *) |}); (
(* GLOBAL VARIABLES -------------------------------------------------- *)
(* ------------------------------------------------------------------- *)
  "global variables - ok", {|
    val a1 = true;
    val b1 = 1;
    val c1 = 2.0;
    val d1 = "3";
    val a2: Bool = false;
    val b2: Int = 4;
    val c2: Float = 5.0;
    val d2: String = "6";
  |}, {|
    DEF VAL a1 : BOOL =
      BOOL(true)
    DEF VAL b1 : INT =
      INT(1)
    DEF VAL c1 : FLOAT =
      FLOAT(2.)
    DEF VAL d1 : STRING =
      STRING("3")
    DEF VAL a2 : BOOL =
      BOOL(false)
    DEF VAL b2 : INT =
      INT(4)
    DEF VAL c2 : FLOAT =
      FLOAT(5.)
    DEF VAL d2 : STRING =
      STRING("6")
  (*-------------------------------------------------------------------*) |}); (
  "global variables - must be val", {|
    var a = 1;
  |}, {|
    error in line 2: global variable 'a' must be defined as 'val'
  (*-------------------------------------------------------------------*) |}); (
  "global variables - redeclaration (variable x variable)", {|
    val x = true;
    val x = false;
  |}, {|
    error in line 3: redeclaration of variable 'x' from line 2
  (*-------------------------------------------------------------------*) |}); (
  "global variables - redeclaration (function x variable)", {|
    function x {}
    val x = true;
  |}, {|
    error in line 3: redeclaration of function 'x' from line 2
  (*-------------------------------------------------------------------*) |}); (
  "global variables - uppercase id", {|
    val A = true;
  |}, {|
    Elo.Parser.MenhirBasics.Error
  (*-------------------------------------------------------------------*) |}); (
  "global variables - mismatching types", {|
    val a: [Bool] = true;
  |}, {|
    error in line 2: mismatching types: expected [Bool], got Bool
(* ------------------------------------------------------------------- *) |}); (
(* FUNCTION DEFINITION ----------------------------------------------- *)
(* ------------------------------------------------------------------- *)
  "function definition - ok", {|
    function f(x: Int, y: Bool, z: [Int]) {
      val a = "A";
      val b = "B";
    }
  |}, {|
    DEF FUNCTION f : VOID
      PARAMETERS (
        DEF VAL x : INT =
          DYNAMIC INT,
        DEF VAL y : BOOL =
          DYNAMIC BOOL,
        DEF VAL z : [INT] =
          DYNAMIC [INT]
      )
    {
      DEF VAL a : STRING =
        STRING("A")
      DEF VAL b : STRING =
        STRING("B")
    }
  (*-------------------------------------------------------------------*) |}); (
  "function definition - recursion visibility", {|
    function f {
      f();
    }
  |}, {|
    DEF FUNCTION f () : VOID {}
  (*-------------------------------------------------------------------*) |}); (
  "function definition - without parameters / without return type", {|
    function f {}
  |}, {|
    DEF FUNCTION f () : VOID {}
  (*-------------------------------------------------------------------*) |}); (
  "function definition - with one parameter / without return type", {|
    function f(a: Bool) {}
  |}, {|
    DEF FUNCTION f : VOID
      PARAMETERS (
        DEF VAL a : BOOL =
          DYNAMIC BOOL
      )
    {}
  (*-------------------------------------------------------------------*) |}); (
  "function definition - with multiple parameters / without return type", {|
    function f1(a: Int, b: Float) {}
    function f2(a: Bool, b: Float, c: Int) {}
  |}, {|
    DEF FUNCTION f1 : VOID
      PARAMETERS (
        DEF VAL a : INT =
          DYNAMIC INT,
        DEF VAL b : FLOAT =
          DYNAMIC FLOAT
      )
    {}
    DEF FUNCTION f2 : VOID
      PARAMETERS (
        DEF VAL a : BOOL =
          DYNAMIC BOOL,
        DEF VAL b : FLOAT =
          DYNAMIC FLOAT,
        DEF VAL c : INT =
          DYNAMIC INT
      )
    {}
  (*-------------------------------------------------------------------*) |}); (
  "function definition - without parameters / with return type", {|
    function f: Bool {}
  |}, {|
    DEF FUNCTION f () : BOOL {}
  (*-------------------------------------------------------------------*) |}); (
  "function definition - with one parameter / with return type", {|
    function f(a: Bool): Float {}
  |}, {|
    DEF FUNCTION f : FLOAT
      PARAMETERS (
        DEF VAL a : BOOL =
          DYNAMIC BOOL
      )
    {}
  (*-------------------------------------------------------------------*) |}); (
  "function definition - with multiple parameters / with return type", {|
    function f1(a: Bool, b: Int): Float {}
    function f2(a: Bool, b: Int, c: Float, d: Float): String {}
  |}, {|
    DEF FUNCTION f1 : FLOAT
      PARAMETERS (
        DEF VAL a : BOOL =
          DYNAMIC BOOL,
        DEF VAL b : INT =
          DYNAMIC INT
      )
    {}
    DEF FUNCTION f2 : STRING
      PARAMETERS (
        DEF VAL a : BOOL =
          DYNAMIC BOOL,
        DEF VAL b : INT =
          DYNAMIC INT,
        DEF VAL c : FLOAT =
          DYNAMIC FLOAT,
        DEF VAL d : FLOAT =
          DYNAMIC FLOAT
      )
    {}
  (*-------------------------------------------------------------------*) |}); (
  "function definition - invalid () for parameters", {|
    function f() {}
  |}, {|
    Elo.Parser.MenhirBasics.Error
  (*-------------------------------------------------------------------*) |}); (
  "function definition - redeclaration (function x function)", {|
    function f : String {}
    function f(a: Int) {}
  |}, {|
    error in line 3: redeclaration of function 'f' from line 2
  (*-------------------------------------------------------------------*) |}); (
  "function definition - redeclaration (variable x function)", {|
    val f = 5;
    function f(a: Int) {}
  |}, {|
    error in line 3: redeclaration of variable 'f' from line 2
  (*-------------------------------------------------------------------*) |}); (
  "function definition - uppercase id", {|
    function F(a: Float) {}
  |}, {|
    Elo.Parser.MenhirBasics.Error
(* ------------------------------------------------------------------- *) |}); (
(* RECORD DEFINITION ------------------------------------------------- *)
(* ------------------------------------------------------------------- *)
  "record definition - ok", {|
    record R {
      val x = 2;
      var y: Int;
    }
  |}, {|
    DEF RECORD R {
      DEF VAL x : INT =
        INT(2)
      DEF VAR y : INT =
        ZEROVALUE INT
    }
  (*-------------------------------------------------------------------*) |}); (
  "record definition - empty record", {|
    record R {}
  |}, {|
    DEF RECORD R {}
  (*-------------------------------------------------------------------*) |}); (
  "record definition - multiple records", {|
    record R1 {
      val a = 1;
    }
    record R2 {
      val a = 1;
      val b: String = "string";
    }
    record R3 {
      var a = 1.0;
    }
    record R4 {
      var a: Float = 1.0;
      var b = true;
    }
    record R5 {
      var a: Int;
    }
    record R6 {
      var a: Int;
      var b: Bool;
    }
    record R7 {
      var a: Float;
      val b = true;
      var c: Int = 100;
      var d = false;
      val e: String = "string";
    }
  |}, {|
    DEF RECORD R1 {
      DEF VAL a : INT =
        INT(1)
    }
    DEF RECORD R2 {
      DEF VAL a : INT =
        INT(1)
      DEF VAL b : STRING =
        STRING("string")
    }
    DEF RECORD R3 {
      DEF VAR a : FLOAT =
        FLOAT(1.)
    }
    DEF RECORD R4 {
      DEF VAR a : FLOAT =
        FLOAT(1.)
      DEF VAR b : BOOL =
        BOOL(true)
    }
    DEF RECORD R5 {
      DEF VAR a : INT =
        ZEROVALUE INT
    }
    DEF RECORD R6 {
      DEF VAR a : INT =
        ZEROVALUE INT
      DEF VAR b : BOOL =
        ZEROVALUE BOOL
    }
    DEF RECORD R7 {
      DEF VAR a : FLOAT =
        ZEROVALUE FLOAT
      DEF VAL b : BOOL =
        BOOL(true)
      DEF VAR c : INT =
        INT(100)
      DEF VAR d : BOOL =
        BOOL(false)
      DEF VAL e : STRING =
        STRING("string")
    }
  (*-------------------------------------------------------------------*) |}); (
  "record definition - redeclaration", {|
    record R {}
    record R {
      var a = 1;
    }
  |}, {|
    error in line 3: redeclaration of record 'R' from line 2
  (*-------------------------------------------------------------------*) |}); (
  "record definition - lowercase id", {|
    record r {}
  |}, {|
    Elo.Parser.MenhirBasics.Error
(* ------------------------------------------------------------------- *) |}); (
(* STATEMENTS -------------------------------------------------------- *)
(* ------------------------------------------------------------------- *)
  "statements - assignment - ok", {|
    function f {
      var a: Int;
      a = 1;
      var b = 2.0;
      b = 3.0;
    }
  |}, {|
    DEF FUNCTION f () : VOID {
      DEF VAR a : INT =
        ZEROVALUE INT
      ASG ID a: INT =
        INT(1)
      DEF VAR b : FLOAT =
        FLOAT(2.)
      ASG ID b: FLOAT =
        FLOAT(3.)
    }
  (*-------------------------------------------------------------------*) |}); (
  "statements - assignment - reassigning val", {|
    function f {
      val a = 1;
      a = 2;
    }
  |}, {|
    error in line 4: cannot reassign to variable 'a' because it was defined as a 'val'
  (*-------------------------------------------------------------------*) |}); (
  "statements - assignment - undefined variable", {|
    function f {
      a = 1;
    }
  |}, {|
    error in line 3: undefined variable 'a'
  (*-------------------------------------------------------------------*) |}); (
  "statements - assignment - mismatching types", {|
    function f {
      var a = 1;
      a = true;
    }
  |}, {|
    error in line 4: mismatching types: expected Int, got Bool
|})]

(* -------------------------------------------------------------------------- *)

open Printf
open Elo
open Ast2

exception ErrPrinter

let tabsize = 2
let rec repeat s n = if n = 0 then "" else s ^ repeat s (n - 1)
let tabs = repeat (repeat " " tabsize)
let map_concat l f sep = String.concat sep (List.map f l)

(* -------------------------------------------------------------------------- *)

let rec tostring_ast2 ast2 = map_concat ast2 tostring_top "\n"

and tostring_top (def: def) =
  let n = 0 in
  match def.u with
  | Val _ -> tostring_var n def
  | Var _ -> tostring_var n def
  | Fun (params, block) ->
    let typ = tostring_typ def.typ in
    let block = tostring_block (n + 1) block in
    (* TODO: assert param exps are Dynamic *)
    begin match params with
    | [] ->
      String.concat " " ["DEF FUNCTION"; def.id; "() :"; typ; block]
    | _ ->
      let buf = Buffer.create 128 in
      let s = String.concat " " ["DEF FUNCTION"; def.id; ":"; typ] in
      Buffer.add_string buf s;
      Buffer.add_string buf "\n";
      let n = n + 1 in
      Buffer.add_string buf (tabs n);
      Buffer.add_string buf "PARAMETERS (";
      Buffer.add_string buf "\n";
      let f def = tabs (n + 1) ^ tostring_var (n + 1) def in
      let params = List.map f params in
      Buffer.add_string buf (String.concat ",\n" params);
      Buffer.add_string buf "\n";
      Buffer.add_string buf (tabs n);
      Buffer.add_string buf ")\n";
      Buffer.add_string buf block;
      Buffer.contents buf
    end
  | Rec defs ->
    let block = List.map (fun def -> V def) defs in
    let block = tostring_block (n + 1) block in
    String.concat " " ["DEF RECORD"; def.id; block]

and tostring_var n (def: def) =
  let typ = tostring_typ def.typ in
  let (vartype, exp) = match def.u with
    | Val exp -> ("VAL", tostring_exp n exp)
    | Var exp -> ("VAR", tostring_exp n exp)
    | _       -> raise ErrPrinter
  in
  let leftside = String.concat " " ["DEF"; vartype; def.id; ":"; typ; "="] in
  let exp = "\n" ^ tabs (n + 1) ^ exp in
  leftside ^ exp

and tostring_block n block = match block with
  | [] -> "{}"
  | _  ->
    let f = function
      | Ast2.V v -> tostring_var  n v
      | Ast2.S s -> tostring_stmt n s
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
  | Record id -> "RECORD " ^ id

and tostring_stmt n (stmt: stmt) = match stmt.u with
  | Asg (lhs, exp) ->
    let lhs = tostring_lhs n lhs in
    let n = n + 1 in
    let exp = tostring_exp n exp in
    let exp = "\n" ^ (tabs n) ^ exp in
    String.concat " " ["ASG"; lhs; "="] ^ exp

and tostring_exp n (exp: exp) =
  let typ = tostring_typ exp.typ in
  match exp.u with
  | Dynamic           -> "DYNAMIC "   ^ typ
  | ZeroValue         -> "ZEROVALUE " ^ typ
  | LiteralNil        -> typ ^ "(nil)"
  | LiteralTrue       -> typ ^ "(true)"
  | LiteralFalse      -> typ ^ "(false)"
  | LiteralInt    v   -> typ ^ "("   ^ string_of_int   v ^   ")"
  | LiteralFloat  v   -> typ ^ "("   ^ string_of_float v ^   ")"
  | LiteralString v   -> typ ^ "(\"" ^                 v ^ "\")"

  | LiteralArray exps ->
    let exps = List.map (tostring_exp n) exps in
    let exps = String.concat ", " exps in
    typ ^ " [" ^ exps ^ "]"

  | LiteralRecord (def, fields) ->
    let f n (lhs, exp) = match lhs.u with
      | Id def -> (tabs n) ^ def.id ^ " = " ^ (tostring_exp n exp) ^ "\n"
      | _ -> raise ErrPrinter
    in
    let n = n + 1 in
    let fields = List.map (f @@ n + 1) fields in
    let fields = String.concat "" fields in
    let fields = if fields = "" then "" else "\n" ^ fields ^ (tabs n) in
    def.id ^ " {" ^ fields ^ "}"

  | Lhs lhs -> tostring_lhs n lhs

and tostring_lhs n lhs =
  let typ = tostring_typ lhs.typ in
  match lhs.u with
  | Id {id; _} ->
    "ID " ^ id ^ ": " ^ typ
  | Index (arr, idx) ->
    let n = n + 1 in
    let arr = tostring_exp n arr in
    let idx = tostring_exp n idx in
    "INDEXED: " ^ typ ^ "\n" ^
      tabs n ^ "ARRAY => " ^ arr ^ "\n" ^
      tabs n ^ "INDEX => " ^ idx
  | Field (exp, def) ->
    "FIELD " ^ def.id ^ " OF " ^ (tostring_exp n exp)

let () =
  let f (name, input, expected) =
    let output = try
      let lexbuf = Lexing.from_string input in
      let ast1 =Parser.program Scanner.scan lexbuf in
      match Sem.analyse ast1 with
      | Ok ast2 -> tostring_ast2 ast2
      | Error s -> s
      with e -> Printexc.to_string e
    in
    let splitout = String.split_on_char '\n' output in
    let expected = String.split_on_char '\n' expected in
    let expected = List.rev (List.tl (List.rev (List.tl expected))) in
    let f s =
      let leftoffset = 2 * tabsize in
      let buf = Buffer.create (String.length s - leftoffset) in
      String.iteri (fun i c -> if i >= leftoffset then Buffer.add_char buf c) s;
      Buffer.contents buf
    in
    let expected = List.map f expected in

    let rec eq n = function
      | [], [] -> true, 0, "", ""
      | x,  [] -> false, n, String.concat "\n" x, ""
      | [], y  -> false, n, "", String.concat "\n" y
      | x :: xs, y :: ys ->
        if x = y then eq (n + 1) (xs, ys) else false, n, x, y
    in
    let (ok, line, got, expected) = eq 1 (splitout, expected) in
    if ok then None else Some (name, line, expected, got, output)
  in
  let div = String.make 70 '=' ^ "\n" in
  let div_got = String.make 66 '-' ^ " GOT\n" in
  let div_expected = String.make 61 '-' ^ " EXPECTED\n" in
  let div_output = String.make 58 '-' ^ " FULL OUTPUT\n" in
  let showerr (name, line, expected, got, output) =
    printf "<%s> DIFF IN LINE %d\n" name line;
    printf "%s%s\n" div_expected expected;
    printf "%s%s\n" div_got got;
    printf "%s%s\n" div_output output;
    print_string div
  in
  print_string div;
  match List.filter_map f tests with
  | [] -> print_string ("SEM TESTS OK!\n" ^ div)
  | errs ->
    printf "SEM TESTS - %d ERRORS\n%s" (List.length errs) div;
    List.iter showerr errs
