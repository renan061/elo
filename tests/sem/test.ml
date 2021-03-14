
(*
  TODO: TYPES
  TYPE CHECKING
  GLOBAL VARIABLES
  FUNCTION DEFINITIONS
    TODO: traverse paths to see if forgot to insert a return
  RECORD DEFINITIONS
  TODO: BLOCKS & SCOPING
  LOCAL VARIABLES
  STATEMENTS
    ASSIGNMENT
      TODO: COMPOUND ASSIGNMENTS
    RETURN
    IF
    TODO: WHILE
    TODO: FOR
    TODO: BLOCK
  CALLS
  TODO: EXPRESSIONS
  TODO: LHS
*)

(*
  TODO
  - separate tests into different files
  - uninitialized val
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
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Bool != Void", {|
    val a: Bool = nil;
  |}, {|
    error in line 2: mismatching types: expected Bool, got Void
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Bool != Int", {|
    val a: Bool = 1;
  |}, {|
    error in line 2: mismatching types: expected Bool, got Int
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Bool != Float", {|
    val a: Bool = 5.5;
  |}, {|
    error in line 2: mismatching types: expected Bool, got Float
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Bool != String", {|
    val a: Bool = "";
  |}, {|
    error in line 2: mismatching types: expected Bool, got String
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Bool != Array", {|
    val a: Bool = [1];
  |}, {|
    error in line 2: mismatching types: expected Bool, got [Int]
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Bool != Record", {|
    record R {}
    val a: Bool = R{};
  |}, {|
    error in line 3: mismatching types: expected Bool, got R
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Int != Void", {|
    val a: Int = nil;
  |}, {|
    error in line 2: mismatching types: expected Int, got Void
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Int != Bool", {|
    val a: Int = true;
  |}, {|
    error in line 2: mismatching types: expected Int, got Bool
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Int != Float", {|
    val a: Int = 5.5;
  |}, {|
    error in line 2: mismatching types: expected Int, got Float
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Int != String", {|
    val a: Int = "";
  |}, {|
    error in line 2: mismatching types: expected Int, got String
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Int != Array", {|
    val a: Int = [1];
  |}, {|
    error in line 2: mismatching types: expected Int, got [Int]
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Int != Record", {|
    record R {}
    val a: Int = R{};
  |}, {|
    error in line 3: mismatching types: expected Int, got R
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Float != Void", {|
    val a: Float = nil;
  |}, {|
    error in line 2: mismatching types: expected Float, got Void
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Float != Bool", {|
    val a: Float = true;
  |}, {|
    error in line 2: mismatching types: expected Float, got Bool
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Float != Int", {|
    val a: Float = 1;
  |}, {|
    error in line 2: mismatching types: expected Float, got Int
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Float != String", {|
    val a: Float = "";
  |}, {|
    error in line 2: mismatching types: expected Float, got String
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Float != Array", {|
    val a: Float = [1];
  |}, {|
    error in line 2: mismatching types: expected Float, got [Int]
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Float != Record", {|
    record R {}
    val a: Float = R{};
  |}, {|
    error in line 3: mismatching types: expected Float, got R
  (*-------------------------------------------------------------------*) |}); (
  "type checking - String != Void", {|
    val a: String = nil;
  |}, {|
    error in line 2: mismatching types: expected String, got Void
  (*-------------------------------------------------------------------*) |}); (
  "type checking - String != Bool", {|
    val a: String = true;
  |}, {|
    error in line 2: mismatching types: expected String, got Bool
  (*-------------------------------------------------------------------*) |}); (
  "type checking - String != Int", {|
    val a: String = 1;
  |}, {|
    error in line 2: mismatching types: expected String, got Int
  (*-------------------------------------------------------------------*) |}); (
  "type checking - String != Float", {|
    val a: String = 5.5;
  |}, {|
    error in line 2: mismatching types: expected String, got Float
  (*-------------------------------------------------------------------*) |}); (
  "type checking - String != Array", {|
    val a: String = [1];
  |}, {|
    error in line 2: mismatching types: expected String, got [Int]
  (*-------------------------------------------------------------------*) |}); (
  "type checking - String != Record", {|
    record R {}
    val a: String = R{};
  |}, {|
    error in line 3: mismatching types: expected String, got R
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Array != Void", {|
    val a: [Void] = nil;
  |}, {|
    error in line 2: mismatching types: expected [Void], got Void
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Array != Bool", {|
    val a: [Int] = true;
  |}, {|
    error in line 2: mismatching types: expected [Int], got Bool
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Array != Int", {|
    val a: [Float] = 1;
  |}, {|
    error in line 2: mismatching types: expected [Float], got Int
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Array != Float", {|
    val a: [Float] = 5.5;
  |}, {|
    error in line 2: mismatching types: expected [Float], got Float
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Array != String", {|
    val a: [Int] = "string";
  |}, {|
    error in line 2: mismatching types: expected [Int], got String
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Array != Record", {|
    record R {}
    val a: [R] = R{};
  |}, {|
    error in line 3: mismatching types: expected [R], got R
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Record != Void", {|
    record R {}
    val a: R = nil;
  |}, {|
    error in line 3: mismatching types: expected R, got Void
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Record != Bool", {|
    record R {}
    val a: R = true;
  |}, {|
    error in line 3: mismatching types: expected R, got Bool
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Record != Int", {|
    record R {}
    val a: R = 1;
  |}, {|
    error in line 3: mismatching types: expected R, got Int
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Record != Float", {|
    record R {}
    val a: R = 5.5;
  |}, {|
    error in line 3: mismatching types: expected R, got Float
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Record != String", {|
    record R {}
    val a: R = "string";
  |}, {|
    error in line 3: mismatching types: expected R, got String
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Record != Array", {|
    record R {}
    val a: R = [R{}];
  |}, {|
    error in line 3: mismatching types: expected R, got [R]
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Array [A] != Array [B]", {|
    val a: [Int] = [5.5];
  |}, {|
    error in line 2: mismatching types: expected [Int], got [Float]
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Array [[A]] != Array [A]", {|
    val a: [[Int]] = [1, 2];
  |}, {|
    error in line 2: mismatching types: expected [[Int]], got [Int]
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Array [A] != Array [[A]]", {|
    val a: [Int] = [[1], [2]];
  |}, {|
    error in line 2: mismatching types: expected [Int], got [[Int]]
  (*-------------------------------------------------------------------*) |}); (
  "type checking - Record A != Record B", {|
    record X {}
    record Y {}
    val a: X = Y{};
  |}, {|
    error in line 4: mismatching types: expected X, got Y
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
  "global variables - uninitialized (with type)", {|
    val a: Int;
  |}, {|
    Elo.Parser.MenhirBasics.Error
  (*-------------------------------------------------------------------*) |}); (
  "global variables - uninitialized (without type)", {|
    val a;
  |}, {|
    Elo.Parser.MenhirBasics.Error
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
(* FUNCTION DEFINITIONS ---------------------------------------------- *)
(* ------------------------------------------------------------------- *)
  "function definitions - ok", {|
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
  "function definitions - parameter visibility", {|
    function f(a: Int) {
      val b = a;
    }
  |}, {|
    DEF FUNCTION f : VOID
      PARAMETERS (
        DEF VAL a : INT =
          DYNAMIC INT
      )
    {
      DEF VAL b : INT =
        ID a: INT
    }
  (*-------------------------------------------------------------------*) |}); (
  "function definitions - function id scope", {|
    function f {
      val f = 1;
    }
  |}, {|
    DEF FUNCTION f () : VOID {
      DEF VAL f : INT =
        INT(1)
    }
  (*-------------------------------------------------------------------*) |}); (
  "function definitions - parameter scope", {|
    function f(a: Int) {
      val a = 1;
    }
  |}, {|
    error in line 3: redeclaration of variable 'a' from line 2
  (*-------------------------------------------------------------------*) |}); (
  "function definitions - recursion visibility", {|
    function f {
      f();
      val f = f();
    }
  |}, {|
    DEF FUNCTION f () : VOID {
      CALL f ()
      DEF VAL f : VOID =
        CALL f () : VOID
    }
  (*-------------------------------------------------------------------*) |}); (
  "function definitions - without parameters / without return type", {|
    function f {}
  |}, {|
    DEF FUNCTION f () : VOID {}
  (*-------------------------------------------------------------------*) |}); (
  "function definitions - with one parameter / without return type", {|
    function f(a: Bool) {}
  |}, {|
    DEF FUNCTION f : VOID
      PARAMETERS (
        DEF VAL a : BOOL =
          DYNAMIC BOOL
      )
    {}
  (*-------------------------------------------------------------------*) |}); (
  "function definitions - with multiple parameters / without return type", {|
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
  "function definitions - without parameters / with return type", {|
    function f: Bool {}
  |}, {|
    DEF FUNCTION f () : BOOL {}
  (*-------------------------------------------------------------------*) |}); (
  "function definitions - with one parameter / with return type", {|
    function f(a: Bool): Float {}
  |}, {|
    DEF FUNCTION f : FLOAT
      PARAMETERS (
        DEF VAL a : BOOL =
          DYNAMIC BOOL
      )
    {}
  (*-------------------------------------------------------------------*) |}); (
  "function definitions - with multiple parameters / with return type", {|
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
  "function definitions - invalid () for parameters", {|
    function f() {}
  |}, {|
    Elo.Parser.MenhirBasics.Error
  (*-------------------------------------------------------------------*) |}); (
  "function definitions - redeclaration (function x function)", {|
    function f : String {}
    function f(a: Int) {}
  |}, {|
    error in line 3: redeclaration of function 'f' from line 2
  (*-------------------------------------------------------------------*) |}); (
  "function definitions - redeclaration (variable x function)", {|
    val f = 5;
    function f(a: Int) {}
  |}, {|
    error in line 3: redeclaration of variable 'f' from line 2
  (*-------------------------------------------------------------------*) |}); (
  "function definitions - uppercase id", {|
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
(* LOCAL VARIABLES --------------------------------------------------- *)
(* ------------------------------------------------------------------- *)
  "local variables - ok", {|
  function f {
    val a1 = true;
    var b1 = 1;
    val c1 = 2.0;
    var d1 = "3";
    var a2: Bool = false;
    val b2: Int = 4;
    var c2: Float = 5.0;
    val d2: String = "6";
  }
  |}, {|
    DEF FUNCTION f () : VOID {
      DEF VAL a1 : BOOL =
        BOOL(true)
      DEF VAR b1 : INT =
        INT(1)
      DEF VAL c1 : FLOAT =
        FLOAT(2.)
      DEF VAR d1 : STRING =
        STRING("3")
      DEF VAR a2 : BOOL =
        BOOL(false)
      DEF VAL b2 : INT =
        INT(4)
      DEF VAR c2 : FLOAT =
        FLOAT(5.)
      DEF VAL d2 : STRING =
        STRING("6")
    }
  (*-------------------------------------------------------------------*) |}); (
  "local variables - uninitialized (with type)", {|
    function f {
      val a: Int;
    }
  |}, {|
    Elo.Parser.MenhirBasics.Error
  (*-------------------------------------------------------------------*) |}); (
  "local variables - uninitialized (without type)", {|
    function f {
      val a;
    }
  |}, {|
    Elo.Parser.MenhirBasics.Error
  (*-------------------------------------------------------------------*) |}); (
  "local variables - redeclaration (variable x variable)", {|
    function f {
      val x = true;
      var x = false;
    }
  |}, {|
    error in line 4: redeclaration of variable 'x' from line 3
  (*-------------------------------------------------------------------*) |}); (
  "local variables - scoping and shadowing", {|
    val x = 1;
    function f {
      val x = true;
    }
  |}, {|
    DEF VAL x : INT =
      INT(1)
    DEF FUNCTION f () : VOID {
      DEF VAL x : BOOL =
        BOOL(true)
    }
  (*-------------------------------------------------------------------*) |}); (
  "local variables - uppercase id", {|
    function f {
      val A = true;
    }
  |}, {|
    Elo.Parser.MenhirBasics.Error
  (*-------------------------------------------------------------------*) |}); (
  "local variables - mismatching types", {|
    function f {
      var a: [Bool] = true;
    }
  |}, {|
    error in line 3: mismatching types: expected [Bool], got Bool
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
  (*-------------------------------------------------------------------*) |}); (
  "statements - return - no value", {|
    function f {
      return;
    }
  |}, {|
    DEF FUNCTION f () : VOID {
      RETURN VOID(nil)
    }
  (*-------------------------------------------------------------------*) |}); (
  "statements - return - with value", {|
    function f: Int {
      return 1;
    }
  |}, {|
    DEF FUNCTION f () : INT {
      RETURN INT(1)
    }
  (*-------------------------------------------------------------------*) |}); (
  "statements - return - empty when expecting value", {|
    function f: Int {
      return;
    }
  |}, {|
    error in line 3: mismatching types: expected Int, got Void
  (*-------------------------------------------------------------------*) |}); (
  "statements - return - with value when expecting Void", {|
    function f {
      return 1;
    }
  |}, {|
    error in line 3: mismatching types: expected Void, got Int
  (*-------------------------------------------------------------------*) |}); (
  "statements - return - value with the wrong type", {|
    function f: Int {
      return true;
    }
  |}, {|
    error in line 3: mismatching types: expected Int, got Bool
  (*-------------------------------------------------------------------*) |}); (
  "statements - if - (if)", {|
    function main {
      if true {
        val a = 1;
      }
    }
  |}, {|
    DEF FUNCTION main () : VOID {
      IF BOOL(true) {
        DEF VAL a : INT =
          INT(1)
      }
    }
  (*-------------------------------------------------------------------*) |}); (
  "statements - if - (if-else)", {|
    function main {
      if false {
        var b = 1.0;
        b = 1.1;
      } else {
        var c = true;
        c = false;
      }
    }
  |}, {|
    DEF FUNCTION main () : VOID {
      IF BOOL(false) {
        DEF VAR b : FLOAT =
          FLOAT(1.)
        ASG ID b: FLOAT =
          FLOAT(1.1)
      } ELSE {
        DEF VAR c : BOOL =
          BOOL(true)
        ASG ID c: BOOL =
          BOOL(false)
      }
    }
  (*-------------------------------------------------------------------*) |}); (
  "statements - if - (if-elseif)", {|
    function main {
      if false {
      } elseif true {
      }
    }
  |}, {|
    DEF FUNCTION main () : VOID {
      IF BOOL(false) {} ELSE {
        IF BOOL(true) {}
      }
    }
  (*-------------------------------------------------------------------*) |}); (
  "statements - if - (if-elseifs)", {|
    function main {
      if false {
      } elseif true {
      } elseif false {
      } elseif true {
      }
    }
  |}, {|
    DEF FUNCTION main () : VOID {
      IF BOOL(false) {} ELSE {
        IF BOOL(true) {} ELSE {
          IF BOOL(false) {} ELSE {
            IF BOOL(true) {}
          }
        }
      }
    }
  (*-------------------------------------------------------------------*) |}); (
  "statements - if - (if-elseif-else)", {|
    function main {
      var a: Int;
      if true {
        a = 1;
      } elseif false {
        a = 2;
      } else {
        a = 3;
      }
    }
  |}, {|
    DEF FUNCTION main () : VOID {
      DEF VAR a : INT =
        ZEROVALUE INT
      IF BOOL(true) {
        ASG ID a: INT =
          INT(1)
      } ELSE {
        IF BOOL(false) {
          ASG ID a: INT =
            INT(2)
        } ELSE {
          ASG ID a: INT =
            INT(3)
        }
      }
    }
  (*-------------------------------------------------------------------*) |}); (
  "statements - if - if inside an if", {|
    function main {
      if false {
        if true {
        }
      }
    }
  |}, {|
    DEF FUNCTION main () : VOID {
      IF BOOL(false) {
        IF BOOL(true) {}
      }
    }
  (*-------------------------------------------------------------------*) |}); (
  "statements - if - invalid (if) condition type", {|
    function main {
      if 5.5 {}
    }
  |}, {|
    error in line 3: mismatching types: expected Bool, got Float
  (*-------------------------------------------------------------------*) |}); (
  "statements - if - invalid (elseif) condition type", {|
    function main {
      if true {}
      elseif "true" {}
    }
  |}, {|
    error in line 4: mismatching types: expected Bool, got String
(* ------------------------------------------------------------------- *) |}); (
(* CALLS ------------------------------------------------------------- *)
(* ------------------------------------------------------------------- *)
  "calls - ok", {|
    function f0 {}
    function f1(a: String) {}
    function f2(a: Int, b: Float) {}
    function fN(a: Bool, b: [Float], third: Int, xpto: Int) {}
    function main {
      f0();
      f1("string");
      f2(2, 3.14);
      fN(true, [3.0, 0.1, 0.04], 555, 777);
    }
  |}, {|
    DEF FUNCTION f0 () : VOID {}
    DEF FUNCTION f1 : VOID
      PARAMETERS (
        DEF VAL a : STRING =
          DYNAMIC STRING
      )
    {}
    DEF FUNCTION f2 : VOID
      PARAMETERS (
        DEF VAL a : INT =
          DYNAMIC INT,
        DEF VAL b : FLOAT =
          DYNAMIC FLOAT
      )
    {}
    DEF FUNCTION fN : VOID
      PARAMETERS (
        DEF VAL a : BOOL =
          DYNAMIC BOOL,
        DEF VAL b : [FLOAT] =
          DYNAMIC [FLOAT],
        DEF VAL third : INT =
          DYNAMIC INT,
        DEF VAL xpto : INT =
          DYNAMIC INT
      )
    {}
    DEF FUNCTION main () : VOID {
      CALL f0 ()
      CALL f1 (
        STRING("string")
      )
      CALL f2 (
        INT(2),
        FLOAT(3.14)
      )
      CALL fN (
        BOOL(true),
        [FLOAT] [FLOAT(3.), FLOAT(0.1), FLOAT(0.04)],
        INT(555),
        INT(777)
      )
    }
  (*-------------------------------------------------------------------*) |}); (
  "call - undefined function", {|
    function main {
      f();
    }
  |}, {|
    error in line 3: undefined function 'f'
  (*-------------------------------------------------------------------*) |}); (
  "call - expected a function", {|
    function main {
      val f = 1;
      f();
    }
  |}, {|
    error in line 4: expected a function
  (*-------------------------------------------------------------------*) |}); (
  "call - more arguments than parameters (1)", {|
    function f { /* empty*/ }
    function main {
      f(1);
    }
  |}, {|
    error in line 4: too many arguments in call to 'f'
  (*-------------------------------------------------------------------*) |}); (
  "call - more arguments than parameters (2)", {|
    function f(a: Int, b: Int) { /* empty*/ }
    function main {
      f(1, 2, 3);
    }
  |}, {|
    error in line 4: too many arguments in call to 'f'
  (*-------------------------------------------------------------------*) |}); (
  "call - more parameters than arguments (1)", {|
    function f(a: Int, b: Int) { /* empty*/ }
    function main {
      f(1);
    }
  |}, {|
    error in line 4: too few arguments in call to 'f'
  (*-------------------------------------------------------------------*) |}); (
  "call - more parameters than arguments (2)", {|
    function f(a: Int) { /* empty*/ }
    function main {
      f();
    }
  |}, {|
    error in line 4: too few arguments in call to 'f'
  (*-------------------------------------------------------------------*) |}); (
  "call - mismatching argument vs. parameter types (1)", {|
    function f(a: Int) { /* empty*/ }
    function main {
      f(1.0);
    }
  |}, {|
    error in line 4: mismatching types: expected Int, got Float
  (*-------------------------------------------------------------------*) |}); (
  "call - mismatching argument vs. parameter types (2)", {|
  function f(a: Int, b: Float, c: Bool) { /* empty*/ }
    function main {
      f(1, true, 2.0);
    }
  |}, {|
    error in line 4: mismatching types: expected Float, got Bool
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
  | Fun (params, typ, block) ->
    let typ = tostring_typ typ in
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
  let (vartype, typ, exp) = match def.u with
    | Val (typ, exp) -> ("VAL", typ, exp)
    | Var (typ, exp) -> ("VAR", typ, exp)
    | _ -> raise ErrPrinter
  in
  let typ = tostring_typ typ in
  let exp = tostring_exp n exp in
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
    "{\n" ^ String.concat "\n" (List.map g block) ^ "\n" ^ (tabs (n - 1)) ^ "}"

and tostring_typ = function
  | Void      -> "VOID"
  | Bool      -> "BOOL"
  | Int       -> "INT"
  | Float     -> "FLOAT"
  | String    -> "STRING"
  | Array typ -> "[" ^ tostring_typ typ ^ "]"
  | Record def -> "RECORD " ^ def.id

and tostring_stmt n (stmt: stmt) = match stmt.u with
  | Asg (lhs, exp) ->
    let lhs = tostring_lhs n lhs in
    let n = n + 1 in
    let exp = tostring_exp n exp in
    let exp = "\n" ^ (tabs n) ^ exp in
    String.concat " " ["ASG"; lhs; "="] ^ exp
  | Call call -> tostring_call n call
  | Ret exp ->
    let exp = tostring_exp n exp in
    "RETURN " ^ exp
  | If (exp, block, else_) ->
    let exp = tostring_exp n exp in
    let n = n + 1 in
    let block = tostring_block n block in
    let if_ = "IF " ^ exp ^ " " ^ block in
    begin match else_ with
    | None -> if_
    | Some block ->
      let block = tostring_block n block in
      if_ ^ " ELSE " ^ block
    end

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

  | LiteralRecord fields ->
    let f n ((lhs: lhs), exp) = match lhs.u with
      | Id def -> (tabs n) ^ def.id ^ " = " ^ (tostring_exp n exp) ^ "\n"
      | _ -> raise ErrPrinter
    in
    let n = n + 1 in
    let fields = List.map (f @@ n + 1) fields in
    let fields = String.concat "" fields in
    let fields = if fields = "" then "" else "\n" ^ fields ^ (tabs n) in
    begin match exp.typ with
    | Record def -> def.id ^ " {" ^ fields ^ "}"
    | _ -> raise ErrPrinter
    end

  | Lhs lhs -> tostring_lhs n lhs
  | Call call -> tostring_call n call ^ " : " ^ (tostring_typ call.typ)

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

and tostring_call n call = match call.u with
  | Fun (fun_def, args) ->
    let args = List.map (tostring_exp n) args in
    let args = String.concat (",\n" ^ tabs (n + 1)) args in
    let args = if args = ""
      then args
      else "\n" ^ (tabs (n + 1)) ^ args ^ "\n" ^ (tabs n) in
    "CALL " ^ fun_def.id ^ " (" ^ args ^ ")"

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
    printf "SEM TESTS - %d ERROR(S)\n%s" (List.length errs) div;
    List.iter showerr errs
