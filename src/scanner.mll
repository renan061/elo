{
    open Parser

    exception InvalidCommentEOF

    exception InvalidStringEscape of string
    exception InvalidStringNewline
    exception InvalidStringEOF

    let p (lexbuf: Lexing.lexbuf) = lexbuf.lex_curr_p
}

(* -------------------------------------------------------------------------- *)

let blank = [' ' '\t']
let newline = '\n'

let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let any   = '_' | lower | upper | digit

let ival = digit+
let fval = digit+ "." digit+

let lid = lower any*
let uid = upper any*

(* -------------------------------------------------------------------------- *)

rule scan = parse
    | blank { scan lexbuf }

    | newline { Lexing.new_line lexbuf; scan lexbuf }

    | "//" [^ '\n']* { scan lexbuf }

    | "/*" { multiline_comment 0 lexbuf }

    | '(' { LPAR     (p lexbuf) }
    | ')' { RPAR     (p lexbuf) }
    | '[' { LBRACKET (p lexbuf) }
    | ']' { RBRACKET (p lexbuf) }
    | '{' { LBRACE   (p lexbuf) }
    | '}' { RBRACE   (p lexbuf) }

    | '=' { ASG       (p lexbuf) }
    | '.' { DOT       (p lexbuf) }
    | ',' { COMMA     (p lexbuf) }
    | ':' { COLON     (p lexbuf) }
    | ';' { SEMICOLON (p lexbuf) }

    | '<' { LT    (p lexbuf) }
    | '>' { GT    (p lexbuf) }
    | '+' { PLUS  (p lexbuf) }
    | '-' { MINUS (p lexbuf) }
    | '*' { MUL   (p lexbuf) }
    | '/' { DIV   (p lexbuf) }

    | "==" { EQUALS  (p lexbuf) }
    | "!=" { NEQUALS (p lexbuf) }

    | "<-" { LARROW  (p lexbuf) }
    | "->" { RARROW  (p lexbuf) }
    | "<=" { LEARROW (p lexbuf) }
    | "=>" { REARROW (p lexbuf) }

    | "+=" { ASGADD (p lexbuf) }
    | "-=" { ASGMIN (p lexbuf) }
    | "*=" { ASGMUL (p lexbuf) }
    | "/=" { ASGDIV (p lexbuf) }

    | "not" { NOT (p lexbuf) }
    | "and" { AND (p lexbuf) }
    | "or"  { OR  (p lexbuf) }

    | "var"      { VAR      (p lexbuf) }
    | "val"      { VAL      (p lexbuf) }
    | "function" { FUNCTION (p lexbuf) }

    | "true"    { TRUE  (p lexbuf)                    }
    | "false"   { FALSE (p lexbuf)                    }
    | ival as v { INT   (p lexbuf, int_of_string   v) }
    | fval as v { FLOAT (p lexbuf, float_of_string v) }

    | '"' { STRING (p lexbuf, string_literal (Buffer.create 16) lexbuf) }

    | lid as id { LID (p lexbuf, id) }
    | uid as id { UID (p lexbuf, id) }

    | eof { EOF }

    | _ as c { INVALID (p lexbuf, c) }

and multiline_comment n = parse
    | "/*" { multiline_comment (n + 1) lexbuf }

    | "*/" { if n == 0 then scan lexbuf else multiline_comment (n - 1) lexbuf }

    | newline { Lexing.new_line lexbuf; multiline_comment n lexbuf }

    | eof  { raise InvalidCommentEOF }

    | _ { multiline_comment n lexbuf }

and string_literal buf = parse
    | '"' { Buffer.contents buf }

    | '\\' '"'    { Buffer.add_char buf '"' ; string_literal buf lexbuf }
    | '\\' 't'    { Buffer.add_char buf '\t'; string_literal buf lexbuf }
    | '\\' 'n'    { Buffer.add_char buf '\n'; string_literal buf lexbuf }
    | '\\' '\\'   { Buffer.add_char buf '\\'; string_literal buf lexbuf }
    | '\\' _ as s { raise (InvalidStringEscape s) }

    | '\n' { raise InvalidStringNewline }
    | eof  { raise InvalidStringEOF     }

    | _ as c { Buffer.add_char buf c; string_literal buf lexbuf }

(* -------------------------------------------------------------------------- *)

{
    let setup file = let ic = (open_in file) in (Lexing.from_channel ic, ic)
        
    let teardown = close_in
}
