
let tests = [
    (
        "line comments",
        {|
            // some comments id1
            id2 // look another id3 // /* comment? */
            // more comments
            //
            end
        |},
        [
            "LID(id2)"; "LID(end)"
        ]
    ); (
        "multiline comments",
        {|
            /**/ /******/
            /* something */ a
            /*
                line one
                // single line commentary
                line two
            */
            /*and*/ b/**/
            C
            /*something123"string inside commentary"more*/
            /* can /* nest /* comments */*/*/
            end
        |},
        [
            "LID(a)"; "LID(b)"; "UID(C)"; "LID(end)"
        ]
    ); (
        "multiline comment (EOF)",
        {|
            things morethings
            /*
            end
        |},
        [
            "LID(things)"; "LID(morethings)"; "InvalidCommentEOF"
        ]
    ); (
        "multiline comment (nested)",
        {|
            /*
                some commentary
                /* ops...
            */
        |},
        [
            "InvalidCommentEOF"
        ]
    ); (
        "characters",
        {|
            ( ) [ ] { }
            = /* TODO .*/ , : ;
            /* TODO < > + - * / */
        |},
        [
            "("; ")"; "["; "]"; "{"; "}";
            "="; (*"."; *)","; ":"; ";";
            (*"<"; ">"; "+"; "-"; "*"; "/";*)
        ]
    ); (
        "special tokens",
        {|/* TODO
            == !=
            <- -> <= =>
            += -= *= /=
        */|},
        [(*
            "=="; "!=";
            "<-"; "->"; "<="; "=>";
            "+="; "-="; "*="; "/=";
        *)]
    ); (
        "keywords",
        {|
            /* TODO not and or */
            var val
            function
            true false
        |},
        [
            (* "NOT"; "AND"; "OR"; *)
            "VAR"; "VAL";
            "FUNCTION";
            "TRUE"; "FALSE";
        ]
    ); (
        "numbers",
        {|
            00000000000
            0 01 001 0001
            1 10 100 1000
            1 12 123 1234
            86788 100000000
            10101010 020201039 87263514 123974
            0000000000000000000000000000000000000000000000000000000000000000000

            0.0 0.00 00.0 00.00
            1.0 0.1 0.0001
            3.1415 123.456
            01020.4802 0000740.0 1928.126
        |},
        [
            "INT(0)";
            "INT(0)"; "INT(1)"; "INT(1)"; "INT(1)";
            "INT(1)"; "INT(10)"; "INT(100)"; "INT(1000)";
            "INT(1)"; "INT(12)"; "INT(123)"; "INT(1234)";
            "INT(86788)"; "INT(100000000)";
            "INT(10101010)"; "INT(20201039)"; "INT(87263514)"; "INT(123974)";
            "INT(0)";

            "FLOAT(0.)"; "FLOAT(0.)"; "FLOAT(0.)"; "FLOAT(0.)";
            "FLOAT(1.)"; "FLOAT(0.1)"; "FLOAT(0.0001)";
            "FLOAT(3.1415)"; "FLOAT(123.456)";
            "FLOAT(1020.4802)"; "FLOAT(740.)"; "FLOAT(1928.126)";
        ]
    ); (
        "mixing numbers with other tokens",
        {|

            029124905d1233
            105]1233
            a94812)746
            0. .1 .2.
            3..4 5.6. .7.8
            123.80e0
            01020.480201.61201e2
        |},
        [
            "INT(29124905)"; "LID(d1233)";
            "INT(105)"; "]"; "INT(1233)";
            "LID(a94812)"; ")"; "INT(746)";
            "INT(0)"; "."; "."; "INT(1)"; "."; "INT(2)"; ".";
            "INT(3)"; "."; "."; "INT(4)"; "FLOAT(5.6)"; "."; "."; "FLOAT(7.8)";
            "FLOAT(123.8)"; "LID(e0)";
            "FLOAT(1020.480201)"; "."; "INT(61201)"; "LID(e2)";
        ]
    ); (
        "strings",
        {|
            "a string" "another string"
            "a string with \t two \t tabs"
            "a string with \n newline"
            "astring with\tweird\nthings and another""string\"next to it"
        |},
        [
            "STRING(a string)"; "STRING(another string)";
            "STRING(a string with \\t two \\t tabs)";
            "STRING(a string with \\n newline)";
            "STRING(astring with\\tweird\\nthings and another)";
            "STRING(string\\\"next to it)"
        ]
    ); (
        "invalid escape in string",
        {| "a string with \x is invalid" |},
        ["InvalidStringEscape(\\x)"]
    ); (
        "invalid multiline string",
        "\"strings cannot \n break lines\"",
        ["InvalidStringNewline"]
    ); (
        "invalid EOF string",
        {| "must close the string |},
        ["InvalidStringEOF"]
    ); (
        "lowercase ids",
        {|
            id idCamelCase id_underscore
            idWithDigits000 id_with_1_digit_and_underscores
            m1_x_3d
        |},
        [
            "LID(id)";
            "LID(idCamelCase)";
            "LID(id_underscore)";
            "LID(idWithDigits000)";
            "LID(id_with_1_digit_and_underscores)";
            "LID(m1_x_3d)";
        ]
    ); (
        "uppercase ids",
        {|
            Id IdCamelCase Id_underscore
            IdWithDigits000 Id_with_1_digit_and_underscores
            M1_x_3d
        |},
        [
            "UID(Id)";
            "UID(IdCamelCase)";
            "UID(Id_underscore)";
            "UID(IdWithDigits000)";
            "UID(Id_with_1_digit_and_underscores)";
            "UID(M1_x_3d)";
        ]
    ); (
        "mixing ids with other tokens",
        {|
            A98hwedsfs Nasdk123 Zs9d801 a_12397asda__ n123has_Sx z123_sada_asD_
            as______ ___ _ AzN18_ nA0_z _a_Z09 Az123_as!asd12_@3fl 7_aojh
            #sodf ]
        |},
        [
            "UID(A98hwedsfs)"; "UID(Nasdk123)"; "UID(Zs9d801)";
            "LID(a_12397asda__)"; "LID(n123has_Sx)";
            "LID(z123_sada_asD_)"; "LID(as______)";
            "INVALID(_)"; "INVALID(_)"; "INVALID(_)"; "INVALID(_)";
            "UID(AzN18_)"; "LID(nA0_z)";
            "INVALID(_)";
            "LID(a_Z09)"; "UID(Az123_as)";
            "INVALID(!)";
            "LID(asd12_)";
            "INVALID(@)";
            "INT(3)"; "LID(fl)"; "INT(7)";
            "INVALID(_)";
            "LID(aojh)";
            "INVALID(#)";
            "LID(sodf)"; "]";
        ]
    );
]

(* -------------------------------------------------------------------------- *)

open Printf
open Elo

let step lexbuf = match Scanner.scan lexbuf with
    | LPAR     _ -> "("
    | RPAR     _ -> ")"
    | LBRACKET _ -> "["
    | RBRACKET _ -> "]"
    | LBRACE   _ -> "{"
    | RBRACE   _ -> "}"

    | ASG       _ -> "="
    | DOT       _ -> "."
    | COMMA     _ -> ","
    | COLON     _ -> ":"
    | SEMICOLON _ -> ";"
(*
    | LT    _ -> "<"
    | GT    _ -> ">"
    | PLUS  _ -> "+"
    | MINUS _ -> "-"
    | MUL   _ -> "*"
    | DIV   _ -> "/"

    | EQUALS  _ -> "=="
    | NEQUALS _ -> "!="

    | LARROW  _ -> "<-"
    | RARROW  _ -> "->"
    | LEARROW _ -> "<="
    | REARROW _ -> "=>"

    | ASGADD _ -> "+="
    | ASGMIN _ -> "-="
    | ASGMUL _ -> "*="
    | ASGDIV _ -> "/="

    | NOT _ -> "NOT"
    | AND _ -> "AND"
    | OR  _ -> "OR"
*)
    | VAR      _ -> "VAR"
    | VAL      _ -> "VAL"
    | FUNCTION _ -> "FUNCTION"

    | TRUE  _       -> "TRUE"
    | FALSE _       -> "FALSE"
    | INT    (_, v) -> sprintf "INT(%s)" (string_of_int v)
    | FLOAT  (_, v) -> sprintf "FLOAT(%s)" (string_of_float v)
    | STRING (_, v) -> sprintf "STRING(%s)" (String.escaped v)

    | LID (_, id) -> sprintf "LID(%s)" id
    | UID (_, id) -> sprintf "UID(%s)" id

    | EOF -> "\n"

    | INVALID (_, c) -> sprintf "INVALID(%c)" c

let test (name, input, expected) =
    let rec recursive_step lexbuf = try match step lexbuf with
        | "\n" -> []
        | _ as tk -> tk :: recursive_step lexbuf
        with
        | Scanner.InvalidCommentEOF     -> ["InvalidCommentEOF"]
        | Scanner.InvalidStringEscape s -> [sprintf "InvalidStringEscape(%s)" s]
        | Scanner.InvalidStringNewline  -> ["InvalidStringNewline"]
        | Scanner.InvalidStringEOF      -> ["InvalidStringEOF"]
    in
    let rec equal output expected = match output, expected with
        | [], [] -> Ok ()
        | x :: _, [] -> Error (sprintf "output > expected at <%s>" x)
        | [], y :: _ -> Error (sprintf "expected > output at <%s>" y)
        | x :: xs, y :: ys -> if compare x y != 0
                                then Error (sprintf "%s != %s" x y)
                                else equal xs ys
    in
    let lexbuf = Lexing.from_string input in
    let output = recursive_step lexbuf in
    match equal output expected with
    | Ok _ -> printf "Scanner ok <%s>\n" name
    | Error s -> printf "Scanner error <%s> => %s\n" name s

let () = List.iter test tests
