%{
    open Ast1
%}

%token <Lexing.position> LPAR "(" RPAR ")"
%token <Lexing.position> LBRACKET "[" RBRACKET "]"
%token <Lexing.position> LBRACE "{" RBRACE "}"
%token <Lexing.position> ASG "=" DOT "." COMMA "," COLON ":" SEMICOLON ";"
%token <Lexing.position> LT "<" GT ">" PLUS "+" MINUS "-" MUL "*" DIV "/"
%token <Lexing.position> EQUALS "==" NEQUALS "!="
%token <Lexing.position> LARROW "<-" RARROW "->" LEARROW "<=" REARROW "=>"
%token <Lexing.position> ASGADD "+=" ASGMIN "-=" ASGMUL "*=" ASGDIV "/="

%token <Lexing.position> NOT AND OR
%token <Lexing.position> VAL VAR
%token <Lexing.position> FUNCTION

%token <Lexing.position * string> LID UID

%token <Lexing.position> TRUE FALSE
%token <Lexing.position * int> INT
%token <Lexing.position * float> FLOAT
%token <Lexing.position * string> STRING

%token EOF
%token <Lexing.position * char> INVALID

%start <Ast1.ast> program

%%

program : def* EOF { $1 }

/* -------------------------------------------------------------------------- */

def : variable_def { $1 }
    | function_def { $1 }

variable_def : val_def { $1 } | var_def { $1 }

val_def : VAL LID sctype? "=" exp  ";"            { Val ($1, $2, $3, $5) }
var_def : VAR LID sctype? preceded("=", exp)? ";" { Var ($1, $2, $3, $4) }

function_def : FUNCTION LID params sctype? block { Fun ($1, $2, $3, $4, $5) }

block : "{" block_elem* "}" { $2 }

block_elem : variable_def { V $1 }
           | stmt         { S $1 }

/* -------------------------------------------------------------------------- */

typ : UID         { Id $1    }
    | "[" typ "]" { Array $2 }

stmt : lhs "=" exp ";" { Asg ($1, $3) }

exp : primitive_literal { Literal $1 }
    | lhs               { Lhs $1     }

primitive_literal : TRUE   { True   $1                        }
                  | FALSE  { False  $1                        }
                  | INT    { let (p, v) = $1 in Int    (p, v) }
                  | FLOAT  { let (p, v) = $1 in Float  (p, v) }
                  | STRING { let (p, v) = $1 in String (p, v) }

lhs : LID             { Id $1                }
    | exp "[" exp "]" { Indexed ($2, $1, $3) }

/* -------------------------------------------------------------------------- */

params : /* empty */                                 { [] }
       | "(" separated_nonempty_list(",", param) ")" { $2 }

param : LID sctype { Val (fst $1, $1, Some $2, Dynamic $2) }

sctype : ":" typ { $2 }

%%
