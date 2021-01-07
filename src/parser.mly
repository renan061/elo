%{
  open Ast1
%}

%token <Lexing.position> LPAR "(" RPAR ")"
%token <Lexing.position> LBRACKET "[" RBRACKET "]"
%token <Lexing.position> LBRACE "{" RBRACE "}"
%token <Lexing.position> ASG "=" DOT "." COMMA "," COLON ":" SEMICOLON ";"
%token <Lexing.position> OR "or" AND "and" NOT "not"
%token <Lexing.position> EQUALS "==" NEQUALS "!="
%token <Lexing.position> LT "<" GT ">" LTEQ "<=" GTEQ ">="
%token <Lexing.position> PLUS "+" MINUS "-" MUL "*" DIV "/"
/*
%token <Lexing.position> ASGADD "+=" ASGMIN "-=" ASGMUL "*=" ASGDIV "/="
*/
%token <Lexing.position> IF "if" ELSEIF "elseif" ELSE "else"
%token <Lexing.position> RETURN "return"
%token <Lexing.position> WHILE "while" FOR "for"

%token <Lexing.position> VAL "val" VAR "var"
%token <Lexing.position> FUNCTION "function"

%token <Lexing.position * string> LID UID

%token <Lexing.position> TRUE FALSE
%token <Lexing.position * int> INT
%token <Lexing.position * float> FLOAT
%token <Lexing.position * string> STRING

%token EOF
%token <Lexing.position * char> INVALID

/* operator precedence and associativity */
%left     OR
%left     AND
%nonassoc EQUALS NEQUALS
%nonassoc LT GT LTEQ GTEQ
%left     PLUS MINUS
%left     MUL DIV
%left     NOT /* precedence for unary minus */

%start <Ast1.ast> program

%%

program : def* EOF { $1 }

(* -------------------------------------------------------------------------- *)

def : variable_def { $1 }
    | function_def { $1 }

variable_def : val_def { $1 } | var_def { $1 }

val_def : "val" LID typedec?          "="  exp   ";" { Val ($1, $2, $3, $5) }
var_def : "var" LID typedec? preceded("=", exp)? ";" { Var ($1, $2, $3, $4) }

function_def : "function" LID params typedec? block { Fun ($1, $2, $3, $4, $5) }

block : "{" block_elem* "}" { $2 }

block_elem : variable_def { V $1 }
           | stmt         { S $1 }

typ : UID         { Id    $1 }
    | "[" typ "]" { Array $2 }

(* -------------------------------------------------------------------------- *)

stmt : simple_stmt ";" { $1 }
     | compound_stmt   { $1 }

simple_stmt : assignment    { $1        }
            | call          { Call   $1 }
            | "return" exp? { Return $2 }

compound_stmt : "if" exp block elseif* else_? { If    ($2, $3, $4, $5) }
              | "while" exp block             { While ($2, $3)         }
              (* for *)
              | block                         { Block $1               }

assignment : lhs "=" exp { Asg ($1, $3) }
             (* += -= *= /= *)

(* auxiliary to "if" in compound_stmt *)
elseif : "elseif" exp block { ($2, $3) }
else_  : "else"       block { $2       }

(* -------------------------------------------------------------------------- *)

exp : exp binop exp     { Binary  ($1, $2, $3) }
    | "-" exp %prec NOT { Unary   (Minus, $2)  }
    | "not" exp         { Unary   (Not,   $2)  }
    | primitive_literal { Literal $1           }
    | lhs               { Lhs     $1           }
    | call              { Call    $1           }
    | "(" exp ")"       { $2                   }
%inline binop : | "or"  { Or                   }
                | "and" { And                  }
                | "=="  { Equal                }
                | "!="  { NotEqual             }
                | "<="  { LessThanOrEqual      }
                | ">="  { GreaterThanOrEqual   }
                | "<"   { LessThan             }
                | ">"   { GreaterThan          }
                | "+"   { Plus                 }
                | "-"   { Minus                }
                | "*"   { Mul                  }
                | "/"   { Div                  }

primitive_literal : TRUE   { True   $1                        }
                  | FALSE  { False  $1                        }
                  | INT    { let (p, v) = $1 in Int    (p, v) }
                  | FLOAT  { let (p, v) = $1 in Float  (p, v) }
                  | STRING { let (p, v) = $1 in String (p, v) }

lhs : LID { Id $1 }

call : LID "(" exps ")" { Function ($1, $3) }

(* -------------------------------------------------------------------------- *)

params : (* empty *)                                 { [] }
       | "(" separated_nonempty_list(",", param) ")" { $2 }

param : LID typedec { Val (fst $1, $1, Some $2, Dynamic $2) }

exps : "(" separated_list(",", exp) ")" { $2 }

(* type declaration *)
typedec : ":" typ { $2 }

%%
