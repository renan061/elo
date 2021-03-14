%{
  open Ast1
  exception ErrCompiler of string
%}

%token <Lexing.position> LPAR "(" RPAR ")"
%token <Lexing.position> LBRACKET "[" RBRACKET "]"
%token <Lexing.position> LBRACE "{" RBRACE "}"
%token <Lexing.position> ASG "=" DOT "." COMMA "," COLON ":" SEMICOLON ";"
%token <Lexing.position> OR "or" AND "and" NOT "not"
%token <Lexing.position> EQ "==" NEQ "!="
%token <Lexing.position> LARROW "<-" RARROW "->"
%token <Lexing.position> LT "<" GT ">" LTEQ "<=" EQGT "=>" GTEQ ">="
%token <Lexing.position> ADD "+" MIN "-" MUL "*" DIV "/"
%token <Lexing.position> ASGADD "+=" ASGMIN "-=" ASGMUL "*=" ASGDIV "/="
%token <Lexing.position> RETURN "return"
%token <Lexing.position> IF "if" ELSEIF "elseif" ELSE "else"
%token <Lexing.position> WHILE "while" FOR "for"

%token <Lexing.position> VAL "val" VAR "var"
%token <Lexing.position> FUN "function"
%token <Lexing.position> REC "record"

%token <Lexing.position * string> LID UID

%token <Lexing.position> NIL
%token <Lexing.position> TRUE FALSE
%token <Lexing.position * int> INT
%token <Lexing.position * float> FLOAT
%token <Lexing.position * string> STRING

%token EOF
%token <Lexing.position * char> INVALID

/* operator precedence and associativity */
%left     OR
%left     AND
%nonassoc EQ NEQ
%nonassoc LT GT LTEQ GTEQ
%left     ADD MIN
%left     MUL DIV
%left     NOT /* precedence for unary minus */

%start <Ast1.ast> program

%%

program : def* EOF { $1 }

(* -------------------------------------------------------------------------- *)

def : variable_def { $1 }
    | function_def { $1 }
    | record_def   { $1 }

variable_def : val_def { $1 } | var_def { $1 }

val_def : "val" LID typedec?          "="  exp   ";" { Val ($1, $2, $3, $5) }
var_def : "var" LID typedec? preceded("=", exp)? ";" { Var ($1, $2, $3, $4) }

function_def : "function" LID params typedec? block { Fun ($1, $2, $3, $4, $5) }

block : "{" block_elem* "}" { $2 }

block_elem : variable_def { V $1 }
           | stmt         { S $1 }

typ : UID         { Id    $1 }
    | "[" typ "]" { Array $2 }

record_def : "record" UID "{" variable_def* "}" { Rec ($1, $2, $4) }

(* -------------------------------------------------------------------------- *)

stmt : simple_stmt ";" { $1 }
     | compound_stmt   { $1 }

simple_stmt : assignment    { $1              }
            | call          { Call   $1       }
            | "return" exp? { Return ($1, $2) }

compound_stmt : "if" exp block elseif* else_? { If    ($1, $2, $3, $4, $5) }
              | "while" exp block             { While ($1, $2, $3)         }
              | "for" LID "=" range block     { For   ($2, $4, $5)         }
              | block                         { Block $1                   }

assignment : lhs op exp { Asg (fst $2, $1, snd $2, $3) }
%inline op : "="        { ($1, AsgSimple)              }
           | "+="       { ($1, AsgAdd)                 }
           | "-="       { ($1, AsgMin)                 }
           | "*="       { ($1, AsgMul)                 }
           | "/="       { ($1, AsgDiv)                 }

(* auxiliary to "if" in compound_stmt *)
elseif : "elseif" exp block { ($1, $2, $3) }
else_  : "else"       block { $2           }

range : exp arrow exp { Range ($1, $2, $3) }
%inline arrow : "->"  { RightExcluding     }
              | "<-"  { LeftExcluding      }
              | "=>"  { RightIncluding     }
              | "<="  { LeftIncluding      }

(* -------------------------------------------------------------------------- *)

exp : exp binop exp       { Binary ($1, $2, $3) }
    | "not" exp           { Unary  (Not, $2)    }
    | "-"   exp %prec NOT { Unary  (Min, $2)    }
    | simple_exp          { $1                  }
%inline binop : "or"      { Or                  }
              | "and"     { And                 }
              | "=="      { Equal               }
              | "!="      { NotEqual            }
              | "<="      { LessThanOrEqual     }
              | ">="      { GreaterThanOrEqual  }
              | "<"       { LessThan            }
              | ">"       { GreaterThan         }
              | "+"       { Add                 }
              | "-"       { Min                 }
              | "*"       { Mul                 }
              | "/"       { Div                 }

simple_exp : primitive_literal { Literal $1 }
           | array_literal     { Literal $1 }
           | record_literal    { Literal $1 }
           | lhs               { Lhs     $1 }
           | call              { Call    $1 }
           | "(" exp ")"       { $2         }

primitive_literal : NIL    { Nil   $1                         }
                  | TRUE   { True  $1                         }
                  | FALSE  { False $1                         }
                  | INT    { let (p, v) = $1 in Int    (p, v) }
                  | FLOAT  { let (p, v) = $1 in Float  (p, v) }
                  | STRING { let (p, v) = $1 in String (p, v) }

array_literal : "[" separated_nonempty_list(",", exp) "]" { ArrayL ($1, $2) }

record_literal : UID "{" separated_list(",", recasg) "}" { RecordL ($1, $3) }

lhs : LID                { Id $1 }
    | simple_exp "." LID { Field ($2, $1, $3) }

call : LID args                { Function    ($1, $2)     }
     | simple_exp "." LID args { Method      ($1, $3, $4) }
     | UID args                { Constructor (Id $1, $2)  }

(* -------------------------------------------------------------------------- *)

args : "(" separated_list(",", exp) ")" { $2 }

params : (* empty *)                                 { [] }
       | "(" separated_nonempty_list(",", param) ")" { $2 }

param : LID typedec { Val (fst $1, $1, Some $2, Dynamic $2) }

recasg : LID "=" exp { ($1, $3) }

(* type declaration *)
typedec : ":" typ { $2 }

%%
