
type p = Lexing.position

type op =
  | Or | And
  | Equal | NotEqual
  | LessThanOrEqual | GreaterThanOrEqual
  | LessThan | GreaterThan
  | Plus | Minus | Mul | Div
  | Not

(* main *)

type ast = def list

and def =
  | Val of p * id * typ option * exp
  | Var of p * id * typ option * exp option
  | Fun of p * id * def list * typ option * block

and block = block_elem list

and typ =
  | Id    of id
  | Array of typ

and stmt =
  (* Simple *)
  | Asg    of lhs * exp
  | Call   of call
  | Return of exp option
  (* Compound *)
  | While  of exp * block
  | If     of exp * block * (exp * block) list * block option
  | Block  of block

and exp =
  | Dynamic of typ
  | Binary  of exp * op * exp
  | Unary   of op * exp
  | Literal of literal
  | Lhs     of lhs
  | Call    of call

and call =
  | Function    of id * exp list
  (*
  | Method      of exp * id * exp list
  | Constructor of typ * exp list
  *)

and literal =
  | True   of p
  | False  of p
  | Int    of p * int
  | Float  of p * float
  | String of p * string

and lhs =
  | Id      of id
  | Indexed of p * exp * exp

(* todo: call *)

(* auxiliary *)

and id = p * string

(* variable definition or statement *)
and block_elem = V of def | S of stmt
