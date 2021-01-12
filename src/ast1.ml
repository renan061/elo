
type p = Lexing.position

type asgop = AsgSimple | AsgAdd | AsgMin | AsgMul | AsgDiv

type binop =
  | Or | And
  | Equal | NotEqual
  | LessThanOrEqual | GreaterThanOrEqual
  | LessThan | GreaterThan
  | Add | Min | Mul | Div
  | Not

type arrow =
  | RightExcluding
  | LeftExcluding
  | RightIncluding
  | LeftIncluding

(* main *)

type ast = def list

and def =
  | Val of p * id * typ option * exp
  | Var of p * id * typ option * exp option
  | Fun of p * id * def list * typ option * block
  | Rec of p * id * def list

and block = block_elem list

and typ =
  | Id    of id
  | Array of typ

and stmt =
  (* Simple *)
  | Asg    of lhs * asgop * exp
  | Call   of call
  | Return of exp option
  (* Compound *)
  | If     of exp * block * (exp * block) list * block option
  | While  of exp * block
  | For    of id * range * block
  | Block  of block

and exp =
  | Dynamic of typ
  | Binary  of exp * binop * exp
  | Unary   of binop * exp
  | Literal of literal
  | Lhs     of lhs
  | Call    of call

and call =
  | Function    of id * exp list
  | Method      of exp * id * exp list
  | Constructor of typ * exp list

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

and block_elem = V of def | S of stmt (* variable definition or statement *)

and range = Range of exp * arrow * exp
