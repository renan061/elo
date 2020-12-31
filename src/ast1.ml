
type p = Lexing.position

(* main *)

type ast = def list

and def =
    | Val of p * id * typ option * exp
    | Var of p * id * typ option * exp option
    | Fun of p * id * def list * typ option * block

and block = block_elem list

and typ =
    | Id of id
    | Array of typ

and stmt =
    | Asg of lhs * exp

and exp =
    | Dynamic of typ
    | Literal of literal
    | Lhs of lhs

and literal =
    | True of p
    | False of p
    | Int of p * int
    | Float of p * float
    | String of p * string

and lhs =
    | Id of id
    | Indexed of p * exp * exp

(* todo: call *)

(* auxiliary *)

and id = p * string

(* variable definition or statement *)
and block_elem = V of def | S of stmt
