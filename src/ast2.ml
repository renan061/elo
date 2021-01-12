
type id = string

type typ =
  | Void
  | Bool
  | Int
  | Float
  | String
  | Array of typ

(* nodes *)

type ast = (def list, string) result

and def = {
  pos: Lexing.position;
  id: id;
  typ: typ;
  u: defU;

  mutable llv: Llvm.llvalue;
}

and block = block_elem list

and block_elem = V of def | S of stmt

and stmt = {
  pos: Lexing.position;
  u: stmtU;
}

and exp = {
  pos: Lexing.position;
  typ: typ;
  u: expU;
}

and lhs = {
  pos: Lexing.position;
  typ: typ;
  u: lhsU;
}

(* union types *)

and defU =
  | Val of exp
  | Var of exp
  | Fun of def list * block

and stmtU =
  | Asg of lhs * exp

and expU =
  | Dynamic
  | ZeroValue
  | LiteralTrue
  | LiteralFalse
  | LiteralInt of int
  | LiteralFloat of float
  | LiteralString of string
  | Lhs of lhs

and lhsU =
  | Id of id * def
  | Indexed of exp * exp
