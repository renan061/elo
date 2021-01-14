
type id = string

type typ =
  | Void
  | Bool
  | Int
  | Float
  | String
  | Array of typ
  | Record of id

(* nodes *)

type ast = (def list, string) result

and def = {
  p: Lexing.position;
  id: id;
  typ: typ;
  mutable u: defU; (* mutated only during the semantic analysis *)

  mutable llv: Llvm.llvalue;
}

and block = block_elem list

and block_elem = V of def | S of stmt

and stmt = {
  p: Lexing.position;
  u: stmtU;
}

and exp = {
  p: Lexing.position;
  typ: typ;
  u: expU;
}

and lhs = {
  p: Lexing.position;
  typ: typ;
  u: lhsU;
}

(* union types *)

and defU =
  | Val of exp
  | Var of exp
  | Fun of def list * block
  | Rec of def list

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
