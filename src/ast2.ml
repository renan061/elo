
type id = string

(* nodes *)

type ast = (def list, string) result

and def = {
  p: Lexing.position;
  id: id;
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
  | Val of typ * exp
  | Var of typ * exp
  | Fun of def list * typ * block
  | Rec of def list

and stmtU =
  | Asg of lhs * exp

and expU =
  | Dynamic
  | ZeroValue
  | LiteralNil
  | LiteralTrue
  | LiteralFalse
  | LiteralInt of int
  | LiteralFloat of float
  | LiteralString of string
  | LiteralArray of exp list
  | LiteralRecord of (lhs * exp) list
  | Lhs of lhs

and lhsU =
  | Id of def
  | Index of exp * exp
  | Field of exp * def

and typ =
  | Void
  | Bool
  | Int
  | Float
  | String
  | Array of typ
  | Record of def