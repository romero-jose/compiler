type t = BoolT | NatT
type op1 = Not
type op2 = Plus | Minus | And | Or

type expr =
  | NatE of int
  | BoolE of bool
  | VarE of string
  | LetE of string * expr * expr
  | Op1E of op1 * expr
  | Op2E of op2 * expr * expr
