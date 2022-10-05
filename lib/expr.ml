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

let pp_op1 fmt op = match op with Not -> Fmt.string fmt "not"

let pp_op2 fmt op =
  match op with
  | Plus -> Fmt.string fmt "+"
  | Minus -> Fmt.string fmt "-"
  | And -> Fmt.string fmt "and"
  | Or -> Fmt.string fmt "or"

let rec pp_expr fmt expr =
  match expr with
  | NatE n -> Fmt.int fmt n
  | BoolE b -> Fmt.bool fmt b
  | VarE id -> Fmt.string fmt id
  | LetE (id, e1, e2) ->
      Fmt.pf fmt "(let (%a %a) %a)" Fmt.string id pp_expr e1 pp_expr e2
  | Op1E (op, e) -> Fmt.pf fmt "(%a %a)" pp_op1 op pp_expr e
  | Op2E (op, e1, e2) -> Fmt.pf fmt "(%a %a %a)" pp_op2 op pp_expr e1 pp_expr e2
