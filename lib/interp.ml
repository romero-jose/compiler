open Expr
open Env

type value = NatV of int | BoolV of bool

let pp_value : value Fmt.t =
 fun fmt v -> match v with NatV n -> Fmt.int fmt n | BoolV b -> Fmt.bool fmt b

exception TypeError of string

let rec interp : value env -> expr -> value =
 fun env expr ->
  match expr with
  | NatE n -> NatV n
  | BoolE b -> BoolV b
  | VarE id -> lookup id env
  | LetE (name, e1, e2) ->
      let value = interp env e1 in
      interp ((name, value) :: env) e2
  | Op1E (op, e1) -> (
      match (op, interp env e1) with
      | Not, BoolV b -> BoolV (not b)
      | _ -> raise (TypeError "Unary operation: Unexpected type"))
  | Op2E (op, e1, e2) -> (
      match (op, interp env e1, interp env e2) with
      | Plus, NatV n1, NatV n2 -> NatV (n1 + n2)
      | Minus, NatV n1, NatV n2 -> NatV (max 0 (n1 - n2))
      | And, BoolV b1, BoolV b2 -> BoolV (b1 && b2)
      | Or, BoolV b1, BoolV b2 -> BoolV (b1 || b2)
      | _ -> raise (TypeError "Binary operation: Unexpected type"))
