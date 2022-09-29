open Expr
open Env

let ( let* ) = Option.bind

let typecheck_op1 (op : op1) (t : t) =
  match op with Not -> if t = BoolT then Some BoolT else None

let typecheck_op2 (op : op2) (t1 : t) (t2 : t) =
  match op with
  | Plus | Minus -> if (t1, t2) = (NatT, NatT) then Some NatT else None
  | And | Or -> if (t1, t2) = (BoolT, BoolT) then Some BoolT else None

let rec typecheck : t env -> expr -> t option =
 fun env expr ->
  match expr with
  | NatE _ -> Some NatT
  | BoolE _ -> Some BoolT
  | VarE id -> Some (lookup id env)
  | LetE (id, e1, e2) ->
      let* t = typecheck env e1 in
      typecheck (extend id t env) e2
  | Op1E (op, e) ->
      let* t = typecheck env e in
      typecheck_op1 op t
  | Op2E (op, e1, e2) ->
      let* t1 = typecheck env e1 in
      let* t2 = typecheck env e2 in
      typecheck_op2 op t1 t2
