open Expr
open Env

let ( let* ) = Option.bind

type tenv = t env

let typecheck_op1 (op : op1) (t : t) =
  match op with Not -> if t = BoolT then Some BoolT else None

let typecheck_op2 (op : op2) (t1 : t) (t2 : t) =
  match op with
  | Plus | Minus -> if (t1, t2) = (NatT, NatT) then Some NatT else None
  | And | Or -> if (t1, t2) = (BoolT, BoolT) then Some BoolT else None

let rec typecheck : tenv -> expr -> t option =
 fun tenv expr ->
  match expr with
  | NatE _ -> Some NatT
  | BoolE _ -> Some BoolT
  | VarE id -> Some (lookup id tenv)
  | LetE (id, e1, e2) ->
      let* t = typecheck tenv e1 in
      typecheck (extend id t tenv) e2
  | Op1E (op, e) ->
      let* t = typecheck tenv e in
      typecheck_op1 op t
  | Op2E (op, e1, e2) ->
      let* t1 = typecheck tenv e1 in
      let* t2 = typecheck tenv e2 in
      typecheck_op2 op t1 t2
