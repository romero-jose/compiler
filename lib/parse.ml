open CCSexp
open Expr

let identifier_regexp = Str.regexp {|[_a-zA-Z]+[_\-a-zA-Z0-9]*|}

let is_valid_identifier (s : string) : bool =
  Str.string_match identifier_regexp s 0

let parse_literal (s : string) : expr =
  match s with
  | "true" -> BoolE true
  | "false" -> BoolE false
  | _ -> (
      match int_of_string_opt s with
      | Some n -> NatE n
      | None ->
          if is_valid_identifier s then VarE s
          else Fmt.failwith "parse: ~a is not a valid identifier" s)

let rec parse (sexp : sexp) : expr =
  match sexp with
  | `Atom id -> parse_literal id
  | `List [ `Atom "let"; `List [ `Atom id; binding ]; s ] ->
      if is_valid_identifier id then LetE (id, parse binding, parse s)
      else Fmt.failwith "parse: ~a is not a valid identifier" id
  | `List [ `Atom "not"; s ] -> Op1E (Not, parse s)
  | `List [ `Atom "and"; s1; s2 ] -> Op2E (And, parse s1, parse s2)
  | `List [ `Atom "or"; s1; s2 ] -> Op2E (Or, parse s1, parse s2)
  | `List [ `Atom "+"; s1; s2 ] -> Op2E (Plus, parse s1, parse s2)
  | `List [ `Atom "-"; s1; s2 ] -> Op2E (Minus, parse s1, parse s2)
  | _ -> Fmt.failwith "parse: Invalid symbolic expression ~a" sexp

let parse_string : string -> expr =
 fun s ->
  match CCSexp.parse_string s with
  | Ok sexpr -> parse sexpr
  | Error error -> Fmt.failwith "parse: Can't parse string ~a" error
