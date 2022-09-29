type 'a env = (string * 'a) list

let lookup : string -> 'a env -> 'a =
 fun id env ->
  try List.assoc id env
  with Not_found -> Fmt.failwith "%s not found in environment" id

let extend : string -> 'a -> 'a env -> 'a env = fun id v env -> (id, v) :: env
