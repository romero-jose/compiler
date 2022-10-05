type 'a env

val init : unit -> 'a env
val lookup : string -> 'a env -> 'a
val extend : string -> 'a -> 'a env -> 'a env
