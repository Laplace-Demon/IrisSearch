open State

type successors = { succ_or : state list; succ_and : state list }

val state_size : state -> int * int
val initial : Ast.instance -> state
val successors : state -> state list * bool
val consistent : state -> bool
