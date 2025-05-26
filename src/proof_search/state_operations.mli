open State

val state_size : state -> int * int
val initial : Ast.instance -> state
val successors : state -> state list * bool
val consistent : state -> bool
