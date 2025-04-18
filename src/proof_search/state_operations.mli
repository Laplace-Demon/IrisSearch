open State

val state_size : state -> int * int
val initial : Ast.instance -> state
val successors : state -> state list
val terminate : state -> bool
val estimate : state -> int
