open State

val state_size : state -> int * int
val initial : Ast.instance -> state
val successors : state -> state list
val estimate : state -> int
