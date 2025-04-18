open Format

type state

val symbol_table : (string, Ast.itype) Hashtbl.t
val global_state : state ref
val state_size : state -> int * int
val pp_state : formatter -> state -> unit
val initial : Ast.instance -> state
val successors : state -> state list
val terminate : state -> bool
val estimate : state -> int
