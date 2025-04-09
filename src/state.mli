open Format
open Ast

type state

val state_size : state -> int

val pp_state : formatter -> state -> unit
val initial : instance -> state
val successors : state -> state list
val terminate : state -> bool
val estimate : state -> int
