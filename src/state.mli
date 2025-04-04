open Format
open Ast

type state

val pp_state_laws : formatter -> state -> unit
val pp_state_atoms : formatter -> state -> unit
val pp_state : formatter -> state -> unit
val initial : instance -> state
val successors : state -> state list
val terminate : state -> bool
