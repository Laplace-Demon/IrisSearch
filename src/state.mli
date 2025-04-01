open Format
open Ast

type state

val pp_state : formatter -> state -> unit
val init : instance -> state
val succ : state -> state list
val terminate : state -> bool
