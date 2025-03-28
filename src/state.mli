open Format
open Ast

type state

val state_equal : state -> state -> bool
val state_hash : state -> int
val pp_state : formatter -> state -> unit
val init : instance -> state
val succ : state -> state list
val terminate : state -> bool
