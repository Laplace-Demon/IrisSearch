open Format
open Internal
open Type

type state = string list * simple_internal_iprop_multiset * internal_prop_set

val facts : internal_prop_set ref
val laws : internal_iprop_set ref
val pp_state : formatter -> state -> unit
val symbol_table : (string, itype) Hashtbl.t

exception Termination
