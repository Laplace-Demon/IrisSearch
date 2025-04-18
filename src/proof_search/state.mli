open Format
open Internal

type state = internal_prop_set * internal_iprop_multiset

val global_state : state ref
val pp_state : formatter -> state -> unit
val symbol_table : (string, Ast.itype) Hashtbl.t
