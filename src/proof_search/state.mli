open Format
open Internal
open Type

type state = {
  local_var_list : (string * itype) list;
  ipr_mset : simple_internal_iprop_multiset;
  pr_set : internal_prop_set;
}

val empty_state : state
val facts : internal_prop_set ref
val laws : internal_iprop_set ref
val pp_state : formatter -> state -> unit

exception Termination
