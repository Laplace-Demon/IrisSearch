open Format
open Internal
open Type

type state = {
  local_var_list : (string * itype) list;
  ipr_mset : simple_internal_iprop_multiset;
  pr_set : internal_prop_set;
}

val pp_state : formatter -> state -> unit
val empty_state : state

type law = {
  name_opt : string option;
  intern : internal_iprop;
  extern : internal_iprop;
}

val pp_law : formatter -> law -> unit
val pp_law_internal : formatter -> law -> unit

type global_state = {
  mutable persistent : internal_prop_set;
  mutable facts : internal_prop_set;
  mutable laws : law list;
}

val global_state : global_state
val pp_global_state : formatter -> unit -> unit

exception Inconsistent
