open Format
open Branch
open Internal
open Type

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

type state = {
  local_var_list : (string * itype) list;
  ipr_mset : simple_internal_iprop_multiset;
  pr_set : internal_prop_set;
  disj_list : simple_internal_iprop list list;
  branch : state branch;
  log : string;
}

val state_br : state -> state branch
val state_info : state -> string
val init_branch : state branch
val empty_state : state
val pp_state : formatter -> state -> unit
val pp_state_path : formatter -> state Path.path -> unit

exception Inconsistent of state option * string
