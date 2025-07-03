open Format
open Internal
open Type
open Path

type law = {
  index : int;
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
  index : int;
  local_var_list : (string * itype) list;
  ipr_mset : simple_internal_iprop_multiset;
  pr_set : internal_prop_set;
  disj_list : simple_internal_iprop list list;
}

val get_index : state -> int
val empty_state : state
val pp_state : ?pp_index:bool -> formatter -> state -> unit
val pp_state_path : formatter -> state path -> unit
val pp_state_debug : formatter -> state -> unit
val pp_laws_debug : formatter -> unit -> unit
