open Format
open Internal
open Type

(** Definition of searching state and symbol table. *)

type state = {
  local_var_list : (string * itype) list;
  ipr_mset : simple_internal_iprop_multiset;
  pr_set : internal_prop_set;
}

let empty_state =
  {
    local_var_list = [];
    ipr_mset = SimpleIpropMset.empty;
    pr_set = PropSet.empty;
  }

let facts : internal_prop_set ref = ref PropSet.empty

type law = { intern : internal_iprop; extern : internal_iprop }

let pp_law_internal fmt { intern } = pp_internal_iprop fmt intern
let pp_law fmt { extern } = pp_internal_iprop fmt extern
let laws : law list ref = ref []

let pp_loval_var_list fmt = function
  | [] -> fprintf fmt "%%empty"
  | _ as local_var_list ->
      fprintf fmt "%a" (pp_typed_strs_list ()) (group_typed_str local_var_list)

let pp_state fmt { local_var_list; ipr_mset; pr_set } =
  let local_varname_list_rev = List.rev_map fst local_var_list in
  fprintf fmt "@[<v 4>locals@,%a@]@.@[<v 4>atoms@,%a@]@.@[<v 4>pures@,%a@]@."
    pp_loval_var_list local_var_list
    (pp_simple_internal_iprop_multiset_env local_varname_list_rev
       ~pp_sep:pp_print_cut)
    ipr_mset
    (pp_internal_prop_set_env local_varname_list_rev ~pp_sep:pp_print_cut)
    pr_set

(** Definition of termination exception. *)

exception Termination of string
