open Format
open Internal
open Type

(** Definition of searching state and symbol table. *)

type state = string list * simple_internal_iprop_multiset * internal_prop_set

let facts = ref PropSet.empty
let laws = ref IpropSet.empty

let pp_loval_var_list fmt = function
  | [] -> fprintf fmt "%%empty"
  | _ as local_var_list ->
      fprintf fmt "%a"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_print_string)
        local_var_list

let pp_state fmt (loval_var_list, ipr_mset, pr_set) =
  let loval_var_list_rev = List.rev loval_var_list in
  fprintf fmt "@[<v 4>locals@,%a@]@.@[<v 4>atoms@,%a@]@.@[<v 4>pures@,%a@]@."
    pp_loval_var_list loval_var_list
    (pp_simple_internal_iprop_multiset_env loval_var_list_rev
       ~pp_sep:pp_print_cut)
    ipr_mset
    (pp_internal_prop_set_env loval_var_list_rev ~pp_sep:pp_print_cut)
    pr_set

let symbol_table : (string, itype) Hashtbl.t = Hashtbl.create 17

(** Definition of termination exception. *)

exception Termination
