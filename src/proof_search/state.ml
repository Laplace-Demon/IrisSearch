open Format
open Internal
open Type

(** Definition of searching state and symbol table. *)

type state = simple_internal_iprop_multiset * internal_prop_set

let facts = ref PropSet.empty
let laws = ref IpropSet.empty

let pp_state fmt (ipr_mset, pr_set) =
  fprintf fmt "@[<v 4>atoms@,%a@]@.@[<v 4>pures@,%a@]@."
    (pp_simple_internal_iprop_multiset ~pp_sep:pp_print_cut)
    ipr_mset
    (pp_internal_prop_set ~pp_sep:pp_print_cut)
    pr_set

let symbol_table : (string, itype) Hashtbl.t = Hashtbl.create 17

(** Definition of termination exception. *)

exception Termination
