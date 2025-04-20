open Format
open Internal
open Type

(** Definition of searching state and symbol table. *)

type state = internal_prop_set * internal_iprop_multiset

let global_state : state ref = ref (PropSet.empty, IpropMset.empty)

let pp_state fmt (pr_set, ipr_mset) =
  fprintf fmt "@[<v 4>pures@,%a@]@.@[<v 4>props@,%a@]@."
    (pp_internal_prop_set ~pp_sep:pp_print_cut)
    pr_set
    (pp_internal_iprop_multiset ~pp_sep:pp_print_cut)
    ipr_mset

let symbol_table : (string, itype) Hashtbl.t = Hashtbl.create 17
