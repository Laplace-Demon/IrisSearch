open Ast
open Duplication_checker
open Internal
open Internal_operations
open State
open Type
open Format

let state_size (ipr_mset, pr_set) =
  (SimpleIpropMset.cardinal ipr_mset, PropSet.cardinal pr_set)

let initial { decl_facts; decl_laws; decl_init } =
  facts := prop_list_to_internal_prop_set decl_facts;
  laws := iprop_list_to_internal_iprop_set decl_laws;
  iprop_list_to_simple_internal_iprop_multiset_and_internal_prop_set decl_init

open Monads.ListMonad

let pp_match_result =
  pp_print_array ~pp_sep:pp_print_newline
    (pp_print_option ~none:(fun fmt () -> printf "none") pp_internal_term)

let apply law (ipr_mset, pr_set) =
  match law with
  | IHForall
      ( { shift },
        IWand (ISimple (ipr_prems, pr_prems), ISimple (ipr_concls, pr_concls))
      ) ->
      let match_init = Array.init shift (fun _ -> None) in
      let* match_result, ipr_mset_prems_elim, is_inf =
        simple_internal_iprop_multiset_match match_init ipr_prems ipr_mset
      in
      let* match_result', pr_set_prems_elim =
        internal_prop_set_match match_result pr_prems pr_set
      in
      let () =
        if SimpleIpropMset.mem1 false_id ipr_concls then raise Termination
      in
      let ipr_concls =
        if is_inf then
          SimpleIpropMset.map_multiplicity
            (fun _ _ -> Multiplicity.inf)
            ipr_concls
        else ipr_concls
      in
      let ipr_concls_instantiate =
        subst_simple_internal_iprop_multiset match_result' ipr_concls
      in
      let pr_concls_instantiate =
        subst_internal_prop_set match_result' pr_concls
      in
      let new_ipr_mset =
        SimpleIpropMset.union ipr_concls_instantiate ipr_mset_prems_elim
      in
      let new_pr_set = PropSet.union pr_concls_instantiate pr_set_prems_elim in
      let new_st = (new_ipr_mset, new_pr_set) in
      if is_duplicate new_st then fail
      else (
        Format.printf "@.%a@." pp_state new_st;
        return new_st)
  | IWand (ISimple (ipr_prems, pr_prems), ISimple (ipr_concls, pr_concls)) -> (
      try
        let ipr_mset_prems_elim, is_inf =
          SimpleIpropMset.diff ipr_mset ipr_prems
        in
        let pr_set_prems_elim = PropSet.diff pr_set pr_prems in
        let () =
          if SimpleIpropMset.mem1 false_id ipr_concls then raise Termination
        in
        let ipr_concls =
          if is_inf then
            SimpleIpropMset.map_multiplicity
              (fun _ _ -> Multiplicity.inf)
              ipr_concls
          else ipr_concls
        in
        let new_ipr_mset =
          SimpleIpropMset.union ipr_concls ipr_mset_prems_elim
        in
        let new_pr_set = PropSet.union pr_concls pr_set_prems_elim in
        let new_st = (new_ipr_mset, new_pr_set) in
        if is_duplicate new_st then fail else return new_st
      with Multiplicity.Underflow -> fail)
  | _ -> fail

let successors st =
  IpropSet.fold
    (fun law acc ->
      let succ = apply law st in
      List.iter
        (fun new_st -> Statistics.record_generated_state (state_size new_st))
        succ;
      choose succ acc)
    !laws fail

let estimate = fun _ -> 0
