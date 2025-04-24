open Ast
open Duplication_checker
open Internal
open Internal_operations
open State
open Type

let state_size (ipr_mset, pr_set) =
  (SimpleIpropMset.cardinal ipr_mset, PropSet.cardinal pr_set)

let initial { decl_facts; decl_laws; decl_init } =
  facts := prop_list_to_internal_prop_set decl_facts;
  laws := iprop_list_to_internal_iprop_set decl_laws;
  iprop_list_to_simple_internal_iprop_multiset_and_internal_prop_set decl_init

let apply law (ipr_mset, pr_set) =
  match law with
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
        if is_duplicate new_st then None else Some new_st
      with Multiplicity.Underflow -> None)
  | _ -> None

let successors st =
  IpropSet.fold
    (fun law acc ->
      match apply law st with
      | Some new_st ->
          Statistics.record_generated_state (state_size new_st);
          new_st :: acc
      | None -> acc)
    !laws []

let estimate = fun _ -> 0
