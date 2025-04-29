open Ast
open Duplication_checker
open Internal
open Internal_operations
open State
open Type
open Freshname

let state_size (_, ipr_mset, pr_set) =
  (SimpleIpropMset.cardinal ipr_mset, PropSet.cardinal pr_set)

let initial { decl_facts; decl_laws; decl_init } =
  let () = facts := prop_list_to_internal_prop_set decl_facts in
  let () = laws := iprop_list_to_internal_iprop_set decl_laws in
  let ipr_mset, pr_set =
    iprop_list_to_simple_internal_iprop_multiset_and_internal_prop_set decl_init
  in
  ([], ipr_mset, pr_set)

let retrieve_law law =
  let open Monads.OptionMonad in
  let* shift, ipr_prems, pr_prems, concls =
    match law with
    | IHForall ({ shift }, IWand (ISimple (ipr_prems, pr_prems), concls)) ->
        return (shift, ipr_prems, pr_prems, concls)
    | IWand (ISimple (ipr_prems, pr_prems), concls) ->
        return (0, ipr_prems, pr_prems, concls)
    | _ -> fail
  in
  let* exists_var_list, ipr_concls, pr_concls =
    match concls with
    | IHExists ({ typed_str_list }, ISimple (ipr_concls, pr_concls)) ->
        return
          ( List.map (fun (str, _) -> generate ~base:str ()) typed_str_list,
            ipr_concls,
            pr_concls )
    | ISimple (ipr_concls, pr_concls) -> return ([], ipr_concls, pr_concls)
    | _ -> fail
  in
  return (shift, ipr_prems, pr_prems, exists_var_list, ipr_concls, pr_concls)

open Monads.ListMonad

let apply law (local_var_list, ipr_mset, pr_set) =
  match retrieve_law law with
  | None -> fail
  | Some (shift, ipr_prems, pr_prems, exists_var_list, ipr_concls, pr_concls)
    -> (
      try
        (* instantiate quantified variables *)
        let* ipr_mset_prems_elim, pr_set_prems_elim, is_inf, subst_task =
          match shift = 0 with
          | true ->
              let ipr_mset_prems_elim, is_inf =
                SimpleIpropMset.diff ipr_mset ipr_prems
              in
              let pr_set_prems_elim = PropSet.diff pr_set pr_prems in
              return (ipr_mset_prems_elim, pr_set_prems_elim, is_inf, [||])
          | false ->
              let match_init = Array.init shift (fun _ -> None) in
              let* match_result, ipr_mset_prems_elim, is_inf =
                simple_internal_iprop_multiset_match match_init ipr_prems
                  ipr_mset
              in
              let+ match_result', pr_set_prems_elim =
                internal_prop_set_match match_result pr_prems pr_set
              in
              (ipr_mset_prems_elim, pr_set_prems_elim, is_inf, match_result')
        in
        (* check for termination *)
        let () =
          if SimpleIpropMset.mem1 false_id ipr_concls then raise Termination
        in
        (* strengthen conclusion *)
        let ipr_concls =
          if is_inf then
            SimpleIpropMset.map_multiplicity
              (fun _ _ -> Multiplicity.inf)
              ipr_concls
          else ipr_concls
        in
        (* subst quantified variables *)
        let ipr_concls, pr_concls =
          let local_var_num = List.length local_var_list in
          let exists_subst_task =
            Array.init (List.length exists_var_list) (fun i ->
                Some (iBVar (i + local_var_num)))
          in
          let subst_task = Array.append exists_subst_task subst_task in
          match Array.length subst_task = 0 with
          | true -> (ipr_concls, pr_concls)
          | false ->
              ( subst_simple_internal_iprop_multiset subst_task ipr_concls,
                subst_internal_prop_set subst_task pr_concls )
        in
        let new_local_var_list = exists_var_list @ local_var_list in
        let new_ipr_mset =
          SimpleIpropMset.union ipr_concls ipr_mset_prems_elim
        in
        let new_pr_set = PropSet.union pr_concls pr_set_prems_elim in
        let new_st = (new_local_var_list, new_ipr_mset, new_pr_set) in
        if is_duplicate new_st then fail else return new_st
      with Multiplicity.Underflow -> fail)

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
