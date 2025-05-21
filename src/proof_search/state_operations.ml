open Ast
open Duplication_checker
open Internal
open Internal_operations
open State
open Type
open Freshname

let state_size { ipr_mset; pr_set } =
  (SimpleIpropMset.cardinal ipr_mset, PropSet.cardinal pr_set)

let initial { decl_facts; decl_laws; decl_init } =
  let () =
    facts := prop_list_to_internal_prop_set Validate.symbol_table decl_facts
  in
  let () =
    laws := iprop_list_to_internal_iprop_set Validate.symbol_table decl_laws
  in
  let ipr_mset, pr_set =
    iprop_list_to_simple_internal_iprop_multiset_and_internal_prop_set
      Validate.symbol_table decl_init
  in
  { local_var_list = []; ipr_mset; pr_set }

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
          ( List.map
              (fun (str, ity) -> (generate ~base:str (), ity))
              typed_str_list,
            ipr_concls,
            pr_concls )
    | ISimple (ipr_concls, pr_concls) -> return ([], ipr_concls, pr_concls)
    | _ -> fail
  in
  return (shift, ipr_prems, pr_prems, exists_var_list, ipr_concls, pr_concls)

open Monads.ListMonad

let apply law ({ local_var_list; ipr_mset; pr_set } as st) =
  match retrieve_law law with
  | None -> fail
  | Some (shift, ipr_prems, pr_prems, exists_var_list, ipr_concls, pr_concls)
    -> (
      try
        (* instantiate quantified variables *)
        let* ipr_mset_prems_elim, is_inf, subst_task =
          match shift = 0 with
          | true ->
              if not (Z3_intf.implication_solver (Some st) pr_prems) then fail
              else
                let ipr_mset_prems_elim, is_inf =
                  SimpleIpropMset.diff ipr_mset ipr_prems
                in
                return (ipr_mset_prems_elim, is_inf, [||])
          | false ->
              let match_init = Array.init shift (fun _ -> None) in
              let* match_result, ipr_mset_prems_elim, is_inf =
                simple_internal_iprop_multiset_match (Some st) match_init
                  ipr_prems ipr_mset
              in
              if match_result_complete match_result then
                let pr_prems = subst_internal_prop_set match_result pr_prems in
                if not (Z3_intf.implication_solver (Some st) pr_prems) then fail
                else return (ipr_mset_prems_elim, is_inf, match_result)
              else fail
        in
        (* check for termination *)
        let () =
          if SimpleIpropMset.mem1 false_id ipr_concls then
            raise
              (Termination
                 Format.(
                   asprintf "@.@[<v 4>Applying law@,%a@.yields False.@]@."
                     pp_internal_iprop law))
        in
        (* strengthen conclusion *)
        let ipr_concls =
          if is_inf then
            SimpleIpropMset.map_multiplicity
              (fun _ _ -> Multiplicity.inf)
              ipr_concls
          else
            SimpleIpropMset.map_multiplicity
              (fun ipr count ->
                (* we can use the new state *)
                if Multiplicity.is_infinite count || Persistent_solver.solve ipr
                then Multiplicity.inf
                else count)
              ipr_concls
        in
        (* subst quantified variables *)
        let ipr_concls, pr_concls =
          let local_var_num = List.length local_var_list in
          let exists_subst_task =
            Array.mapi
              (fun i (_, ity) -> Some (iBVar (i + local_var_num, ity)))
              (Array.of_list exists_var_list)
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
        let new_pr_set = PropSet.union pr_concls pr_set in
        let new_st =
          {
            local_var_list = new_local_var_list;
            ipr_mset = new_ipr_mset;
            pr_set = new_pr_set;
          }
        in
        let () =
          (* check consistency of facts *)
          match Z3_intf.consistent_solver (Some new_st) with
          | Some unsat_core ->
              raise
                (Termination
                   Format.(asprintf "@.↓@.@.%a%s" pp_state new_st unsat_core))
          | None -> ()
        in
        if is_dup new_st then fail else return new_st
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

let consistent st =
  Option.is_none (Z3_intf.consistent_solver (Some st))
  && not (SimpleIpropMset.mem1 false_id st.ipr_mset)
