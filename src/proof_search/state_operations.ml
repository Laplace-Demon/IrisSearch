open Ast
open Branch
open Format
open Internal
open Internal_operations
open State
open Subsumption_checker
open Pattern_match
open Freshname

let state_size { ipr_mset; pr_set; _ } =
  (SimpleIpropMset.cardinal ipr_mset, PropSet.cardinal pr_set)

let transform_law = function
  | IHForall
      ( { shift; typed_str_list },
        IWand (ISimple (((ipr_mset, _) as ipr1), []), ipr2) ) ->
      let free_vars = free_vars_simple_internal_iprop_multiset ipr_mset in
      let free_vars_num = List.length free_vars in
      let new_binder_info =
        {
          shift = free_vars_num + shift;
          typed_str_list =
            List.map
              (fun (var, ity) -> (generate ~base:(VarId.export var) (), ity))
              free_vars
            @ typed_str_list;
        }
      in
      let ipr_mset, pr_set =
        subst_simple_internal_iprop
          (function
            | IVar var -> (
                match
                  List.find_index
                    (fun (var', _) -> VarId.equal var var')
                    free_vars
                with
                | Some ind ->
                    Some
                      (iBVar
                         ( free_vars_num - 1 - ind + shift,
                           List.assoc var free_vars ))
                | None -> None)
            | _ -> None)
          ipr1
      in
      let new_pr_set =
        PropSet.union pr_set
          (PropSet.of_list
             (List.mapi
                (fun ind (var, ity) ->
                  iEq
                    ( iVar (var, ity),
                      iBVar (free_vars_num - 1 - ind + shift, ity) ))
                free_vars))
      in
      iHForall
        (new_binder_info, iWand (iSimple ((ipr_mset, new_pr_set), []), ipr2))
  | IWand (ISimple (((ipr_mset, _) as ipr1), []), ipr2) as law -> (
      let free_vars = free_vars_simple_internal_iprop_multiset ipr_mset in
      match List.length free_vars with
      | 0 -> law
      | free_vars_num ->
          let new_binder_info =
            {
              shift = free_vars_num;
              typed_str_list =
                List.map
                  (fun (var, ity) ->
                    (generate ~base:(VarId.export var) (), ity))
                  free_vars;
            }
          in
          let ipr_mset, pr_set =
            subst_simple_internal_iprop
              (function
                | IVar var -> (
                    match
                      List.find_index
                        (fun (var', _) -> VarId.equal var var')
                        free_vars
                    with
                    | Some ind ->
                        Some
                          (iBVar
                             (free_vars_num - 1 - ind, List.assoc var free_vars))
                    | None -> None)
                | _ -> None)
              ipr1
          in
          let new_pr_set =
            PropSet.union pr_set
              (PropSet.of_list
                 (List.mapi
                    (fun ind (var, ity) ->
                      iEq (iVar (var, ity), iBVar (free_vars_num - 1 - ind, ity)))
                    free_vars))
          in
          iHForall
            (new_binder_info, iWand (iSimple ((ipr_mset, new_pr_set), []), ipr2))
      )
  | _ -> assert false

let initial { decl_facts; decl_laws; decl_init; _ } =
  (* Build facts and extract persistent specifications. *)
  let () =
    let all_facts =
      prop_list_to_internal_prop_set Validate.symbol_table decl_facts
    in
    let facts, persistent =
      PropSet.partition
        (function
          | IPersistent _ | IForall (_, IPersistent _) -> false | _ -> true)
        all_facts
    in
    global_state.facts <- facts;
    global_state.persistent <- persistent
  in
  (* Build laws. *)
  let () =
    global_state.laws <-
      List.map
        (fun (name_opt, ipr) ->
          let ipr = iprop_to_internal_iprop Validate.symbol_table ipr in
          { name_opt; extern = ipr; intern = transform_law ipr })
        decl_laws
  in
  let (ipr_mset, pr_set), disj_list =
    iprop_list_to_simple_internal_iprop_and_disj_list Validate.symbol_table
      decl_init
  in
  {
    local_var_list = [];
    ipr_mset;
    pr_set;
    disj_list = [ disj_list ];
    (* TODO *)
    branch = init_branch;
    log = "  path";
  }

let retrieve_law law =
  let shift, ipr_prems, pr_prems, concls =
    match law.intern with
    | IHForall
        ({ shift; _ }, IWand (ISimple ((ipr_prems, pr_prems), []), concls)) ->
        (shift, ipr_prems, pr_prems, concls)
    | IWand (ISimple ((ipr_prems, pr_prems), []), concls) ->
        (0, ipr_prems, pr_prems, concls)
    | _ -> assert false
  in
  let exists_var_list, ipr_concls, pr_concls, disj_list =
    match concls with
    | IHExists
        ({ typed_str_list; _ }, ISimple ((ipr_concls, pr_concls), disj_list)) ->
        ( List.map
            (fun (str, ity) -> (generate ~base:str (), ity))
            typed_str_list,
          ipr_concls,
          pr_concls,
          disj_list )
    | ISimple ((ipr_concls, pr_concls), disj_list) ->
        ([], ipr_concls, pr_concls, disj_list)
    | _ -> assert false
  in
  (shift, ipr_prems, pr_prems, exists_var_list, ipr_concls, pr_concls, disj_list)

let make_subst_task match_result = function
  | IBVar ind -> match_result.(ind)
  | _ -> None

type successors = { succ_or : state list; succ_and : state list }

open Monads.ListMonad

let apply law st =
  let ( shift,
        ipr_prems,
        pr_prems,
        exists_var_list,
        ipr_concls,
        pr_concls,
        disj_list ) =
    retrieve_law law
  in
  try
    (* instantiate quantified variables *)
    let* ipr_mset_prems_elim, is_inf, match_result =
      match shift = 0 with
      | true ->
          if not (Z3_intf.implication_solver (Some st) pr_prems) then fail
          else
            let ipr_mset_prems_elim, is_inf =
              SimpleIpropMset.diff st.ipr_mset ipr_prems
            in
            return (ipr_mset_prems_elim, is_inf, [||])
      | false ->
          let match_init = Array.init shift (fun _ -> None) in
          let* match_result, ipr_mset_prems_elim, is_inf =
            simple_internal_iprop_multiset_match (Some st) match_init ipr_prems
              st.ipr_mset
          in
          if match_result_complete match_result then
            let subst_task = make_subst_task match_result in
            let pr_prems = subst_internal_prop_set subst_task pr_prems in
            if not (Z3_intf.implication_solver (Some st) pr_prems) then fail
            else return (ipr_mset_prems_elim, is_inf, match_result)
          else fail
    in
    (* check for termination *)
    let () =
      if SimpleIpropMset.mem1 false_id ipr_concls then
        raise
          (Inconsistent
             (None, asprintf "Applying law (%a) yields False." pp_law law))
    in
    (* strengthen conclusion *)
    let ipr_concls, disj_list =
      if is_inf then
        ( SimpleIpropMset.map_multiplicity
            (fun _ _ -> Multiplicity.inf)
            ipr_concls,
          List.map
            (fun (ipr_mset, pr_set) ->
              ( SimpleIpropMset.map_multiplicity
                  (fun _ _ -> Multiplicity.inf)
                  ipr_mset,
                pr_set ))
            disj_list )
      else
        ( SimpleIpropMset.map_multiplicity
            (fun ipr count ->
              (* we can use the new state *)
              if Multiplicity.is_infinite count || Persistent_solver.solve ipr
              then Multiplicity.inf
              else count)
            ipr_concls,
          List.map
            (fun (ipr_mset, pr_set) ->
              ( SimpleIpropMset.map_multiplicity
                  (fun ipr count ->
                    (* we can use the new state *)
                    if
                      Multiplicity.is_infinite count
                      || Persistent_solver.solve ipr
                    then Multiplicity.inf
                    else count)
                  ipr_mset,
                pr_set ))
            disj_list )
    in
    (* subst quantified variables *)
    let ipr_concls, pr_concls, disj_list =
      let local_var_num = List.length st.local_var_list in
      let exists_match_result =
        Array.mapi
          (fun i (_, ity) -> Some (iBVar (i + local_var_num, ity)))
          (Array.of_list exists_var_list)
      in
      let match_result = Array.append exists_match_result match_result in
      match Array.length match_result = 0 with
      | true -> (ipr_concls, pr_concls, disj_list)
      | false ->
          let subst_task = make_subst_task match_result in
          ( subst_simple_internal_iprop_multiset subst_task ipr_concls,
            subst_internal_prop_set subst_task pr_concls,
            List.map (subst_simple_internal_iprop subst_task) disj_list )
    in
    let new_local_var_list = exists_var_list @ st.local_var_list in
    let new_ipr_mset = SimpleIpropMset.union ipr_concls ipr_mset_prems_elim in
    let new_pr_set = PropSet.union pr_concls st.pr_set in
    let new_st =
      {
        st with
        local_var_list = new_local_var_list;
        ipr_mset = new_ipr_mset;
        pr_set = new_pr_set;
        log = asprintf "  ↓ Applying law (%a)." pp_law law;
      }
    in
    let () =
      (* check consistency of facts *)
      match Z3_intf.consistent_solver (Some new_st) with
      | Some unsat_core -> raise (Inconsistent (Some new_st, unsat_core))
      | None -> ()
    in
    if subsume new_st then fail else return new_st
  with Multiplicity.Underflow -> fail

let case_analysis st disj_index =
  match List.nth_opt st.disj_list disj_index with
  | None -> assert false
  | Some disj ->
      let new_disj_list =
        List.filteri
          (fun disj_index' _ -> not (Int.equal disj_index disj_index'))
          st.disj_list
      in
      List.mapi
        (fun i ipr ->
          let ipr_mset', pr_set' =
            combine_simple_internal_iprop ipr (st.ipr_mset, st.pr_set)
          in
          {
            st with
            ipr_mset = ipr_mset';
            pr_set = pr_set';
            disj_list = new_disj_list;
            log =
              (let local_varname_list_rev =
                 List.rev_map fst st.local_var_list
               in
               asprintf "Splitting on (%a), branch %i:"
                 (pp_internal_iprop_env local_varname_list_rev)
                 (iSimple (empty_simple_internal_iprop, disj))
                 (i + 1));
          })
        disj

let case_analysis_hint st : int list =
  List.init (List.length st.disj_list) (fun i -> i)

let default_case_analysis_hint = [ 0 ]

let successors st =
  let from_law_application =
    List.fold_left
      (fun acc law ->
        let succ = apply law st in
        List.iter
          (fun new_st -> Statistics.record_generated_state (state_size new_st))
          succ;
        choose succ acc)
      fail global_state.laws
  in
  let from_case_analysis =
    let* disj_index =
      match case_analysis_hint st with
      | []
        when (not (List.is_empty st.disj_list))
             && List.is_empty from_law_application ->
          default_case_analysis_hint
      | hint -> hint
    in
    let succ = case_analysis st disj_index in
    let succ_br = add_br st.branch (List.length succ) in
    List.map2
      (fun succ' succ_br' -> { succ' with branch = succ_br' })
      succ succ_br
  in
  choose from_law_application from_case_analysis

let consistent st =
  Option.is_none (Z3_intf.consistent_solver (Some st))
  && not (SimpleIpropMset.mem1 false_id st.ipr_mset)
