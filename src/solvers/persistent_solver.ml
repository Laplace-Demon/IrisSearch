open Internal
open Internal_operations
open State

let solve : hpred_id * internal_term array -> bool =
 fun (hpred, tm_arr) ->
  let knowledge = global_state.persistent in
  PropSet.exists
    (function
      | IPersistent (ISimple ((ipr_mset, _), _)) ->
          SimpleIpropMset.exists
            (fun (hpred', tm_arr') _ ->
              HPredId.equal hpred hpred'
              && compare_internal_term_array tm_arr tm_arr' = 0)
            ipr_mset
      | IForall ({ shift; _ }, IPersistent (ISimple ((ipr_mset, _), _))) ->
          SimpleIpropMset.exists
            (fun (hpred', tm_arr') _ ->
              HPredId.equal hpred hpred'
              &&
              let match_init = Array.init shift (fun _ -> None) in
              not
                (List.is_empty
                   (internal_term_array_match None match_init tm_arr' tm_arr)))
            ipr_mset
      | _ -> false)
    knowledge
