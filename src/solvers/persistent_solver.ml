open Internal
open Internal_operations
open State

let solve : hpred_id * internal_term array -> state -> bool =
 fun (hpred, tm_arr) (_, _, pr_set) ->
  PropSet.exists
    (function
      | IPersistent (ISimple (ipr_mset, _)) ->
          SimpleIpropMset.exists
            (fun (hpred', tm_arr') _ ->
              HPredId.equal hpred hpred'
              && compare_internal_term_array tm_arr tm_arr' = 0)
            ipr_mset
      | IForall ({ shift }, IPersistent (ISimple (ipr_mset, _))) ->
          SimpleIpropMset.exists
            (fun (hpred', tm_arr') _ ->
              HPredId.equal hpred hpred'
              &&
              let match_init =
                Array.init (Array.length tm_arr) (fun _ -> None)
              in
              not
                (List.is_empty
                   (internal_term_array_match match_init tm_arr' tm_arr)))
            ipr_mset
      | _ -> false)
    pr_set
