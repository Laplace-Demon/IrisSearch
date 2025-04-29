open Internal
open State

let is_duplicate : state -> bool =
  let state_list = ref [] in
  let is_duplicate_aux ((local_var_list, ipr_mset, pr_set) as st) =
    if
      List.exists
        (fun (local_var_list', ipr_mset', pr_set') ->
          (* TODO *)
          SimpleIpropMset.subset ipr_mset ipr_mset'
          && PropSet.subset pr_set pr_set')
        !state_list
    then (
      Statistics.record_duplication ();
      true)
    else (
      state_list := st :: !state_list;
      false)
  in
  is_duplicate_aux
