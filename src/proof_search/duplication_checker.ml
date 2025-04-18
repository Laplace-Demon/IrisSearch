open Internal
open State

let is_duplicate : state -> bool =
  let state_list = ref [] in
  let is_duplicate_aux ((pr_set, ipr_mset) as st) =
    if
      List.exists
        (fun (pr_set', ipr_mset') ->
          PropSet.subset pr_set pr_set' && IpropMset.subset ipr_mset ipr_mset')
        !state_list
    then (
      Statistics.record_duplication ();
      true)
    else (
      state_list := st :: !state_list;
      false)
  in
  is_duplicate_aux
