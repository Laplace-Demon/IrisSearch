open Internal
open State
open MenhirLib

let state_num = ref 0
let state_array = InfiniteArray.make empty_state

module StateSet = Set.Make (Int)

type state_set = StateSet.t

module IpropMap = Map.Make (HPredId)

type iprop_map = state_set IpropMap.t

let iprop_state_map : iprop_map ref = ref IpropMap.empty

let register_state ((_, ipr_mset, _) as st) =
  let ind = !state_num in
  state_num := ind - 1;
  InfiniteArray.set state_array (-ind) st;
  SimpleIpropMset.iter1
    (fun hpred ->
      iprop_state_map :=
        IpropMap.update hpred
          (fun st_set_opt ->
            match st_set_opt with
            | Some st_set -> Some (StateSet.add ind st_set)
            | None -> Some (StateSet.singleton ind))
          !iprop_state_map)
    ipr_mset

let get_state ind = InfiniteArray.get state_array (-ind)

let dup_candidate ipr_mset =
  let st_set = ref None in
  SimpleIpropMset.iter1
    (fun hpred ->
      match (IpropMap.find_opt hpred !iprop_state_map, !st_set) with
      | Some new_st_set, Some old_st_set ->
          st_set := Some (StateSet.inter new_st_set old_st_set)
      | Some new_st_set, None -> st_set := Some new_st_set
      | None, _ -> ())
    ipr_mset;
  !st_set

let is_sub_state (_, ipr_mset1, pr_set1) (_, ipr_mset2, pr_set2) =
  SimpleIpropMset.subset ipr_mset1 ipr_mset2 && PropSet.subset pr_set1 pr_set2

let is_dup ((_, ipr_mset, _) as st) : bool =
  match dup_candidate ipr_mset with
  | None ->
      let () = register_state st in
      false
  | Some st_set ->
      if
        StateSet.exists
          (fun ind ->
            let st' = get_state ind in
            is_sub_state st st')
          st_set
      then (
        Statistics.record_duplication ();
        true)
      else
        let () = register_state st in
        false
