open Internal
open State

module HashedOrderedState = struct
  type t = state

  let compare (pr_set1, ipr_mset1) (pr_set2, ipr_mset2) =
    let tmp = PropSet.compare pr_set1 pr_set2 in
    if tmp = 0 then IpropMset.compare ipr_mset1 ipr_mset2 else tmp

  let hash (pr_set, ipr_mset) =
    Hashtbl.hash (PropSet.hash pr_set, IpropMset.hash ipr_mset)
end

module StateSet = Set.Make (HashedOrderedState)

type state_set = StateSet.t

module PropStateMap = Baby.W.Map.Make (HashedOrderedInternalProp)
module IpropStateMap = Baby.W.Map.Make (HashedOrderedInternalIprop)

let prop_state_table : state_set PropStateMap.t ref = ref PropStateMap.empty
let iprop_state_table : state_set IpropStateMap.t ref = ref IpropStateMap.empty

let record_state_prop st pr =
  match PropStateMap.find_opt pr !prop_state_table with
  | Some st_set ->
      prop_state_table :=
        PropStateMap.add pr (StateSet.add st st_set) !prop_state_table
  | None ->
      prop_state_table :=
        PropStateMap.add pr (StateSet.singleton st) !prop_state_table

let record_state_iprop st ipr =
  match IpropStateMap.find_opt ipr !iprop_state_table with
  | Some st_set ->
      iprop_state_table :=
        IpropStateMap.add ipr (StateSet.add st st_set) !iprop_state_table
  | None ->
      iprop_state_table :=
        IpropStateMap.add ipr (StateSet.singleton st) !iprop_state_table

let record_state ((pr_set, ipr_mset) as st) =
  PropSet.iter (fun pr -> record_state_prop st pr) pr_set;
  IpropMset.iter (fun ipr _ -> record_state_iprop st ipr) ipr_mset

let is_duplicate : state -> bool =
  let () = Random.self_init () in
  let is_duplicate_aux ((pr_set, ipr_mset) as st) =
    let pr_set_size = PropSet.cardinal pr_set in
    let ipr_mset_size = IpropMset.cardinal ipr_mset in
    let state_set = ref None in
    let () =
      for i = 1 to Int.min pr_set_size 5 do
        let prop_sample = PropSet.get pr_set (Random.int pr_set_size) in
        let pr_st_set =
          match PropStateMap.find_opt prop_sample !prop_state_table with
          | Some pr_st_set -> pr_st_set
          | None -> StateSet.empty
        in
        match !state_set with
        | Some st_set -> state_set := Some (StateSet.union st_set pr_st_set)
        | None -> state_set := Some pr_st_set
      done
    in
    let () =
      for i = 1 to Int.min ipr_mset_size 5 do
        let iprop_sample, _ =
          IpropMset.get ipr_mset (Random.int ipr_mset_size)
        in
        let ipr_st_set =
          match IpropStateMap.find_opt iprop_sample !iprop_state_table with
          | Some ipr_st_set -> ipr_st_set
          | None -> StateSet.empty
        in
        match !state_set with
        | Some st_set -> state_set := Some (StateSet.union st_set ipr_st_set)
        | None -> state_set := Some ipr_st_set
      done
    in
    if
      Option.is_some !state_set
      && StateSet.exists
           (fun (pr_set', ipr_mset') ->
             PropSet.subset pr_set pr_set'
             && IpropMset.subset ipr_mset ipr_mset')
           (Option.get !state_set)
    then (
      Statistics.record_duplication ();
      true)
    else (
      record_state st;
      false)
  in
  is_duplicate_aux
