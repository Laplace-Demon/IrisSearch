open Internal
open State
open Type
module InfiniteArray = MenhirLib.InfiniteArray

let state_num = ref 0
let state_array : state InfiniteArray.t = InfiniteArray.make empty_state
let get_state = InfiniteArray.get state_array
let set_state st = InfiniteArray.set state_array st.index st

let make_state (local_var_list, ipr_mset, pr_set, disj_list) =
  let rec iter index =
    if index >= !state_num then None
    else
      let st = get_state index in
      if
        List.equal
          (fun (var1, ity1) (var2, ity2) ->
            String.equal var1 var2 && itype_eqb ity1 ity2)
          local_var_list st.local_var_list
        && SimpleIpropMset.equal ipr_mset st.ipr_mset
        && PropSet.equal pr_set st.pr_set
        && List.equal
             (List.equal (fun ipr1 ipr2 ->
                  compare_simple_internal_iprop ipr1 ipr2 = 0))
             disj_list st.disj_list
      then Some st
      else iter (index + 1)
  in
  match iter 0 with
  | Some st -> (st, false)
  | None ->
      let st =
        { index = !state_num; local_var_list; ipr_mset; pr_set; disj_list }
      in
      set_state st;
      state_num := !state_num + 1;
      (st, true)

let get_substates st =
  let substates = ref [] in
  for index = !state_num - 1 downto 0 do
    let st' = get_state index in
    if
      List.is_empty st'.disj_list
      && List.length st'.local_var_list < List.length st.local_var_list
      && SimpleIpropMset.subset st'.ipr_mset st.ipr_mset
      && PropSet.subset st'.pr_set st.pr_set
    then substates := st' :: !substates
  done;
  !substates

let get_superstates st =
  let superstates = ref [] in
  for index = !state_num - 1 downto 0 do
    let st' = get_state index in
    if
      List.is_empty st.disj_list
      && List.length st.local_var_list < List.length st'.local_var_list
      && SimpleIpropMset.subset st.ipr_mset st'.ipr_mset
      && PropSet.subset st.pr_set st'.pr_set
    then superstates := st' :: !superstates
  done;
  !superstates

(* module StateSet = Set.Make (Int)

type state_set = StateSet.t

module IpropMap = Map.Make (HPredId)

type iprop_map = state_set IpropMap.t

let iprop_state_map : iprop_map ref = ref IpropMap.empty

let register_state ({ ipr_mset; _ } as st) =
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

let candidate ipr_mset =
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

let is_sub_state { ipr_mset = ipr_mset1; pr_set = pr_set1; _ }
    { ipr_mset = ipr_mset2; pr_set = pr_set2; _ } =
  SimpleIpropMset.subset ipr_mset1 ipr_mset2 && PropSet.subset pr_set1 pr_set2

let subsume ({ ipr_mset; _ } as st) : bool =
  match candidate ipr_mset with
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
        Statistics.record_subsumption ();
        true)
      else
        let () = register_state st in
        false *)
