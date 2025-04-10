open Format
open Internal
open Statistics

(** Definition of state and its operations. *)

type state = internal_prop_set * internal_iprop_multiset

(** Maybe it's better to use a list because the global state won't change and we
    don't want to convert *)

let global_state : state ref = ref (PropSet.empty, IpropMset.empty)

let state_size (pr_set, ipr_mset) =
  (PropSet.cardinal pr_set, IpropMset.cardinal ipr_mset)

let pp_state fmt (pr_set, ipr_mset) =
  fprintf fmt "@[<v 4>pures@,%a@]@.@[<v 4>props@,%a@]@."
    (pp_internal_prop_set ~pp_sep:pp_print_cut)
    pr_set
    (pp_internal_iprop_multiset ~pp_sep:pp_print_cut)
    ipr_mset

(** Goal-directed searching functions. *)

let is_persistent ipr =
  let global_pr_set, _ = !global_state in
  PropSet.mem (iPersistent ipr) global_pr_set

let strengthen_persistent =
  IpropMset.map
  (fun ipr count ->
    if Multiplicity.is_finite count && is_persistent ipr then
      Multiplicity.inf
    else count)

let initial ins =
  let symbol_table, facts, laws, atoms = Ast.validate ins in
  global_state := (prop_list_to_internal facts, iprop_list_to_internal laws);
  (PropSet.empty, strengthen_persistent (iprop_list_to_internal atoms))

let visited : state -> bool =
  let state_list = ref [] in
  let visited_aux ((pr_set, ipr_mset) as st) =
    if
      List.exists
        (fun (pr_set', ipr_mset') ->
          PropSet.subset pr_set pr_set' && IpropMset.subset ipr_mset ipr_mset')
        !state_list
    then (
      record_duplication ();
      true)
    else (
      state_list := st :: !state_list;
      false)
  in
  visited_aux

let successors (pr_set, ipr_mset) =
  let global_pr_set, global_ipr_mset = !global_state in
  let new_state_list =
    List.filter_map
      (fun (ipr, count) ->
        match ipr with
        | IWand (ipr1, ipr2) ->
            let prems =
              match ipr1 with
              | IStar ipr_set -> ipr_set
              | _ -> IpropMset.singleton ipr1 Multiplicity.one
            in
            let concls =
              match ipr2 with
              | IStar ipr_set -> ipr_set
              | _ -> IpropMset.singleton ipr2 Multiplicity.one
            in
            if IpropMset.subset prems ipr_mset then
              let strengthened_concls =
                strengthen_persistent concls
              in
              let new_ipr_mset =
                IpropMset.union strengthened_concls
                  (IpropMset.diff ipr_mset prems)
              in
              let new_st = (pr_set, new_ipr_mset) in
              if visited new_st then None else Some new_st
            else None
        | _ -> None)
      (IpropMset.to_list ipr_mset @ IpropMset.to_list global_ipr_mset)
  in
  List.iter (fun st -> record_state (state_size st)) new_state_list;
  new_state_list

let terminate (_, ipr_mset) = IpropMset.mem iFalse ipr_mset
let estimate = fun _ -> 0
