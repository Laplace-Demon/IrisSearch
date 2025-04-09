open Format
open Ast
open Internal

(** Definition of state and its operations. *)

let facts : internal_prop_set ref = ref PropSet.empty

type state = internal_iprop_multiset

let pp_state fmt st =
  fprintf fmt "%a@." (pp_internal_iprop_multiset ~pp_sep:pp_print_newline) st

let initial ins =
  let symbol_table, prop_list, iprop_list = validate ins in
  facts := prop_list_to_internal prop_list;
  iprop_list_to_internal iprop_list

let visited : state -> bool =
  let state_list = ref [] in
  let visited_aux st =
    if List.exists (IpropMset.subset st) !state_list then true
    else (
      state_list := st :: !state_list;
      false)
  in
  visited_aux

let successors ipr_mset =
  let from_facts =
    List.filter_map
      (fun pr ->
        match pr with
        | IPersistent ipr ->
            if IpropMset.mem ipr ipr_mset then
              Some (IpropMset.add ipr Infinite ipr_mset)
            else None
        | _ -> None)
      (PropSet.to_list !facts)
  in
  let from_laws =
    List.filter_map
      (fun (ipr, _) ->
        match ipr with
        | IWand (ipr1, ipr2) ->
            let prems =
              match ipr1 with
              | IStar ipr_set -> ipr_set
              | _ -> IpropMset.singleton ipr1 (Finite 1)
            in
            let concls =
              match ipr2 with
              | IStar ipr_set -> ipr_set
              | _ -> IpropMset.singleton ipr2 (Finite 1)
            in
            if IpropMset.subset prems ipr_mset then
              let new_atoms =
                IpropMset.union concls (IpropMset.diff ipr_mset prems)
              in
              let new_st = new_atoms in
              if visited new_st then None else Some new_st
            else None
        | _ -> None)
      (IpropMset.to_list ipr_mset)
  in
  from_facts @ from_laws

let terminate st = IpropMset.mem iFalse st
let estimate = fun _ -> 0
