open Ast
open Duplication_checker
open Internal
open Internal_operations
open State
open Type

let state_size (pr_set, ipr_mset) =
  (PropSet.cardinal pr_set, IpropMset.cardinal ipr_mset)

let initial
    { decl_types; decl_preds; decl_consts; decl_facts; decl_laws; decl_init } =
  global_state :=
    (prop_list_to_internal decl_facts, iprop_list_to_internal decl_laws);
  (PropSet.empty, iprop_list_to_internal decl_init)

let apply ipr (pr_set, ipr_mset) =
  match ipr with
  | IWand (ipr1, ipr2) -> (
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
      try
        let ipr_mset_prems_elim = IpropMset.diff ipr_mset prems in
        let new_ipr_mset = IpropMset.union concls ipr_mset_prems_elim in
        let new_st = (pr_set, new_ipr_mset) in
        if is_duplicate new_st then None else Some new_st
      with Multiplicity.Underflow -> None)
  | _ -> None

(** apply_multiple tries to apply the wand as many times as possible. *)

let apply_multiple ipr count (pr_set, ipr_mset) =
  match ipr with
  | IWand (ipr1, ipr2) -> (
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
      try
        let factor = Multiplicity.min (IpropMset.factor ipr_mset prems) count in
        let ipr_mset_prems_elim =
          IpropMset.diff_multiple factor ipr_mset prems
        in
        let new_ipr_mset =
          IpropMset.union
            (IpropMset.map
               (fun _ count -> Multiplicity.mul count factor)
               concls)
            ipr_mset_prems_elim
        in
        let new_st = (pr_set, new_ipr_mset) in
        if is_duplicate new_st then None else Some new_st
      with Multiplicity.Underflow -> None)
  | _ -> None

let successors st =
  let global_pr_set, global_ipr_mset = !global_state in
  IpropMset.fold
    (fun ipr count acc ->
      match apply_multiple ipr count st with
      | Some new_st ->
          Statistics.record_generated_state (state_size new_st);
          new_st :: acc
      | None -> acc)
    global_ipr_mset []

let terminate (_, ipr_mset) = IpropMset.mem iFalse ipr_mset
let estimate = fun _ -> 0
