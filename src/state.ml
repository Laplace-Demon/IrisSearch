open Format
open Ast
open Internal_iprop

(** Definition of intermediate searching state and its operations. *)

type state = internal_iprop_multiset

let pp_state fmt st =
  fprintf fmt "%a@." (pp_internal_iprop_multiset ~pp_sep:pp_print_newline) st

let initial ({ decl_consts; decl_laws; decl_init } as ins) =
  let symbol_table = validate ins in
  iprop_list_to_internal (decl_init @ decl_laws)

let visited : state -> bool =
  let state_list = ref [] in
  let visited_aux st =
    if List.exists (Mset.subset st) !state_list then true
    else (
      state_list := st :: !state_list;
      false)
  in
  visited_aux

let successors ipr_mset =
  List.filter_map
    (fun (ipr, _) ->
      match ipr with
      | IWand (ipr1, ipr2) ->
          let prems =
            match ipr1 with
            | IStar ipr_set -> ipr_set
            | _ -> Mset.singleton ipr1 (Finite 1)
          in
          let concls =
            match ipr2 with
            | IStar ipr_set -> ipr_set
            | _ -> Mset.singleton ipr2 (Finite 1)
          in
          if Mset.subset prems ipr_mset then
            let new_atoms = Mset.union concls (Mset.diff ipr_mset prems) in
            let new_st = new_atoms in
            if visited new_st then None else Some new_st
          else None
      | _ -> None)
    (Mset.to_list ipr_mset)

let terminate st = Mset.mem iFalse st
