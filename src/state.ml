open Format
open Ast
open Internal_iprop

(** Definition of intermediate searching state and its operations. *)

type state = internal_iprop_multiset * internal_iprop_multiset

let pp_state_laws fmt (laws, _) =
  fprintf fmt "@[<v 2>Laws:@,%a@]@."
    (pp_internal_iprop_multiset ~pp_sep:pp_print_cut)
    laws

let pp_state_atoms fmt (_, atoms) =
  fprintf fmt "@[<v 2>Atoms:@,%a@]@."
    (pp_internal_iprop_multiset ~pp_sep:pp_print_cut)
    atoms

let pp_state fmt st = fprintf fmt "%a%a@." pp_state_laws st pp_state_atoms st

let initial ipr_list =
  ipr_list |> iprop_list_to_internal
  |> M.partition (fun ipr _ -> match ipr with IBox _ -> true | _ -> false)

let visited : state -> bool =
  let state_list = ref [] in
  let visited_aux ((_, atoms) : state) : bool =
    if List.exists (M.subset atoms) !state_list then true
    else (
      state_list := atoms :: !state_list;
      false)
  in
  visited_aux

let successors (laws, atoms) =
  List.filter_map
    (fun (law, _) ->
      match law with
      | IBox (IWand (ipr1, ipr2)) ->
          let prems =
            match ipr1 with IStar ipr_set -> ipr_set | _ -> M.singleton ipr1
          in
          let concls =
            match ipr2 with IStar ipr_set -> ipr_set | _ -> M.singleton ipr2
          in
          if M.subset prems atoms then
            let new_atoms = M.union concls (M.diff atoms prems) in
            let new_st = (laws, new_atoms) in
            if visited new_st then None else Some new_st
          else None
      | _ -> None)
    (M.to_list laws)

let terminate (_, atoms) = M.mem iFalse atoms
