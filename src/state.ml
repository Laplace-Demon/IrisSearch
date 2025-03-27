open Format
open Hashcons
open Ast

(* type niprop = niprop_node hash_consed
and niprop_node = private
  | IFalse
  | IAtom of string
  | IStar of niprop list
  | IWand of niprop * niprop *)

type state = iprop list

let pp_state = pp_print_list ~pp_sep:pp_print_newline pp_iprop

let rec list_delete_one (eqb : 'a -> 'a -> bool) (x : 'a) (l : 'a list) :
    'a list =
  match l with
  | [] -> []
  | x' :: l' -> if eqb x x' then l' else x' :: list_delete_one eqb x l'

let list_delete_all (eqb : 'a -> 'a -> bool) (xl : 'a list) (l : 'a list) :
    'a list =
  List.fold_right (list_delete_one eqb) xl l

let rec substate (st1 : state) (st2 : state) : bool =
  match st1 with
  | [] -> true
  | ipr :: st1' ->
      if List.exists (iprop_eqb ipr) st2 then
        substate st1' (list_delete_one iprop_eqb ipr st2)
      else false

let visited : state -> bool =
  let state_list = ref [] in
  let aux (st : state) : bool =
    if List.exists (substate st) !state_list then true
    else (
      state_list := st :: !state_list;
      true)
  in
  aux

let rec divide : iprop -> iprop list = function
  | Star (ipr1, ipr2) -> divide ipr1 @ divide ipr2
  | _ as ipr -> [ ipr ]

let init (ins : instance) : state = List.map divide ins |> List.concat

let transfer (st : state) : state list =
  List.filter_map
    (fun ipr ->
      match ipr with
      | Wand (ipr1, ipr2) ->
          let st = list_delete_one iprop_eqb ipr st in
          let premises = divide ipr1 in
          let conclusions = divide ipr2 in
          if substate premises st then
            let new_st =
              List.concat [ list_delete_all iprop_eqb premises st; conclusions ]
            in
            Some new_st
          else None
      | _ -> None)
    st

let terminate : state -> bool = substate [ False ]
