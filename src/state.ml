open Format
open Hashcons
open Ast

type niprop = niprop_node hash_consed

and niprop_node =
  | IFalse
  | IAtom of string
  | IStar of niprop_node Multiset.R.t
  | IWand of niprop * niprop

module Niprop_node = struct
  type t = niprop_node

  let equal nipr1 nipr2 =
    match (nipr1, nipr2) with
    | IFalse, IFalse -> true
    | IAtom str1, IAtom str2 -> String.equal str1 str2
    | IStar set1, IStar set2 -> Multiset.R.equal set1 set2
    | IWand (nipr11, nipr12), IWand (nipr21, nipr22) ->
        nipr11 == nipr21 && nipr12 == nipr22
    | _, _ -> false

  let hash = function
    | IFalse -> 0
    | IAtom str -> String.hash str
    | IStar set -> Multiset.R.hash set
    | IWand (nipr1, nipr2) -> 0
end

module Niprop = Hashcons.Make (Niprop_node)

let niprop_table = Niprop.create 251
let iFalse = Niprop.hashcons niprop_table IFalse
let iAtom str = Niprop.hashcons niprop_table (IAtom str)
let iStar set = Niprop.hashcons niprop_table (IStar set)
let iWand (nipr1, nipr2) = Niprop.hashcons niprop_table (IWand (nipr1, nipr2))

type state = niprop_node Multiset.R.t

let rec pp_niprop fmt nipr =
  match nipr.node with
  | IFalse -> fprintf fmt "⊥"
  | IAtom str -> fprintf fmt "%s" str
  | IStar set ->
      fprintf fmt "(%a)"
        (pp_niprop_set ~pp_sep:(fun fmt () -> fprintf fmt " * "))
        set
  | IWand (ipr1, ipr2) -> fprintf fmt "(%a -* %a)" pp_niprop ipr1 pp_niprop ipr2

and pp_niprop_set ~pp_sep fmt set =
  pp_print_list ~pp_sep pp_niprop fmt (Multiset.R.to_list set)

let pp_state = pp_niprop_set ~pp_sep:pp_print_newline

let rec divide_star = function
  | Star (ipr1, ipr2) -> List.concat [ divide_star ipr1; divide_star ipr2 ]
  | _ as ipr -> [ ipr ]

let rec ipr2nipr ipr =
  let ipr2nipr_aux = function
    | False -> iFalse
    | Atom str -> iAtom str
    | Wand (ipr1, ipr2) -> iWand (ipr2nipr ipr1, ipr2nipr ipr2)
    | _ -> assert false
  in
  match divide_star ipr with
  | [] -> assert false
  | [ ipr ] -> ipr2nipr_aux ipr
  | iprl -> iStar (Multiset.R.of_list (List.map ipr2nipr_aux iprl))

let init ins =
  List.concat_map divide_star ins |> List.map ipr2nipr |> Multiset.R.of_list

let visited : state -> bool =
  let state_list = ref [] in
  let visited_aux (st : state) : bool =
    if List.exists (Multiset.R.subset st) !state_list then true
    else (
      state_list := st :: !state_list;
      false)
  in
  visited_aux

let transfer st =
  List.filter_map
    (fun nipr ->
      match nipr.node with
      | IWand (nipr1, nipr2) ->
          let prems =
            match nipr1.node with
            | IStar set -> set
            | _ as s -> Multiset.R.singleton nipr1
          in
          let concls =
            match nipr2.node with
            | IStar set -> set
            | _ as s -> Multiset.R.singleton nipr2
          in
          if Multiset.R.subset prems st then
            let new_st =
              Multiset.R.union concls
                (Multiset.R.diff st (Multiset.R.add nipr prems))
            in
            if visited new_st then None else Some new_st
          else None
      | _ -> None)
    (Multiset.R.to_list st)

let terminate st = Option.is_some (Multiset.R.mem iFalse st)
