open Format
open Hashcons
open Ast
open Multiset

(** Definition for hash-consed, normalized iprops and multisets holding them as
    elements. *)

let compare_hc x y = compare x.tag y.tag
let hash_hc x = x.hkey

module rec Niprop : sig
  type niprop = niprop_node hash_consed

  and niprop_node =
    | IFalse
    | IAtom of string
    | IStar of MultisetInner.t
    | IWand of niprop * niprop
end = struct
  type niprop = niprop_node hash_consed

  and niprop_node =
    | IFalse
    | IAtom of string
    | IStar of MultisetInner.t
    | IWand of niprop * niprop
end

and HashedOrderedNiprop : HashedOrderedType with type t = Niprop.niprop = struct
  type t = Niprop.niprop

  let compare = compare_hc
  let hash = hash_hc
end

and M : sig
  module L : Multiset with type elt = Niprop.niprop
  module T : Multiset with type elt = Niprop.niprop

  val l2t : L.t -> T.t
  val t2l : T.t -> L.t
end =
  Multiset.Make (HashedOrderedNiprop)

and MultisetOuter : (Multiset with type elt = Niprop.niprop and type t = M.T.t) = M.T
and MultisetInner : (Multiset with type elt = Niprop.niprop and type t = M.L.t) = M.L

open Niprop

module HashedNipropNode : HashedType with type t = niprop_node = struct
  type t = niprop_node

  let equal nipr1 nipr2 =
    match (nipr1, nipr2) with
    | IFalse, IFalse -> true
    | IAtom str1, IAtom str2 -> String.equal str1 str2
    | IStar set1, IStar set2 -> MultisetInner.equal set1 set2
    | IWand (nipr11, nipr12), IWand (nipr21, nipr22) ->
        nipr11 == nipr21 && nipr12 == nipr22
    | _, _ -> false

  let hash = function
    | IFalse -> Hashtbl.hash 0
    | IAtom str -> Hashtbl.hash (1, str)
    | IStar set -> Hashtbl.hash (2, MultisetInner.hash set)
    | IWand (nipr1, nipr2) -> Hashtbl.hash (3, nipr1.hkey, nipr2.hkey)
end

module Niprop_hc = Hashcons.Make (HashedNipropNode)

let niprop_table = Niprop_hc.create 251
let iFalse = Niprop_hc.hashcons niprop_table IFalse
let iAtom str = Niprop_hc.hashcons niprop_table (IAtom str)
let iStar set = Niprop_hc.hashcons niprop_table (IStar set)

let iWand (nipr1, nipr2) =
  Niprop_hc.hashcons niprop_table (IWand (nipr1, nipr2))

type state = MultisetOuter.t

let rec pp_niprop fmt nipr =
  match nipr.node with
  | IFalse -> fprintf fmt "⊥"
  | IAtom str -> fprintf fmt "%s" str
  | IStar set ->
      fprintf fmt "(%a)"
        (pp_niprop_multiset_inner ~pp_sep:(fun fmt () -> fprintf fmt " * "))
        set
  | IWand (ipr1, ipr2) -> fprintf fmt "(%a -* %a)" pp_niprop ipr1 pp_niprop ipr2

and pp_niprop_multiset_inner ~pp_sep fmt set =
  pp_print_list ~pp_sep
    (fun fmt (nipr, cnt) ->
      pp_print_list ~pp_sep pp_niprop fmt (List.init cnt (fun _ -> nipr)))
    fmt
    (MultisetInner.to_list set)

let pp_niprop_multiset_outer ~pp_sep fmt set =
  pp_print_list ~pp_sep
    (fun fmt (nipr, cnt) ->
      pp_print_list ~pp_sep pp_niprop fmt (List.init cnt (fun _ -> nipr)))
    fmt
    (MultisetOuter.to_list set)

let pp_state = pp_niprop_multiset_outer ~pp_sep:pp_print_newline

(** Functions used for transition between states. *)

(** Invariant: there is no "top-level" Star in the output. *)
let rec divide_star = function
  | Star (ipr1, ipr2) -> List.concat [ divide_star ipr1; divide_star ipr2 ]
  | _ as ipr -> [ ipr ]

let list_group equal l =
  let rec list_group_aux current_opt count = function
    | [] ->
      (match current_opt with
      | Some current -> [ current, count ]
      | None -> [])
    | e :: l ->
      (match current_opt with
      | Some current ->
        if equal current e
        then list_group_aux current_opt (count + 1) l
        else (current, count) :: list_group_aux (Some e) 1 l
      | None -> list_group_aux (Some e) 1 l)
  in
  list_group_aux None 0 l

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
  | iprl ->
    let niprl = List.map ipr2nipr_aux iprl in
    let sorted_niprl = List.sort compare_hc niprl in
    let grouped_niprl = list_group ( == ) sorted_niprl in
    iStar (MultisetInner.of_list grouped_niprl)

let init ins =
  List.concat_map divide_star ins |> List.map ipr2nipr
  |> List.sort compare_hc
  |> list_group ( == )
  |> MultisetOuter.of_list

let visited : state -> bool =
  let state_list = ref [] in
  let visited_aux (st : state) : bool =
    if List.exists (MultisetOuter.subset st) !state_list then true
    else (
      state_list := st :: !state_list;
      false)
  in
  visited_aux

let succ st =
  List.filter_map
    (fun (nipr, _) ->
      match nipr.node with
      | IWand (nipr1, nipr2) ->
          let prems =
            match nipr1.node with
            | IStar set -> M.l2t set
            | _ as s -> MultisetOuter.singleton nipr1
          in
          let concls =
            match nipr2.node with
            | IStar set -> M.l2t set
            | _ as s -> MultisetOuter.singleton nipr2
          in
          if MultisetOuter.subset prems st then
            let new_st =
              MultisetOuter.union concls
                (MultisetOuter.diff st (MultisetOuter.add nipr prems))
            in
            if visited new_st then None else Some new_st
          else None
      | _ -> None)
    (MultisetOuter.to_list st)

let terminate st = MultisetOuter.mem iFalse st
