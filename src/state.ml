open Format
open Hashcons
open Ast
open Multiset

(** Definition for hash-consed, normalized iprops and multisets holding them as
    elements. *)

module rec Niprop : sig
  type niprop =
    | IFalse
    | IAtom of string
    | IStar of M.t
    | IWand of niprop * niprop
end = struct
  type niprop =
    | IFalse
    | IAtom of string
    | IStar of M.t
    | IWand of niprop * niprop
end

and HashedOrderedNiprop : (HashedOrderedType with type t = Niprop.niprop) =
struct
  type t = Niprop.niprop

  let compare = Stdlib.compare
  let hash = Hashtbl.hash
end

and M : (Multiset with type elt = Niprop.niprop) =
  Multiset.Make (HashedOrderedNiprop)

open Niprop

let iFalse = IFalse
let iAtom str = IAtom str
let iStar set = IStar set
let iWand (nipr1, nipr2) = IWand (nipr1, nipr2)

type state = M.t

let rec pp_niprop fmt nipr =
  match nipr with
  | IFalse -> fprintf fmt "⊥"
  | IAtom str -> fprintf fmt "%s" str
  | IStar set ->
      fprintf fmt "(%a)"
        (pp_niprop_multiset ~pp_sep:(fun fmt () -> fprintf fmt " * "))
        set
  | IWand (ipr1, ipr2) -> fprintf fmt "(%a -* %a)" pp_niprop ipr1 pp_niprop ipr2

and pp_niprop_multiset ~pp_sep fmt set =
  pp_print_list ~pp_sep
    (fun fmt (nipr, cnt) ->
      pp_print_list ~pp_sep pp_niprop fmt (List.init cnt (fun _ -> nipr)))
    fmt (M.to_list set)

let pp_state = pp_niprop_multiset ~pp_sep:pp_print_newline

(** Functions used for transition between states. *)

(** Invariant: there is no "top-level" Star in the output. *)
let rec divide_star = function
  | Star (ipr1, ipr2) -> List.concat [ divide_star ipr1; divide_star ipr2 ]
  | _ as ipr -> [ ipr ]

let list_group equal l =
  let rec list_group_aux current_opt count = function
    | [] -> (
        match current_opt with
        | Some current -> [ (current, count) ]
        | None -> [])
    | e :: l -> (
        match current_opt with
        | Some current ->
            if equal current e then list_group_aux current_opt (count + 1) l
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
      let sorted_niprl = List.sort Stdlib.compare niprl in
      let grouped_niprl = list_group ( == ) sorted_niprl in
      iStar (M.of_list grouped_niprl)

let init ins =
  List.concat_map divide_star ins
  |> List.map ipr2nipr |> List.sort Stdlib.compare |> list_group ( == ) |> M.of_list

let visited : state -> bool =
  let state_list = ref [] in
  let visited_aux (st : state) : bool =
    if List.exists (M.subset st) !state_list then true
    else (
      state_list := st :: !state_list;
      false)
  in
  visited_aux

let succ st =
  List.filter_map
    (fun (nipr, _) ->
      match nipr with
      | IWand (nipr1, nipr2) ->
          let prems =
            match nipr1 with
            | IStar set -> set
            | _ as s -> M.singleton nipr1
          in
          let concls =
            match nipr2 with
            | IStar set -> set
            | _ as s -> M.singleton nipr2
          in
          if M.subset prems st then
            let new_st = M.union concls (M.diff st (M.add nipr prems)) in
            if visited new_st then None else Some new_st
          else None
      | _ -> None)
    (M.to_list st)

let terminate st = M.mem iFalse st
