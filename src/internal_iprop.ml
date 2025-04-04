open Format
open Ast
open Multiset

(** Definition of hash-consed, internal representation of iprops and multisets
    holding them. *)

module rec InternalIprop : sig
  type internal_iprop =
    | IFalse
    | IAtom of string
    | IStar of M.t
    | IWand of internal_iprop * internal_iprop
    | IBox of internal_iprop
end = struct
  type internal_iprop =
    | IFalse
    | IAtom of string
    | IStar of M.t
    | IWand of internal_iprop * internal_iprop
    | IBox of internal_iprop
end

and HashedOrderedInternalIprop :
  (HashedOrderedType with type t = InternalIprop.internal_iprop) = struct
  type t = InternalIprop.internal_iprop

  let compare = Stdlib.compare
  let hash = Hashtbl.hash
end

and M : (Multiset with type elt = InternalIprop.internal_iprop) =
  Multiset.Make (HashedOrderedInternalIprop)

include InternalIprop

type internal_iprop_multiset = M.t

let rec pp_internal_iprop fmt = function
  | IFalse -> fprintf fmt "⊥"
  | IAtom str -> fprintf fmt "%s" str
  | IStar ipr_set ->
      fprintf fmt "(%a)"
        (pp_internal_iprop_multiset ~pp_sep:(fun fmt () -> fprintf fmt " * "))
        ipr_set
  | IWand (ipr1, ipr2) ->
      fprintf fmt "(%a -* %a)" pp_internal_iprop ipr1 pp_internal_iprop ipr2
  | IBox ipr -> fprintf fmt "□ %a" pp_internal_iprop ipr

and pp_internal_iprop_multiset ?(pp_sep = pp_print_cut) fmt ipr_set =
  pp_print_list ~pp_sep
    (fun fmt (ipr, cnt) ->
      pp_print_list ~pp_sep pp_internal_iprop fmt (List.init cnt (fun _ -> ipr)))
    fmt (M.to_list ipr_set)

(** Smart constructors of internal_iprop required for hash-consing. *)

let iFalse = IFalse
let iAtom str = IAtom str
let iStar ipr_set = IStar ipr_set
let iWand (ipr1, ipr2) = IWand (ipr1, ipr2)
let iBox ipr = IBox ipr

(** Functions converting iprop to internal_iprop. *)

let rec divide_star = function
  | Star (ipr1, ipr2) -> List.concat [ divide_star ipr1; divide_star ipr2 ]
  | _ as ipr -> [ ipr ]

let list_group compare equal l =
  let rec group_sorted current_opt count = function
    | [] -> (
        match current_opt with
        | Some current ->
            printf "%i" count;
            [ (current, count) ]
        | None -> [])
    | e :: l -> (
        match current_opt with
        | Some current ->
            if equal current e then group_sorted current_opt (count + 1) l
            else (current, count) :: group_sorted (Some e) 1 l
        | None -> group_sorted (Some e) 1 l)
  in
  List.sort compare l |> group_sorted None 0

(** Should use physical equality when hash-consing is implemented. *)

let rec iprop_to_internal : iprop -> internal_iprop = function
  | False -> iFalse
  | Atom str -> iAtom str
  | Star _ as ipr ->
      let open HashedOrderedInternalIprop in
      ipr |> divide_star |> List.map iprop_to_internal
      |> list_group compare ( = ) |> M.of_list |> iStar
  | Wand (ipr1, ipr2) -> iWand (iprop_to_internal ipr1, iprop_to_internal ipr2)
  | Box ipr -> iBox (iprop_to_internal ipr)

let iprop_list_to_internal : iprop list -> internal_iprop_multiset =
 fun iprl ->
  let open HashedOrderedInternalIprop in
  iprl |> List.map divide_star |> List.concat |> List.map iprop_to_internal
  |> list_group compare ( = ) |> M.of_list
