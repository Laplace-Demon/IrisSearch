open Format
open Ast
open Multiset

(** Definition of hash-consed, internal representation of iprops and multisets
    holding them. *)

module rec InternalIprop : sig
  type internal_iprop =
    | IFalse
    | IAtom of string
    | IStar of Mset.t
    | IWand of internal_iprop * internal_iprop
end = struct
  type internal_iprop =
    | IFalse
    | IAtom of string
    | IStar of Mset.t
    | IWand of internal_iprop * internal_iprop
end

and HashedOrderedInternalIprop :
  (HashedOrderedType with type t = InternalIprop.internal_iprop) = struct
  open InternalIprop

  type t = internal_iprop

  let rec compare ipr1 ipr2 =
    match (ipr1, ipr2) with
    | IFalse, IFalse -> 0
    | IAtom str1, IAtom str2 -> String.compare str1 str2
    | IStar ipr_mset1, IStar ipr_mset2 -> Mset.compare ipr_mset1 ipr_mset2
    | IWand (ipr11, ipr12), IWand (ipr21, ipr22) ->
        let tmp = compare ipr11 ipr21 in
        if tmp = 0 then compare ipr12 ipr22 else tmp
    | _, _ -> Stdlib.compare ipr1 ipr2

  let hash = Hashtbl.hash
end

and Mset : (Multiset with type elt = InternalIprop.internal_iprop) =
  Multiset.Make (HashedOrderedInternalIprop)

include InternalIprop

type internal_iprop_multiset = Mset.t

let rec pp_internal_iprop fmt = function
  | IFalse -> fprintf fmt "⊥"
  | IAtom str -> fprintf fmt "%s" str
  | IStar ipr_mset ->
      fprintf fmt "(%a)"
        (pp_internal_iprop_multiset ~pp_sep:(fun fmt () -> fprintf fmt " * "))
        ipr_mset
  | IWand (ipr1, ipr2) ->
      fprintf fmt "(%a -* %a)" pp_internal_iprop ipr1 pp_internal_iprop ipr2

and pp_internal_iprop_multiset ?(pp_sep = pp_print_cut) fmt ipr_mset =
  pp_print_list ~pp_sep
    (fun fmt (ipr, cnt) ->
      let open Multiplicity in
      match cnt with
      | Finite i ->
          pp_print_list ~pp_sep pp_internal_iprop fmt
            (List.init i (fun _ -> ipr))
      | Infinite -> fprintf fmt "□ %a" pp_internal_iprop ipr)
    fmt (Mset.to_list ipr_mset)

(** Smart constructors of internal_iprop required for hash-consing. *)

let iFalse = IFalse
let iAtom str = IAtom str
let iStar ipr_mset = IStar ipr_mset
let iWand (ipr1, ipr2) = IWand (ipr1, ipr2)

(** Functions converting iprop to internal_iprop. *)

let rec iprop_to_internal : iprop -> internal_iprop = function
  | False -> iFalse
  | Atom str -> iAtom str
  | Star (ipr1, ipr2) ->
      let ipr_mset1 =
        match iprop_to_internal ipr1 with
        | IStar ipr_mset -> ipr_mset
        | _ as ipr -> Mset.singleton ipr (Finite 1)
      in
      let ipr_mset2 =
        match iprop_to_internal ipr2 with
        | IStar ipr_mset -> ipr_mset
        | _ as ipr -> Mset.singleton ipr (Finite 1)
      in
      iStar (Mset.union ipr_mset1 ipr_mset2)
  | Wand (ipr1, ipr2) -> iWand (iprop_to_internal ipr1, iprop_to_internal ipr2)
  | Box ipr -> iStar (Mset.singleton (iprop_to_internal ipr) Infinite)

let iprop_list_to_internal : iprop list -> internal_iprop_multiset =
 fun iprl ->
  iprl |> List.map iprop_to_internal
  |> List.fold_left
       (fun acc ipr ->
         Mset.union acc
           (match ipr with
           | IStar ipr_mset -> ipr_mset
           | _ -> Mset.singleton ipr (Finite 1)))
       Mset.empty
