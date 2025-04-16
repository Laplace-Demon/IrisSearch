open Format
open Ast

(** Definition of hash-consed, internal representation of propositions and sets
    holding them. *)

module rec Internal : sig
  type internal_term = IVar of string

  type internal_prop =
    | IPersistent of internal_iprop
    | INot of internal_prop
    | IAnd of PropSet.t
    | IOr of internal_prop * internal_prop
    | IImply of internal_prop * internal_prop
    | IPred of string * internal_term list
    | IEq of internal_term * internal_term
    | INeq of internal_term * internal_term

  and internal_iprop =
    | IFalse
    | IAtom of string
    | IStar of IpropMset.t
    | IWand of internal_iprop * internal_iprop
    | IPure of internal_prop
    | IHPred of string * internal_term list

  val compare_internal_term : internal_term -> internal_term -> int
  val compare_internal_prop : internal_prop -> internal_prop -> int
  val compare_internal_iprop : internal_iprop -> internal_iprop -> int
  val hash_internal_term : internal_term -> int
  val hash_internal_prop : internal_prop -> int
  val hash_internal_iprop : internal_iprop -> int
end = struct
  type internal_term = IVar of string

  type internal_prop =
    | IPersistent of internal_iprop
    | INot of internal_prop
    | IAnd of PropSet.t
    | IOr of internal_prop * internal_prop
    | IImply of internal_prop * internal_prop
    | IPred of string * internal_term list
    | IEq of internal_term * internal_term
    | INeq of internal_term * internal_term

  and internal_iprop =
    | IFalse
    | IAtom of string
    | IStar of IpropMset.t
    | IWand of internal_iprop * internal_iprop
    | IPure of internal_prop
    | IHPred of string * internal_term list

  let compare_internal_term tm1 tm2 =
    match (tm1, tm2) with IVar str1, IVar str2 -> String.compare str1 str2

  let rec compare_internal_prop pr1 pr2 =
    match (pr1, pr2) with
    | IPersistent ipr1, IPersistent ipr2 -> compare_internal_iprop ipr1 ipr2
    | INot pr1, INot pr2 -> compare_internal_prop pr1 pr2
    | IAnd pr_set1, IAnd pr_set2 -> PropSet.compare pr_set1 pr_set2
    | IOr (pr11, pr12), IOr (pr21, pr22)
    | IImply (pr11, pr12), IImply (pr21, pr22) ->
        let tmp = compare_internal_prop pr11 pr21 in
        if tmp = 0 then compare_internal_prop pr12 pr22 else tmp
    | IPred (str1, param_list1), IPred (str2, param_list2) ->
        let tmp = String.compare str1 str2 in
        if tmp = 0 then
          List.compare compare_internal_term param_list1 param_list2
        else tmp
    | IEq (tm11, tm12), IEq (tm21, tm22) | INeq (tm11, tm12), INeq (tm21, tm22)
      ->
        let tmp = compare_internal_term tm11 tm21 in
        if tmp = 0 then compare_internal_term tm12 tm22 else tmp
    | _, _ -> Stdlib.compare pr1 pr2

  and compare_internal_iprop ipr1 ipr2 =
    match (ipr1, ipr2) with
    | IFalse, IFalse -> 0
    | IAtom str1, IAtom str2 -> String.compare str1 str2
    | IStar ipr_mset1, IStar ipr_mset2 -> IpropMset.compare ipr_mset1 ipr_mset2
    | IWand (ipr11, ipr12), IWand (ipr21, ipr22) ->
        let tmp = compare_internal_iprop ipr11 ipr21 in
        if tmp = 0 then compare_internal_iprop ipr12 ipr22 else tmp
    | IHPred (str1, param_list1), IHPred (str2, param_list2) ->
        let tmp = String.compare str1 str2 in
        if tmp = 0 then
          List.compare compare_internal_term param_list1 param_list2
        else tmp
    | _, _ -> Stdlib.compare ipr1 ipr2

  let hash_internal_term = Hashtbl.hash
  let hash_internal_prop = Hashtbl.hash
  let hash_internal_iprop = Hashtbl.hash
end

and HashedOrderedInternalProp :
  (Set.HashedOrderedType with type t = Internal.internal_prop) = struct
  type t = Internal.internal_prop

  let compare = Internal.compare_internal_prop
  let hash = Internal.hash_internal_prop
end

and HashedOrderedInternalIprop :
  (Multiset.HashedOrderedType with type t = Internal.internal_iprop) = struct
  type t = Internal.internal_iprop

  let compare = Internal.compare_internal_iprop
  let hash = Internal.hash_internal_iprop
end

and PropSet : (Set.Set with type elt = Internal.internal_prop) =
  Set.Make (HashedOrderedInternalProp)

and IpropMset : (Multiset.Multiset with type elt = Internal.internal_iprop) =
  Multiset.Make (HashedOrderedInternalIprop)

include Internal

type internal_prop_set = PropSet.t
type internal_iprop_multiset = IpropMset.t

let pp_internal_term fmt = function IVar str -> fprintf fmt "%s" str

let rec pp_internal_prop fmt = function
  | IPersistent ipr -> fprintf fmt "Persistent %a" pp_internal_iprop ipr
  | INot pr -> fprintf fmt "¬ %a" pp_internal_prop pr
  | IAnd pr_set ->
      fprintf fmt "(%a)"
        (pp_internal_prop_set ~pp_sep:(fun fmt () -> fprintf fmt " ∧ "))
        pr_set
  | IOr (pr1, pr2) ->
      fprintf fmt "(%a ∨ %a)" pp_internal_prop pr1 pp_internal_prop pr2
  | IImply (pr1, pr2) ->
      fprintf fmt "(%a → %a)" pp_internal_prop pr1 pp_internal_prop pr2
  | IPred (str, param_list) ->
      fprintf fmt "%s %a" str
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_internal_term)
        param_list
  | IEq (tm1, tm2) ->
      fprintf fmt "%a = %a" pp_internal_term tm1 pp_internal_term tm2
  | INeq (tm1, tm2) ->
      fprintf fmt "%a ≠ %a" pp_internal_term tm1 pp_internal_term tm2

and pp_internal_iprop fmt = function
  | IFalse -> fprintf fmt "⊥"
  | IAtom str -> fprintf fmt "%s" str
  | IStar ipr_mset ->
      fprintf fmt "(%a)"
        (pp_internal_iprop_multiset ~pp_sep:(fun fmt () -> fprintf fmt " * "))
        ipr_mset
  | IWand (ipr1, ipr2) ->
      fprintf fmt "(%a -* %a)" pp_internal_iprop ipr1 pp_internal_iprop ipr2
  | IPure pr -> fprintf fmt "⌜%a⌝" pp_internal_prop pr
  | IHPred (str, param_list) ->
      fprintf fmt "%s %a" str
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_internal_term)
        param_list

and pp_internal_prop_set ?(pp_sep = pp_print_cut) fmt pr_set =
  if PropSet.is_empty pr_set then pp_print_string fmt "%empty"
  else pp_print_list ~pp_sep pp_internal_prop fmt (PropSet.to_list pr_set)

and pp_internal_iprop_multiset ?(pp_sep = pp_print_cut) fmt ipr_mset =
  if IpropMset.is_empty ipr_mset then pp_print_string fmt "%empty"
  else
    pp_print_list ~pp_sep
      (fun fmt (ipr, count) ->
        if Multiplicity.is_finite count then
          pp_print_seq ~pp_sep pp_internal_iprop fmt
            (Seq.init (Multiplicity.to_int count) (fun _ -> ipr))
        else fprintf fmt "□ %a" pp_internal_iprop ipr)
      fmt
      (IpropMset.to_list ipr_mset)

(** Smart constructors required for hash-consing. *)

let iVar str = IVar str
let iPersistent ipr = IPersistent ipr
let iNot pr = INot pr
let iAnd pr_set = IAnd pr_set
let iOr (pr1, pr2) = IOr (pr1, pr2)
let iImply (ipr1, ipr2) = IImply (ipr1, ipr2)
let iPred (str, param_list) = IPred (str, param_list)
let iEq (tm1, tm2) = IEq (tm1, tm2)
let iNeq (tm1, tm2) = INeq (tm1, tm2)
let iFalse = IFalse
let iAtom str = IAtom str
let iStar ipr_mset = IStar ipr_mset
let iWand (ipr1, ipr2) = IWand (ipr1, ipr2)
let iPure pr = IPure pr
let iHPred (str, param_list) = IHPred (str, param_list)

(** Functions converting to internal representation. *)

let term_to_internal : term -> internal_term = function Var str -> iVar str

let rec prop_to_internal : prop -> internal_prop = function
  | Persistent ipr -> iPersistent (iprop_to_internal ipr)
  | Not pr -> iNot (prop_to_internal pr)
  | And (pr1, pr2) ->
      let pr_set1 =
        match prop_to_internal pr1 with
        | IAnd pr_set -> pr_set
        | _ as pr -> PropSet.singleton pr
      in
      let pr_set2 =
        match prop_to_internal pr2 with
        | IAnd pr_set -> pr_set
        | _ as pr -> PropSet.singleton pr
      in
      iAnd (PropSet.union pr_set1 pr_set2)
  | Or (pr1, pr2) -> iOr (prop_to_internal pr1, prop_to_internal pr2)
  | Imply (pr1, pr2) -> iImply (prop_to_internal pr1, prop_to_internal pr2)
  | Pred (str, param_list) -> iPred (str, List.map term_to_internal param_list)
  | Eq (tm1, tm2) -> iEq (term_to_internal tm1, term_to_internal tm2)
  | Neq (tm1, tm2) -> iNeq (term_to_internal tm1, term_to_internal tm2)

and iprop_to_internal : iprop -> internal_iprop = function
  | False -> iFalse
  | Atom str -> iAtom str
  | Star (ipr1, ipr2) ->
      let ipr_mset1 =
        match iprop_to_internal ipr1 with
        | IStar ipr_mset -> ipr_mset
        | _ as ipr -> IpropMset.singleton ipr Multiplicity.one
      in
      let ipr_mset2 =
        match iprop_to_internal ipr2 with
        | IStar ipr_mset -> ipr_mset
        | _ as ipr -> IpropMset.singleton ipr Multiplicity.one
      in
      iStar (IpropMset.union ipr_mset1 ipr_mset2)
  | Wand (ipr1, ipr2) -> iWand (iprop_to_internal ipr1, iprop_to_internal ipr2)
  | Box ipr -> (
      match iprop_to_internal ipr with
      | IStar ipr_mset ->
          iStar (IpropMset.map (fun _ _ -> Multiplicity.inf) ipr_mset)
      | _ as ipr -> iStar (IpropMset.singleton ipr Multiplicity.inf))
  | Pure pr -> iPure (prop_to_internal pr)
  | HPred (str, param_list) -> iHPred (str, List.map term_to_internal param_list)

let prop_list_to_internal : prop list -> internal_prop_set =
 fun prl ->
  prl |> List.map prop_to_internal
  |> List.fold_left
       (fun acc pr ->
         PropSet.union acc
           (match pr with IAnd pr_set -> pr_set | _ -> PropSet.singleton pr))
       PropSet.empty

let iprop_list_to_internal : iprop list -> internal_iprop_multiset =
 fun iprl ->
  iprl |> List.map iprop_to_internal
  |> List.fold_left
       (fun acc ipr ->
         IpropMset.union acc
           (match ipr with
           | IStar ipr_mset -> ipr_mset
           | _ -> IpropMset.singleton ipr Multiplicity.one))
       IpropMset.empty
