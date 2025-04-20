open Format
open Ast
open Type

(** Definition of string interning modules. Term variables, predicates, heap
    predicates, atoms are distinguished at the type level. *)

module VarId = Interned_string.Make ()
module PredId = Interned_string.Make ()
module HPredId = Interned_string.Make ()
module AtomId = Interned_string.Make ()

type var_id = VarId.t
type pred_id = PredId.t
type hpred_id = HPredId.t
type atom_id = AtomId.t

(** Definition of hash-consed, internal representation of propositions and sets
    holding them. *)

module rec Internal : sig
  (** De bruijn shift and binder names. This record satisfies the invariant:
      shift = List.length name_list *)

  type binder_info = { shift : int; typed_str_list : (string * itype) list }
  type internal_term = IVar of var_id

  type internal_prop =
    | IPersistent of internal_iprop
    | INot of internal_prop
    | IAnd of PropSet.t
    | IOr of internal_prop * internal_prop
    | IImply of internal_prop * internal_prop
    | IPred of pred_id * internal_term list
    | IForall of binder_info * internal_prop
    | IEq of internal_term * internal_term
    | INeq of internal_term * internal_term

  and internal_iprop =
    | IFalse
    | IAtom of atom_id
    | IStar of IpropMset.t
    | IWand of internal_iprop * internal_iprop
    | IPure of internal_prop
    | IHPred of hpred_id * internal_term list
    | IHForall of binder_info * internal_iprop

  val compare_internal_term : internal_term -> internal_term -> int
  val compare_internal_prop : internal_prop -> internal_prop -> int
  val compare_internal_iprop : internal_iprop -> internal_iprop -> int
  val hash_internal_term : internal_term -> int
  val hash_internal_prop : internal_prop -> int
  val hash_internal_iprop : internal_iprop -> int
end = struct
  type binder_info = { shift : int; typed_str_list : (string * itype) list }
  type internal_term = IVar of var_id

  type internal_prop =
    | IPersistent of internal_iprop
    | INot of internal_prop
    | IAnd of PropSet.t
    | IOr of internal_prop * internal_prop
    | IImply of internal_prop * internal_prop
    | IPred of pred_id * internal_term list
    | IForall of binder_info * internal_prop
    | IEq of internal_term * internal_term
    | INeq of internal_term * internal_term

  and internal_iprop =
    | IFalse
    | IAtom of atom_id
    | IStar of IpropMset.t
    | IWand of internal_iprop * internal_iprop
    | IPure of internal_prop
    | IHPred of hpred_id * internal_term list
    | IHForall of binder_info * internal_iprop

  let compare_internal_term tm1 tm2 =
    match (tm1, tm2) with IVar var1, IVar var2 -> VarId.compare var1 var2

  let rec compare_internal_prop pr1 pr2 =
    match (pr1, pr2) with
    | IPersistent ipr1, IPersistent ipr2 -> compare_internal_iprop ipr1 ipr2
    | INot pr1, INot pr2 -> compare_internal_prop pr1 pr2
    | IAnd pr_set1, IAnd pr_set2 -> PropSet.compare pr_set1 pr_set2
    | IOr (pr11, pr12), IOr (pr21, pr22)
    | IImply (pr11, pr12), IImply (pr21, pr22) ->
        let tmp = compare_internal_prop pr11 pr21 in
        if tmp = 0 then compare_internal_prop pr12 pr22 else tmp
    | IPred (pred1, param_list1), IPred (pred2, param_list2) ->
        let tmp = PredId.compare pred1 pred2 in
        if tmp = 0 then
          List.compare compare_internal_term param_list1 param_list2
        else tmp
    | IForall ({ shift = shift1 }, pr1), IForall ({ shift = shift2 }, pr2) ->
        let tmp = Int.compare shift1 shift2 in
        if tmp = 0 then compare_internal_prop pr1 pr2 else tmp
    | IEq (tm11, tm12), IEq (tm21, tm22) | INeq (tm11, tm12), INeq (tm21, tm22)
      ->
        let tmp = compare_internal_term tm11 tm21 in
        if tmp = 0 then compare_internal_term tm12 tm22 else tmp
    | _, _ -> Stdlib.compare pr1 pr2

  and compare_internal_iprop ipr1 ipr2 =
    match (ipr1, ipr2) with
    | IFalse, IFalse -> 0
    | IAtom atom1, IAtom atom2 -> AtomId.compare atom1 atom2
    | IStar ipr_mset1, IStar ipr_mset2 -> IpropMset.compare ipr_mset1 ipr_mset2
    | IWand (ipr11, ipr12), IWand (ipr21, ipr22) ->
        let tmp = compare_internal_iprop ipr11 ipr21 in
        if tmp = 0 then compare_internal_iprop ipr12 ipr22 else tmp
    | IHPred (hpred1, param_list1), IHPred (hpred2, param_list2) ->
        let tmp = HPredId.compare hpred1 hpred2 in
        if tmp = 0 then
          List.compare compare_internal_term param_list1 param_list2
        else tmp
    | IHForall ({ shift = shift1 }, ipr1), IHForall ({ shift = shift2 }, ipr2)
      ->
        let tmp = Int.compare shift1 shift2 in
        if tmp = 0 then compare_internal_iprop ipr1 ipr2 else tmp
    | _, _ -> Stdlib.compare ipr1 ipr2

  (* The addition of binder_info now makes using Hashtbl.hash incorrect,
      because we should ignore the typed_str_list field. *)
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

let extract_de_Bruijn_index var = int_of_string_opt var

let ( pp_internal_term,
      pp_internal_prop,
      pp_internal_iprop,
      pp_internal_prop_set,
      pp_internal_iprop_multiset ) =
  let pp_internal_term ?(env = []) fmt = function
    | IVar var -> (
        let var = VarId.export var in
        match extract_de_Bruijn_index var with
        | Some i -> fprintf fmt "%s" (List.nth env i)
        | None -> fprintf fmt "%s" var)
  in
  let rec pp_internal_prop ?(env = []) fmt = function
    | IPersistent ipr ->
        fprintf fmt "Persistent %a" (pp_internal_iprop ~env) ipr
    | INot pr -> fprintf fmt "¬ %a" (pp_internal_prop ~env) pr
    | IAnd pr_set ->
        fprintf fmt "(%a)"
          (pp_internal_prop_set ~pp_sep:(fun fmt () -> fprintf fmt " ∧ ") ~env)
          pr_set
    | IOr (pr1, pr2) ->
        fprintf fmt "(%a ∨ %a)" (pp_internal_prop ~env) pr1
          (pp_internal_prop ~env) pr2
    | IImply (pr1, pr2) ->
        fprintf fmt "(%a → %a)" (pp_internal_prop ~env) pr1
          (pp_internal_prop ~env) pr2
    | IPred (pred, param_list) ->
        fprintf fmt "%s %a" (PredId.export pred)
          (pp_print_list
             ~pp_sep:(fun fmt () -> fprintf fmt " ")
             (pp_internal_term ~env))
          param_list
    | IForall ({ typed_str_list }, pr) ->
        fprintf fmt "forall %a, %a"
          (pp_typed_strs_list
             ~pp_sep:(fun fmt () -> fprintf fmt " ")
             ~pp_paren:true ())
          (group_typed_str typed_str_list)
          (pp_internal_prop
             ~env:
               (List.fold_left
                  (fun acc (str, _) -> str :: acc)
                  env typed_str_list))
          pr
    | IEq (tm1, tm2) ->
        fprintf fmt "%a = %a" (pp_internal_term ~env) tm1
          (pp_internal_term ~env) tm2
    | INeq (tm1, tm2) ->
        fprintf fmt "%a ≠ %a" (pp_internal_term ~env) tm1
          (pp_internal_term ~env) tm2
  and pp_internal_iprop ?(env = []) fmt = function
    | IFalse -> fprintf fmt "⊥"
    | IAtom atom -> fprintf fmt "%s" (AtomId.export atom)
    | IStar ipr_mset ->
        fprintf fmt "(%a)"
          (pp_internal_iprop_multiset
             ~pp_sep:(fun fmt () -> fprintf fmt " * ")
             ~env)
          ipr_mset
    | IWand (ipr1, ipr2) ->
        fprintf fmt "(%a -* %a)" (pp_internal_iprop ~env) ipr1
          (pp_internal_iprop ~env) ipr2
    | IPure pr -> fprintf fmt "⌜ %a ⌝" (pp_internal_prop ~env) pr
    | IHPred (hpred, param_list) ->
        fprintf fmt "%s %a" (HPredId.export hpred)
          (pp_print_list
             ~pp_sep:(fun fmt () -> fprintf fmt " ")
             (pp_internal_term ~env))
          param_list
    | IHForall ({ typed_str_list }, ipr) ->
        fprintf fmt "forall %a, %a"
          (pp_typed_strs_list
             ~pp_sep:(fun fmt () -> fprintf fmt " ")
             ~pp_paren:true ())
          (group_typed_str typed_str_list)
          (pp_internal_iprop
             ~env:
               (List.fold_left
                  (fun acc (str, _) -> str :: acc)
                  env typed_str_list))
          ipr
  and pp_internal_prop_set ?(pp_sep = pp_print_cut) ?(env = []) fmt pr_set =
    if PropSet.is_empty pr_set then pp_print_string fmt "%empty"
    else
      pp_print_list ~pp_sep (pp_internal_prop ~env) fmt (PropSet.to_list pr_set)
  and pp_internal_iprop_multiset ?(pp_sep = pp_print_cut) ?(env = []) fmt
      ipr_mset =
    if IpropMset.is_empty ipr_mset then pp_print_string fmt "%empty"
    else
      pp_print_list ~pp_sep
        (fun fmt (ipr, count) ->
          if Multiplicity.is_finite count then
            pp_print_seq ~pp_sep pp_internal_iprop fmt
              (Seq.init (Multiplicity.to_int count) (fun _ -> ipr))
          else fprintf fmt "□ %a" (pp_internal_iprop ~env) ipr)
        fmt
        (IpropMset.to_list ipr_mset)
  in
  ( pp_internal_term ~env:[],
    pp_internal_prop ~env:[],
    pp_internal_iprop ~env:[],
    pp_internal_prop_set ~env:[],
    pp_internal_iprop_multiset ~env:[] )

(** Smart constructors required for hash-consing. *)

let iVar var = IVar (VarId.import var)
let iPersistent ipr = IPersistent ipr
let iNot pr = INot pr
let iAnd pr_set = IAnd pr_set
let iOr (pr1, pr2) = IOr (pr1, pr2)
let iImply (ipr1, ipr2) = IImply (ipr1, ipr2)
let iPred (pred, param_list) = IPred (PredId.import pred, param_list)

let iForall (typed_str_list, pr) =
  IForall ({ shift = List.length typed_str_list; typed_str_list }, pr)

let iEq (tm1, tm2) = IEq (tm1, tm2)
let iNeq (tm1, tm2) = INeq (tm1, tm2)
let iFalse = IFalse
let iAtom atom = IAtom (AtomId.import atom)
let iStar ipr_mset = IStar ipr_mset
let iWand (ipr1, ipr2) = IWand (ipr1, ipr2)
let iPure pr = IPure pr
let iHPred (hpred, param_list) = IHPred (HPredId.import hpred, param_list)

let iHForall (typed_str_list, ipr) =
  IHForall ({ shift = List.length typed_str_list; typed_str_list }, ipr)
