open Format
open Ast
open Type

(** Definition of string interning modules. Term variables, predicates, heap
    predicates, atoms are distinguished at the type level. *)

module VarId = Interned_string.Make ()
module PredId = Interned_string.Make ()
module HPredId = Interned_string.Make ()

type var_id = VarId.t
type pred_id = PredId.t
type hpred_id = HPredId.t

let false_id = HPredId.import "⊥"

(** Definition of internal representation of terms, propositions and data
    structures storing them. *)

module rec Internal : sig
  (** De bruijn shift and binder names. This record satisfies the invariant:
      shift = List.length name_list *)

  type binder_info = { shift : int; typed_str_list : (string * itype) list }
  type internal_term = IVar of var_id | IBVar of int

  type internal_prop =
    | IPersistent of internal_iprop
    | INot of internal_prop
    | IAnd of PropSet.t
    | IOr of internal_prop * internal_prop
    | IImply of internal_prop * internal_prop
    | IPred of pred_id * internal_term array
    | IForall of binder_info * internal_prop
    | IExists of binder_info * internal_prop
    | IEq of internal_term * internal_term
    | INeq of internal_term * internal_term

  and simple_internal_iprop = SimpleIpropMset.t * PropSet.t

  and internal_iprop =
    | ISimple of simple_internal_iprop
    | IWand of internal_iprop * internal_iprop
    | IHForall of binder_info * internal_iprop
    | IHExists of binder_info * internal_iprop

  val compare_internal_term : internal_term -> internal_term -> int

  val compare_internal_term_array :
    internal_term array -> internal_term array -> int

  val compare_internal_prop : internal_prop -> internal_prop -> int

  val compare_simple_internal_iprop :
    simple_internal_iprop -> simple_internal_iprop -> int

  val compare_internal_iprop : internal_iprop -> internal_iprop -> int
end = struct
  type binder_info = { shift : int; typed_str_list : (string * itype) list }
  type internal_term = IVar of var_id | IBVar of int

  type internal_prop =
    | IPersistent of internal_iprop
    | INot of internal_prop
    | IAnd of PropSet.t
    | IOr of internal_prop * internal_prop
    | IImply of internal_prop * internal_prop
    | IPred of pred_id * internal_term array
    | IForall of binder_info * internal_prop
    | IExists of binder_info * internal_prop
    | IEq of internal_term * internal_term
    | INeq of internal_term * internal_term

  and simple_internal_iprop = SimpleIpropMset.t * PropSet.t

  and internal_iprop =
    | ISimple of simple_internal_iprop
    | IWand of internal_iprop * internal_iprop
    | IHForall of binder_info * internal_iprop
    | IHExists of binder_info * internal_iprop

  let compare_internal_term tm1 tm2 =
    match (tm1, tm2) with
    | IVar var1, IVar var2 -> VarId.compare var1 var2
    | IBVar ind1, IBVar ind2 -> Int.compare ind1 ind2
    | _, _ -> Stdlib.compare tm1 tm2

  (** This function assumes that two arrays are of the same length. *)

  let compare_internal_term_array tm_arr1 tm_arr2 =
    let len = Array.length tm_arr1 in
    let rec cmp i =
      if i = len then 0
      else
        let c = compare_internal_term tm_arr1.(i) tm_arr2.(i) in
        if c = 0 then cmp (i + 1) else c
    in
    cmp 0

  let rec compare_internal_prop pr1 pr2 =
    match (pr1, pr2) with
    | IPersistent ipr1, IPersistent ipr2 -> compare_internal_iprop ipr1 ipr2
    | INot pr1, INot pr2 -> compare_internal_prop pr1 pr2
    | IAnd pr_set1, IAnd pr_set2 -> PropSet.compare pr_set1 pr_set2
    | IOr (pr11, pr12), IOr (pr21, pr22)
    | IImply (pr11, pr12), IImply (pr21, pr22) ->
        let tmp = compare_internal_prop pr11 pr21 in
        if tmp = 0 then compare_internal_prop pr12 pr22 else tmp
    | IPred (pred1, tm_arr1), IPred (pred2, tm_arr2) ->
        let tmp = PredId.compare pred1 pred2 in
        if tmp = 0 then compare_internal_term_array tm_arr1 tm_arr2 else tmp
    | IForall ({ shift = shift1 }, pr1), IForall ({ shift = shift2 }, pr2)
    | IExists ({ shift = shift1 }, pr1), IExists ({ shift = shift2 }, pr2) ->
        let tmp = Int.compare shift1 shift2 in
        if tmp = 0 then compare pr1 pr2 else tmp
    | IEq (tm11, tm12), IEq (tm21, tm22) | INeq (tm11, tm12), INeq (tm21, tm22)
      ->
        let tmp = compare_internal_term tm11 tm21 in
        if tmp = 0 then compare_internal_term tm12 tm22 else tmp
    | _, _ -> Stdlib.compare pr1 pr2

  and compare_simple_internal_iprop (ipr_mset1, pr_set1) (ipr_mset2, pr_set2) =
    let tmp = SimpleIpropMset.compare ipr_mset1 ipr_mset2 in
    if tmp = 0 then PropSet.compare pr_set1 pr_set2 else tmp

  and compare_internal_iprop ipr1 ipr2 =
    match (ipr1, ipr2) with
    | ISimple ipr1, ISimple ipr2 -> compare_simple_internal_iprop ipr1 ipr2
    | IWand (ipr11, ipr12), IWand (ipr21, ipr22) ->
        let tmp = compare_internal_iprop ipr11 ipr21 in
        if tmp = 0 then compare_internal_iprop ipr12 ipr22 else tmp
    | IHForall ({ shift = shift1 }, ipr1), IHForall ({ shift = shift2 }, ipr2)
      ->
        let tmp = Int.compare shift1 shift2 in
        if tmp = 0 then compare_internal_iprop ipr1 ipr2 else tmp
    | IHExists ({ shift = shift1 }, ipr1), IHExists ({ shift = shift2 }, ipr2)
      ->
        let tmp = Int.compare shift1 shift2 in
        if tmp = 0 then compare_internal_iprop ipr1 ipr2 else tmp
    | _, _ -> Stdlib.compare ipr1 ipr2
end

and OrderedInternalProp :
  (Baby.OrderedType with type t = Internal.internal_prop) = struct
  type t = Internal.internal_prop

  let compare = Internal.compare_internal_prop
end

and OrderedInternalIprop :
  (Baby.OrderedType with type t = Internal.internal_iprop) = struct
  type t = Internal.internal_iprop

  let compare = Internal.compare_internal_iprop
end

and OrderedTermArray :
  (Baby.OrderedType with type t = Internal.internal_term array) = struct
  type t = Internal.internal_term array

  let compare = Internal.compare_internal_term_array
end

and OrderedHPredId : (Baby.OrderedType with type t = HPredId.t) = struct
  type t = HPredId.t

  let compare = HPredId.compare
end

and PropSet : (Set.Set with type elt = Internal.internal_prop) =
  Set.Make (OrderedInternalProp)

and IpropSet : (Set.Set with type elt = Internal.internal_iprop) =
  Set.Make (OrderedInternalIprop)

and SimpleIpropMset :
  (Multiset.Multiset2
    with type elt1 = OrderedHPredId.t
     and type elt2 = OrderedTermArray.t) =
  Multiset.Make2 (OrderedHPredId) (OrderedTermArray)

include Internal

type internal_prop_set = PropSet.t
type internal_iprop_set = IpropSet.t
type simple_internal_iprop_multiset = SimpleIpropMset.t

let ( pp_internal_term,
      pp_internal_term_env,
      pp_internal_prop,
      pp_internal_prop_env,
      pp_simple_internal_iprop,
      pp_simple_internal_iprop_env,
      pp_internal_iprop,
      pp_internal_iprop_env,
      pp_internal_prop_set,
      pp_internal_prop_set_env,
      pp_internal_iprop_set,
      pp_internal_iprop_set_env,
      pp_simple_internal_iprop_multiset,
      pp_simple_internal_iprop_multiset_env ) =
  let rec repeat f sep n =
    if n != 0 then (
      f ();
      if n != 1 then sep ();
      repeat f sep (n - 1))
  in
  let pp_internal_term_aux env fmt = function
    | IVar var -> fprintf fmt "%s" (VarId.export var)
    | IBVar ind -> (
        match List.nth_opt env ind with
        | Some var -> fprintf fmt "%s" var
        | None -> fprintf fmt "#%i" ind)
  in
  let rec pp_internal_prop_aux env fmt = function
    | IPersistent ipr ->
        fprintf fmt "Persistent %a" (pp_internal_iprop_aux env) ipr
    | INot pr -> fprintf fmt "¬ %a" (pp_internal_prop_aux env) pr
    | IAnd pr_set ->
        fprintf fmt "(%a)"
          (pp_internal_prop_set_aux false env ~pp_sep:(fun fmt () ->
               fprintf fmt " ∧ "))
          pr_set
    | IOr (pr1, pr2) ->
        fprintf fmt "(%a ∨ %a)" (pp_internal_prop_aux env) pr1
          (pp_internal_prop_aux env) pr2
    | IImply (pr1, pr2) ->
        fprintf fmt "(%a → %a)" (pp_internal_prop_aux env) pr1
          (pp_internal_prop_aux env) pr2
    | IPred (pred, tm_arr) ->
        if Array.length tm_arr = 0 then fprintf fmt "%s" (PredId.export pred)
        else
          fprintf fmt "%s %a" (PredId.export pred)
            (pp_print_array
               ~pp_sep:(fun fmt () -> fprintf fmt " ")
               (pp_internal_term_aux env))
            tm_arr
    | IForall ({ typed_str_list }, pr) ->
        fprintf fmt "forall %a, %a"
          (pp_typed_strs_list
             ~pp_sep:(fun fmt () -> fprintf fmt " ")
             ~pp_paren:true ())
          (group_typed_str typed_str_list)
          (pp_internal_prop_aux
             (List.fold_left
                (fun acc (str, _) -> str :: acc)
                env typed_str_list))
          pr
    | IExists ({ typed_str_list }, pr) ->
        fprintf fmt "exists %a, %a"
          (pp_typed_strs_list
             ~pp_sep:(fun fmt () -> fprintf fmt " ")
             ~pp_paren:true ())
          (group_typed_str typed_str_list)
          (pp_internal_prop_aux
             (List.fold_left
                (fun acc (str, _) -> str :: acc)
                env typed_str_list))
          pr
    | IEq (tm1, tm2) ->
        fprintf fmt "%a = %a" (pp_internal_term_aux env) tm1
          (pp_internal_term_aux env) tm2
    | INeq (tm1, tm2) ->
        fprintf fmt "%a ≠ %a" (pp_internal_term_aux env) tm1
          (pp_internal_term_aux env) tm2
  and pp_simple_internal_iprop_aux env fmt (ipr_mset, pr_set) =
    match (SimpleIpropMset.is_empty ipr_mset, PropSet.is_empty pr_set) with
    | false, false ->
        fprintf fmt "%a * %a"
          (pp_simple_internal_iprop_multiset_aux env ~pp_sep:(fun fmt () ->
               fprintf fmt " * "))
          ipr_mset
          (pp_internal_prop_set_aux true env ~pp_sep:(fun fmt () ->
               fprintf fmt " * "))
          pr_set
    | false, true ->
        fprintf fmt "%a"
          (pp_simple_internal_iprop_multiset_aux env ~pp_sep:(fun fmt () ->
               fprintf fmt " * "))
          ipr_mset
    | true, false ->
        fprintf fmt "%a"
          (pp_internal_prop_set_aux true env ~pp_sep:(fun fmt () ->
               fprintf fmt " * "))
          pr_set
    | true, true -> assert false
  and pp_internal_iprop_aux env fmt = function
    | ISimple ipr -> pp_simple_internal_iprop_aux env fmt ipr
    | IWand (ipr1, ipr2) ->
        fprintf fmt "(%a -* %a)"
          (pp_internal_iprop_aux env)
          ipr1
          (pp_internal_iprop_aux env)
          ipr2
    | IHForall ({ typed_str_list }, ipr) ->
        fprintf fmt "forall %a, %a"
          (pp_typed_strs_list
             ~pp_sep:(fun fmt () -> fprintf fmt " ")
             ~pp_paren:true ())
          (group_typed_str typed_str_list)
          (pp_internal_iprop_aux
             (List.fold_left
                (fun acc (str, _) -> str :: acc)
                env typed_str_list))
          ipr
    | IHExists ({ typed_str_list }, ipr) ->
        fprintf fmt "exists %a, %a"
          (pp_typed_strs_list
             ~pp_sep:(fun fmt () -> fprintf fmt " ")
             ~pp_paren:true ())
          (group_typed_str typed_str_list)
          (pp_internal_iprop_aux
             (List.fold_left
                (fun acc (str, _) -> str :: acc)
                env typed_str_list))
          ipr
  and pp_internal_prop_set_aux pure env ?(pp_sep = pp_print_cut) fmt pr_set =
    if PropSet.is_empty pr_set then pp_print_string fmt "%empty"
    else
      pp_print_list ~pp_sep
        (if pure then fun fmt pr ->
           fprintf fmt "⌜ %a ⌝" (pp_internal_prop_aux env) pr
         else pp_internal_prop_aux env)
        fmt (PropSet.to_list pr_set)
  and pp_internal_iprop_set_aux env ?(pp_sep = pp_print_cut) fmt ipr_set =
    if IpropSet.is_empty ipr_set then pp_print_string fmt "%empty"
    else
      pp_print_list ~pp_sep
        (pp_internal_iprop_aux env)
        fmt (IpropSet.to_list ipr_set)
  and pp_simple_internal_iprop_multiset_aux env ?(pp_sep = pp_print_cut) fmt
      ipr_mset =
    if SimpleIpropMset.is_empty ipr_mset then pp_print_string fmt "%empty"
    else
      pp_print_list ~pp_sep
        (fun fmt ((hpred, tm_arr), count) ->
          match (Array.length tm_arr = 0, Multiplicity.is_finite count) with
          | true, true ->
              repeat
                (fun () -> fprintf fmt "%s" (HPredId.export hpred))
                (pp_sep fmt)
                (Multiplicity.to_int count)
          | true, false -> fprintf fmt "□ %s" (HPredId.export hpred)
          | false, true ->
              repeat
                (fun () ->
                  fprintf fmt "%s %a" (HPredId.export hpred)
                    (pp_print_array
                       ~pp_sep:(fun fmt () -> fprintf fmt " ")
                       (pp_internal_term_aux env))
                    tm_arr)
                (pp_sep fmt)
                (Multiplicity.to_int count)
          | false, false ->
              fprintf fmt "□ (%s %a)" (HPredId.export hpred)
                (pp_print_array
                   ~pp_sep:(fun fmt () -> fprintf fmt " ")
                   (pp_internal_term_aux env))
                tm_arr)
        fmt
        (SimpleIpropMset.to_list ipr_mset)
  in
  ( pp_internal_term_aux [],
    pp_internal_term_aux,
    pp_internal_prop_aux [],
    pp_internal_prop_aux,
    pp_simple_internal_iprop_aux [],
    pp_simple_internal_iprop_aux,
    pp_internal_iprop_aux [],
    pp_internal_iprop_aux,
    pp_internal_prop_set_aux false [],
    pp_internal_prop_set_aux false,
    pp_internal_iprop_set_aux [],
    pp_internal_iprop_set_aux,
    pp_simple_internal_iprop_multiset_aux [],
    pp_simple_internal_iprop_multiset_aux )

(** Smart internal_term constructors. *)

let iVar var = IVar var
let iVar_str str = IVar (VarId.import str)
let iBVar ind = IBVar ind

(** Smart internal_prop constructors. *)

let iPersistent ipr = IPersistent ipr
let iNot pr = INot pr
let iAnd pr_set = IAnd pr_set
let iOr (pr1, pr2) = IOr (pr1, pr2)
let iImply (ipr1, ipr2) = IImply (ipr1, ipr2)
let iPred (pred, tm_arr) = IPred (pred, tm_arr)
let iPred_str (str, tm_arr) = IPred (PredId.import str, tm_arr)
let iForall (binder_info, pr) = IForall (binder_info, pr)

let iForall_raw (typed_str_list, pr) =
  IForall ({ shift = List.length typed_str_list; typed_str_list }, pr)

let iExists (binder_info, pr) = IExists (binder_info, pr)

let iExists_raw (typed_str_list, pr) =
  IExists ({ shift = List.length typed_str_list; typed_str_list }, pr)

let iEq (tm1, tm2) = IEq (tm1, tm2)
let iNeq (tm1, tm2) = INeq (tm1, tm2)

(** Smart internal_iprop constructors. *)

let iSimple (ipr_mset, pr_set) = ISimple (ipr_mset, pr_set)
let iWand (ipr1, ipr2) = IWand (ipr1, ipr2)
let iHForall (binder_info, ipr) = IHForall (binder_info, ipr)

let iHForall_raw (typed_str_list, ipr) =
  IHForall ({ shift = List.length typed_str_list; typed_str_list }, ipr)

let iHExists (binder_info, ipr) = IHExists (binder_info, ipr)

let iHExists_raw (typed_str_list, ipr) =
  IHExists ({ shift = List.length typed_str_list; typed_str_list }, ipr)
