open Ast
open Internal
open Format
open State

let term_to_internal_term, prop_to_internal_prop, iprop_to_internal_iprop =
  let term_to_internal_term_aux ~env = function
    | Var var -> (
        match List.find_index (String.equal var) env with
        | Some i -> iBVar i
        | None -> iVar_str var)
  in
  let rec prop_to_internal_prop_aux ~env = function
    | Persistent ipr -> iPersistent (iprop_to_internal_iprop_aux ~env ipr)
    | Not pr -> iNot (prop_to_internal_prop_aux ~env pr)
    | And (pr1, pr2) ->
        let pr_set1 =
          match prop_to_internal_prop_aux ~env pr1 with
          | IAnd pr_set -> pr_set
          | _ as pr -> PropSet.singleton pr
        in
        let pr_set2 =
          match prop_to_internal_prop_aux ~env pr2 with
          | IAnd pr_set -> pr_set
          | _ as pr -> PropSet.singleton pr
        in
        iAnd (PropSet.union pr_set1 pr_set2)
    | Or (pr1, pr2) ->
        iOr
          ( prop_to_internal_prop_aux ~env pr1,
            prop_to_internal_prop_aux ~env pr2 )
    | Imply (pr1, pr2) ->
        iImply
          ( prop_to_internal_prop_aux ~env pr1,
            prop_to_internal_prop_aux ~env pr2 )
    | Pred (pred, tm_list) ->
        iPred_str
          ( pred,
            Array.of_list (List.map (term_to_internal_term_aux ~env) tm_list) )
    | Forall (typed_str_list, pr) ->
        iForall_raw
          ( typed_str_list,
            prop_to_internal_prop_aux
              ~env:
                (List.fold_left
                   (fun acc (str, _) -> str :: acc)
                   env typed_str_list)
              pr )
    | Exists (typed_str_list, pr) ->
        iExists_raw
          ( typed_str_list,
            prop_to_internal_prop_aux
              ~env:
                (List.fold_left
                   (fun acc (str, _) -> str :: acc)
                   env typed_str_list)
              pr )
    | Eq (tm1, tm2) ->
        iEq
          ( term_to_internal_term_aux ~env tm1,
            term_to_internal_term_aux ~env tm2 )
    | Neq (tm1, tm2) ->
        iNeq
          ( term_to_internal_term_aux ~env tm1,
            term_to_internal_term_aux ~env tm2 )
  and iprop_to_internal_iprop_aux ~env : iprop -> internal_iprop = function
    | False ->
        iSimple
          ( SimpleIpropMset.singleton (HPredId.import "⊥", [||]) Multiplicity.one,
            PropSet.empty )
    | Atom atom ->
        iSimple
          ( SimpleIpropMset.singleton
              (HPredId.import atom, [||])
              Multiplicity.one,
            PropSet.empty )
    | Pure pr ->
        iSimple
          ( SimpleIpropMset.empty,
            PropSet.singleton (prop_to_internal_prop_aux ~env pr) )
    | Star (ipr1, ipr2) ->
        let ipr_mset1, pr_set1 =
          match iprop_to_internal_iprop_aux ~env ipr1 with
          | ISimple (ipr_mset, pr_set) -> (ipr_mset, pr_set)
          | _ -> assert false
        in
        let ipr_mset2, pr_set2 =
          match iprop_to_internal_iprop_aux ~env ipr2 with
          | ISimple (ipr_mset, pr_set) -> (ipr_mset, pr_set)
          | _ -> assert false
        in
        iSimple
          ( SimpleIpropMset.union ipr_mset1 ipr_mset2,
            PropSet.union pr_set1 pr_set2 )
    | Wand (ipr1, ipr2) ->
        iWand
          ( iprop_to_internal_iprop_aux ~env ipr1,
            iprop_to_internal_iprop_aux ~env ipr2 )
    | Box ipr -> (
        match iprop_to_internal_iprop_aux ~env ipr with
        | ISimple (ipr_mset, pr_set) ->
            iSimple
              ( SimpleIpropMset.map_multiplicity
                  (fun _ _ -> Multiplicity.inf)
                  ipr_mset,
                pr_set )
        | _ -> assert false)
    | HPred (hpred, tm_list) ->
        iSimple
          ( SimpleIpropMset.singleton
              ( HPredId.import hpred,
                Array.of_list
                  (List.map (term_to_internal_term_aux ~env) tm_list) )
              Multiplicity.one,
            PropSet.empty )
    | HForall (typed_str_list, ipr) ->
        iHForall_raw
          ( typed_str_list,
            iprop_to_internal_iprop_aux
              ~env:
                (List.fold_left
                   (fun acc (str, _) -> str :: acc)
                   env typed_str_list)
              ipr )
    | HExists (typed_str_list, ipr) ->
        iHExists_raw
          ( typed_str_list,
            iprop_to_internal_iprop_aux
              ~env:
                (List.fold_left
                   (fun acc (str, _) -> str :: acc)
                   env typed_str_list)
              ipr )
  in
  ( term_to_internal_term_aux ~env:[],
    prop_to_internal_prop_aux ~env:[],
    iprop_to_internal_iprop_aux ~env:[] )

let prop_list_to_internal_prop_set pr_list =
  List.fold_left
    (fun acc pr ->
      PropSet.union acc
        (match prop_to_internal_prop pr with
        | IAnd pr_set -> pr_set
        | _ as pr -> PropSet.singleton pr))
    PropSet.empty pr_list

let iprop_list_to_internal_iprop_set iprl =
  IpropSet.of_list (List.map iprop_to_internal_iprop iprl)

let iprop_list_to_simple_internal_iprop_multiset_and_internal_prop_set iprl =
  List.fold_left
    (fun (ipr_mset, pr_set) ipr ->
      match iprop_to_internal_iprop ipr with
      | ISimple (ipr_mset', pr_set') ->
          ( SimpleIpropMset.union ipr_mset ipr_mset',
            PropSet.union pr_set pr_set' )
      | _ -> assert false)
    (SimpleIpropMset.empty, PropSet.empty)
    iprl

(** Substitution of quantified variable. *)

type subst_task = internal_term option array

let ( subst_internal_term,
      subst_internal_term_array,
      subst_internal_prop,
      subst_simple_internal_iprop,
      subst_internal_iprop,
      subst_internal_prop_set,
      subst_internal_iprop_set,
      subst_simple_internal_iprop_multiset ) =
  let subst_internal_term_aux shift subst_task tm =
    match tm with
    | IBVar ind -> (
        match subst_task.(ind - shift) with
        | Some subst_tm -> subst_tm
        | None -> tm)
    | _ -> tm
  in
  let subst_internal_term_array_aux shift subst_task tm_arr =
    Array.map (subst_internal_term_aux shift subst_task) tm_arr
  in
  let rec subst_internal_prop_aux shift subst_task = function
    | IPersistent ipr ->
        iPersistent (subst_internal_iprop_aux shift subst_task ipr)
    | INot pr -> iNot (subst_internal_prop_aux shift subst_task pr)
    | IAnd pr_set -> iAnd (subst_internal_prop_set_aux shift subst_task pr_set)
    | IOr (pr1, pr2) ->
        iOr
          ( subst_internal_prop_aux shift subst_task pr1,
            subst_internal_prop_aux shift subst_task pr2 )
    | IImply (pr1, pr2) ->
        iImply
          ( subst_internal_prop_aux shift subst_task pr1,
            subst_internal_prop_aux shift subst_task pr2 )
    | IPred (pred, tm_arr) ->
        iPred (pred, Array.map (subst_internal_term_aux shift subst_task) tm_arr)
    | IForall (binder_info, pr) ->
        iForall
          ( binder_info,
            subst_internal_prop_aux (shift + binder_info.shift) subst_task pr )
    | IExists (binder_info, pr) ->
        iExists
          ( binder_info,
            subst_internal_prop_aux (shift + binder_info.shift) subst_task pr )
    | IEq (tm1, tm2) ->
        iEq
          ( subst_internal_term_aux shift subst_task tm1,
            subst_internal_term_aux shift subst_task tm2 )
    | INeq (tm1, tm2) ->
        iNeq
          ( subst_internal_term_aux shift subst_task tm1,
            subst_internal_term_aux shift subst_task tm2 )
  and subst_simple_internal_iprop_aux shift subst_task (ipr_mset, pr_set) =
    ( subst_simple_internal_iprop_multiset_aux shift subst_task ipr_mset,
      subst_internal_prop_set_aux shift subst_task pr_set )
  and subst_internal_iprop_aux shift subst_task = function
    | IWand (ipr1, ipr2) ->
        iWand
          ( subst_internal_iprop_aux shift subst_task ipr1,
            subst_internal_iprop_aux shift subst_task ipr2 )
    | IHForall (binder_info, ipr) ->
        iHForall
          ( binder_info,
            subst_internal_iprop_aux (shift + binder_info.shift) subst_task ipr
          )
    | _ as ipr -> ipr
  and subst_internal_prop_set_aux shift subst_task pr_set =
    PropSet.map (subst_internal_prop_aux shift subst_task) pr_set
  and subst_internal_iprop_set_aux shift subst_task ipr_set =
    IpropSet.map (subst_internal_iprop_aux shift subst_task) ipr_set
  and subst_simple_internal_iprop_multiset_aux shift subst_task ipr_mset =
    SimpleIpropMset.map
      (subst_internal_term_array_aux shift subst_task)
      ipr_mset
  in
  ( subst_internal_term_aux 0,
    subst_internal_term_array_aux 0,
    subst_internal_prop_aux 0,
    subst_simple_internal_iprop_aux 0,
    subst_internal_iprop_aux 0,
    subst_internal_prop_set_aux 0,
    subst_internal_iprop_set_aux 0,
    subst_simple_internal_iprop_multiset_aux 0 )

(** Pattern match with quantifiers. *)

type match_result = internal_term option array

open Monads.ListMonad

let ( internal_term_match,
      internal_term_array_match,
      internal_prop_match,
      internal_iprop_match,
      internal_prop_set_match,
      simple_internal_iprop_multiset_match,
      internal_prop_set_substract_match,
      simple_internal_iprop_multiset_substract_match ) =
  let internal_term_match_aux shift match_result ptm tm =
    match (ptm, tm) with
    | IVar var1, IVar var2 ->
        if VarId.equal var1 var2 then return match_result else fail
    | IBVar ind, _ -> (
        match match_result.(ind - shift) with
        | Some res ->
            if compare_internal_term res tm = 0 then return match_result
            else fail
        | None ->
            let match_result' = Array.copy match_result in
            let () = match_result'.(ind - shift) <- Some tm in
            return match_result')
    | _, _ -> fail
  in
  let internal_term_array_match_aux shift match_result ptm_arr tm_arr =
    let match_result = ref (return match_result) in
    Array.iter2
      (fun ptm tm ->
        match_result :=
          let* match_result' = !match_result in
          internal_term_match_aux shift match_result' ptm tm)
      ptm_arr tm_arr;
    !match_result
  in
  let rec internal_prop_match_aux shift match_result ppr pr =
    match (ppr, pr) with
    | IPersistent ipr1, IPersistent ipr2 ->
        internal_iprop_match_aux shift match_result ipr1 ipr2
    | INot pr1, INot pr2 -> internal_prop_match_aux shift match_result pr1 pr2
    | IAnd pr_set1, IAnd pr_set2 ->
        if PropSet.cardinal pr_set1 = PropSet.cardinal pr_set2 then
          let+ match_result', _ =
            internal_prop_set_match_aux shift match_result pr_set1 pr_set2
          in
          match_result'
        else fail
    | IOr (pr11, pr12), IOr (pr21, pr22)
    | IImply (pr11, pr12), IImply (pr21, pr22) ->
        let* match_result' =
          internal_prop_match_aux shift match_result pr11 pr21
        in
        internal_prop_match_aux shift match_result' pr12 pr22
    | IPred (pred1, tm_arr1), IPred (pred2, tm_arr2) ->
        if PredId.equal pred1 pred2 then
          internal_term_array_match_aux shift match_result tm_arr1 tm_arr2
        else fail
    | IForall ({ shift = shift1 }, pr1), IForall ({ shift = shift2 }, pr2)
    | IExists ({ shift = shift1 }, pr1), IExists ({ shift = shift2 }, pr2) ->
        if shift1 = shift2 then
          internal_prop_match_aux (shift + shift1) match_result pr1 pr2
        else fail
    | IEq (tm11, tm12), IEq (tm21, tm22) | INeq (tm11, tm12), INeq (tm21, tm22)
      ->
        let* match_result' =
          internal_term_match_aux shift match_result tm11 tm21
        in
        internal_term_match_aux shift match_result' tm12 tm22
    | _, _ -> fail
  and internal_iprop_match_aux shift match_result pipr ipr =
    match (pipr, ipr) with
    | ISimple (ipr_mset1, pr_set1), ISimple (ipr_mset2, pr_set2) ->
        if
          SimpleIpropMset.cardinal ipr_mset1
          = SimpleIpropMset.cardinal ipr_mset2
          && PropSet.cardinal pr_set1 = PropSet.cardinal pr_set2
        then
          let* match_result', _, _ =
            simple_internal_iprop_multiset_match_aux shift match_result
              ipr_mset1 ipr_mset2
          in
          let+ match_result'', _ =
            internal_prop_set_match_aux shift match_result' pr_set1 pr_set2
          in
          match_result''
        else fail
    | IWand (ipr11, ipr12), IWand (ipr21, ipr22) ->
        let* match_result' =
          internal_iprop_match_aux shift match_result ipr11 ipr21
        in
        internal_iprop_match_aux shift match_result' ipr12 ipr22
    | IHForall ({ shift = shift1 }, ipr1), IHForall ({ shift = shift2 }, ipr2)
    | IHExists ({ shift = shift1 }, ipr1), IHExists ({ shift = shift2 }, ipr2)
      ->
        if shift1 = shift2 then
          internal_iprop_match_aux (shift + shift1) match_result ipr1 ipr2
        else fail
  and internal_prop_set_match_aux shift match_result ppr_set pr_set =
    let match_result_and_pr_set = ref (return (match_result, pr_set)) in
    PropSet.iter
      (fun ppr ->
        match_result_and_pr_set :=
          let* match_result', pr_set' = !match_result_and_pr_set in
          internal_prop_set_substract_match_aux shift match_result' ppr pr_set')
      ppr_set;
    !match_result_and_pr_set
  and simple_internal_iprop_multiset_match_aux shift match_result pipr_mset
      ipr_mset : (match_result * simple_internal_iprop_multiset * bool) t =
    let match_result_and_ipr_mset_and_is_inf =
      ref (return (match_result, ipr_mset, true))
    in
    SimpleIpropMset.iter
      (fun pipr count ->
        match_result_and_ipr_mset_and_is_inf :=
          let* match_result', ipr_mset', is_inf =
            !match_result_and_ipr_mset_and_is_inf
          in
          let+ match_result'', ipr_mset'', is_inf' =
            simple_internal_iprop_multiset_substract_match_aux shift
              match_result' pipr count ipr_mset'
          in
          (match_result'', ipr_mset'', is_inf && is_inf'))
      pipr_mset;
    !match_result_and_ipr_mset_and_is_inf
  and internal_prop_set_substract_match_aux shift match_result ppr pr_set :
      (match_result * internal_prop_set) t =
    PropSet.fold
      (fun pr acc ->
        (let+ match_result' =
           internal_prop_match_aux shift match_result ppr pr
         in
         (match_result', PropSet.remove pr pr_set))
        |> choose acc)
      pr_set fail
  and simple_internal_iprop_multiset_substract_match_aux shift match_result
      (phpred, ptm_arr) pcount ipr_mset :
      (match_result * simple_internal_iprop_multiset * bool) t =
    SimpleIpropMset.fold
      (fun ((hpred, tm_arr) as ipr) count acc ->
        (if HPredId.equal phpred hpred && Multiplicity.compare pcount count <= 0
         then
           let+ match_result' =
             internal_term_array_match_aux shift match_result ptm_arr tm_arr
           in
           ( match_result',
             SimpleIpropMset.remove ipr pcount ipr_mset,
             Multiplicity.is_infinite count )
         else fail)
        |> choose acc)
      ipr_mset fail
  in

  ( internal_term_match_aux 0,
    internal_term_array_match_aux 0,
    internal_prop_match_aux 0,
    internal_iprop_match_aux 0,
    internal_prop_set_match_aux 0,
    simple_internal_iprop_multiset_match_aux 0,
    internal_prop_set_substract_match_aux 0,
    simple_internal_iprop_multiset_substract_match_aux 0 )
