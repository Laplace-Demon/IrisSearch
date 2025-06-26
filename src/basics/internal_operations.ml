open Ast
open Internal
open Type
open Validate

let term_to_internal_term, prop_to_internal_prop, iprop_to_internal_iprop =
  let rec term_to_internal_term_aux ~env symbol_table = function
    | Var var -> (
        match
          List.find_mapi
            (fun ind (str, ity) ->
              if String.equal var str then Some (ind, ity) else None)
            env
        with
        | Some (ind, ity) -> iBVar (ind, ity)
        | None -> (
            match Hashtbl.find symbol_table var with
            | { ity; kind = Constr } -> iConstr_str (var, [||], ity)
            | _ -> iVar_str (var, (Hashtbl.find symbol_table var).ity)))
    | App (func, tm_list) -> (
        match Hashtbl.find symbol_table func with
        | { ity = Tarrow (_, ity); kind = Constr } ->
            iConstr_str
              ( func,
                Array.of_list
                  (List.map
                     (term_to_internal_term_aux ~env symbol_table)
                     tm_list),
                ity )
        | { ity = Tarrow (_, ity); kind = Func } ->
            iFunc_str
              ( func,
                Array.of_list
                  (List.map
                     (term_to_internal_term_aux ~env symbol_table)
                     tm_list),
                ity )
        | _ -> assert false)
  in
  let rec prop_to_internal_prop_aux ~env symbol_table = function
    | Persistent ipr ->
        iPersistent (iprop_to_internal_iprop_aux ~env symbol_table ipr)
    | Not pr -> iNot (prop_to_internal_prop_aux ~env symbol_table pr)
    | And (pr1, pr2) ->
        let pr_set1 =
          match prop_to_internal_prop_aux ~env symbol_table pr1 with
          | IAnd pr_set -> pr_set
          | _ as pr -> PropSet.singleton pr
        in
        let pr_set2 =
          match prop_to_internal_prop_aux ~env symbol_table pr2 with
          | IAnd pr_set -> pr_set
          | _ as pr -> PropSet.singleton pr
        in
        iAnd (PropSet.union pr_set1 pr_set2)
    | Or (pr1, pr2) ->
        iOr
          ( prop_to_internal_prop_aux ~env symbol_table pr1,
            prop_to_internal_prop_aux ~env symbol_table pr2 )
    | Imply (pr1, pr2) ->
        iImply
          ( prop_to_internal_prop_aux ~env symbol_table pr1,
            prop_to_internal_prop_aux ~env symbol_table pr2 )
    | Pred (pred, tm_list) ->
        iPred_str
          ( pred,
            Array.of_list
              (List.map (term_to_internal_term_aux ~env symbol_table) tm_list)
          )
    | Forall (typed_str_list, pr) ->
        iForall_raw
          ( typed_str_list,
            prop_to_internal_prop_aux
              ~env:
                (List.fold_left
                   (fun acc typed_str -> typed_str :: acc)
                   env typed_str_list)
              symbol_table pr )
    | Exists (typed_str_list, pr) ->
        iExists_raw
          ( typed_str_list,
            prop_to_internal_prop_aux
              ~env:
                (List.fold_left
                   (fun acc typed_str -> typed_str :: acc)
                   env typed_str_list)
              symbol_table pr )
    | Eq (tm1, tm2) ->
        iEq
          ( term_to_internal_term_aux ~env symbol_table tm1,
            term_to_internal_term_aux ~env symbol_table tm2 )
    | Neq (tm1, tm2) ->
        iNeq
          ( term_to_internal_term_aux ~env symbol_table tm1,
            term_to_internal_term_aux ~env symbol_table tm2 )
  and iprop_to_internal_iprop_aux ~env symbol_table = function
    | False ->
        iSimple
          ( ( SimpleIpropMset.singleton
                (HPredId.import "⊥", [||])
                Multiplicity.one,
              PropSet.empty ),
            [] )
    | Atom atom ->
        iSimple
          ( ( SimpleIpropMset.singleton
                (HPredId.import atom, [||])
                Multiplicity.one,
              PropSet.empty ),
            [] )
    | Pure pr ->
        iSimple
          ( ( SimpleIpropMset.empty,
              PropSet.singleton (prop_to_internal_prop_aux ~env symbol_table pr)
            ),
            [] )
    | Star (ipr1, ipr2) ->
        let ipr1, ipr_disj1 =
          match iprop_to_internal_iprop_aux ~env symbol_table ipr1 with
          | ISimple (ipr, ipr_disj) -> (ipr, ipr_disj)
          | _ -> assert false
        in
        let ipr2, ipr_disj2 =
          match iprop_to_internal_iprop_aux ~env symbol_table ipr2 with
          | ISimple (ipr, ipr_disj) -> (ipr, ipr_disj)
          | _ -> assert false
        in
        let disj_list =
          match (ipr_disj1, ipr_disj2) with
          | [], _ -> ipr_disj2
          | _, [] -> ipr_disj1
          | _, _ ->
              List.concat_map
                (fun ipr1 ->
                  List.map (combine_simple_internal_iprop ipr1) ipr_disj2)
                ipr_disj1
        in
        iSimple (combine_simple_internal_iprop ipr1 ipr2, disj_list)
    | HOr (ipr1, ipr2) ->
        let ipr1, ipr_disj1 =
          match iprop_to_internal_iprop_aux ~env symbol_table ipr1 with
          | ISimple (ipr, ipr_disj) -> (ipr, ipr_disj)
          | _ -> assert false
        in
        let ipr2, ipr_disj2 =
          match iprop_to_internal_iprop_aux ~env symbol_table ipr2 with
          | ISimple (ipr, ipr_disj) -> (ipr, ipr_disj)
          | _ -> assert false
        in
        let ipr_disj_combine1 =
          match ipr_disj1 with
          | [] -> [ ipr1 ]
          | _ -> List.map (combine_simple_internal_iprop ipr1) ipr_disj1
        in
        let ipr_disj_combine2 =
          match ipr_disj2 with
          | [] -> [ ipr2 ]
          | _ -> List.map (combine_simple_internal_iprop ipr2) ipr_disj2
        in
        iSimple
          (empty_simple_internal_iprop, ipr_disj_combine1 @ ipr_disj_combine2)
    | Wand (ipr1, ipr2) ->
        iWand
          ( iprop_to_internal_iprop_aux ~env symbol_table ipr1,
            iprop_to_internal_iprop_aux ~env symbol_table ipr2 )
    | Box ipr -> (
        match iprop_to_internal_iprop_aux ~env symbol_table ipr with
        | ISimple (ipr, ipr_disj) ->
            let to_inf (ipr_mset, pr_set) =
              ( SimpleIpropMset.map_multiplicity
                  (fun _ _ -> Multiplicity.inf)
                  ipr_mset,
                pr_set )
            in
            iSimple (to_inf ipr, List.map to_inf ipr_disj)
        | _ -> assert false)
    | HPred (hpred, tm_list) ->
        iSimple
          ( ( SimpleIpropMset.singleton
                ( HPredId.import hpred,
                  Array.of_list
                    (List.map
                       (term_to_internal_term_aux ~env symbol_table)
                       tm_list) )
                Multiplicity.one,
              PropSet.empty ),
            [] )
    | HForall (typed_str_list, ipr) ->
        iHForall_raw
          ( typed_str_list,
            iprop_to_internal_iprop_aux
              ~env:
                (List.fold_left
                   (fun acc typed_str -> typed_str :: acc)
                   env typed_str_list)
              symbol_table ipr )
    | HExists (typed_str_list, ipr) ->
        iHExists_raw
          ( typed_str_list,
            iprop_to_internal_iprop_aux
              ~env:
                (List.fold_left
                   (fun acc typed_str -> typed_str :: acc)
                   env typed_str_list)
              symbol_table ipr )
  in
  ( term_to_internal_term_aux ~env:[],
    prop_to_internal_prop_aux ~env:[],
    iprop_to_internal_iprop_aux ~env:[] )

let prop_list_to_internal_prop_set symbol_table pr_list =
  List.fold_left
    (fun acc pr ->
      PropSet.union acc
        (match prop_to_internal_prop symbol_table pr with
        | IAnd pr_set -> pr_set
        | _ as pr -> PropSet.singleton pr))
    PropSet.empty pr_list

let iprop_list_to_simple_internal_iprop_and_disj_list symbol_table iprl =
  List.fold_left
    (fun (ipr_, disj_list_) ipr ->
      match iprop_to_internal_iprop symbol_table ipr with
      | ISimple (ipr', disj_list') ->
          let disj_list =
            match (disj_list_, disj_list') with
            | [], _ -> disj_list'
            | _, [] -> disj_list_
            | _, _ ->
                List.concat_map
                  (fun disj ->
                    List.map (combine_simple_internal_iprop disj) disj_list')
                  disj_list_
          in
          (combine_simple_internal_iprop ipr_ ipr', disj_list)
      | _ -> assert false)
    (empty_simple_internal_iprop, [])
    iprl

(** Free variables. *)

let ( free_vars_internal_term,
      free_vars_internal_term_array,
      free_vars_internal_prop,
      free_vars_simple_internal_iprop,
      free_vars_internal_iprop,
      free_vars_internal_prop_set,
      free_vars_simple_internal_iprop_multiset ) =
  let compare_typed_var_id =
   fun (var_id1, ity1) (var_id2, ity2) ->
    let tmp = VarId.compare var_id1 var_id2 in
    if tmp = 0 then compare_itype ity1 ity2 else tmp
  in
  let rec free_vars_internal_term_aux acc { desc; ity } =
    match desc with
    | IVar var -> (var, ity) :: acc
    | IBVar _ -> acc
    | IConstr (_, tm_arr) | IFunc (_, tm_arr) ->
        free_vars_internal_term_array_aux acc tm_arr
  and free_vars_internal_term_array_aux acc =
    Array.fold_left free_vars_internal_term_aux acc
  in
  let rec free_vars_internal_prop_aux acc = function
    | IPersistent ipr -> free_vars_internal_iprop_aux acc ipr
    | INot pr -> free_vars_internal_prop_aux acc pr
    | IAnd pr_set -> free_vars_internal_prop_set_aux acc pr_set
    | IOr (pr1, pr2) | IImply (pr1, pr2) ->
        let acc' = free_vars_internal_prop_aux acc pr1 in
        free_vars_internal_prop_aux acc' pr2
    | IPred (_, tm_arr) -> free_vars_internal_term_array_aux acc tm_arr
    | IForall (_, pr) | IExists (_, pr) -> free_vars_internal_prop_aux acc pr
    | IEq (tm1, tm2) | INeq (tm1, tm2) ->
        let acc' = free_vars_internal_term_aux acc tm1 in
        free_vars_internal_term_aux acc' tm2
  and free_vars_simple_internal_iprop_aux acc (ipr_mset, pr_set) =
    let acc' = free_vars_simple_internal_iprop_multiset_aux acc ipr_mset in
    free_vars_internal_prop_set_aux acc' pr_set
  and free_vars_internal_iprop_aux acc = function
    | ISimple (ipr, ipr_disj) ->
        let acc' = free_vars_simple_internal_iprop_aux acc ipr in
        List.fold_left free_vars_simple_internal_iprop_aux acc' ipr_disj
    | IWand (ipr1, ipr2) ->
        let acc' = free_vars_internal_iprop_aux acc ipr1 in
        free_vars_internal_iprop_aux acc' ipr2
    | IHForall (_, ipr) | IHExists (_, ipr) ->
        free_vars_internal_iprop_aux acc ipr
  and free_vars_internal_prop_set_aux acc pr_set =
    PropSet.fold (fun pr acc -> free_vars_internal_prop_aux acc pr) pr_set acc
  and free_vars_simple_internal_iprop_multiset_aux acc ipr_mset =
    SimpleIpropMset.fold
      (fun (_, tm_arr) _ acc -> free_vars_internal_term_array_aux acc tm_arr)
      ipr_mset acc
  in

  ( (fun tm ->
      free_vars_internal_term_aux [] tm |> List.sort_uniq compare_typed_var_id),
    (fun tm_arr ->
      free_vars_internal_term_array_aux [] tm_arr
      |> List.sort_uniq compare_typed_var_id),
    (fun pr ->
      free_vars_internal_prop_aux [] pr |> List.sort_uniq compare_typed_var_id),
    (fun ipr ->
      free_vars_simple_internal_iprop_aux [] ipr
      |> List.sort_uniq compare_typed_var_id),
    (fun ipr ->
      free_vars_internal_iprop_aux [] ipr |> List.sort_uniq compare_typed_var_id),
    (fun pr_set ->
      free_vars_internal_prop_set_aux [] pr_set
      |> List.sort_uniq compare_typed_var_id),
    fun ipr_mset ->
      free_vars_simple_internal_iprop_multiset_aux [] ipr_mset
      |> List.sort_uniq compare_typed_var_id )

(** Substitution. *)

type subst_task = internal_term_desc -> internal_term option

let ( subst_internal_term,
      subst_internal_term_array,
      subst_internal_prop,
      subst_simple_internal_iprop,
      subst_internal_iprop,
      subst_internal_prop_set,
      subst_simple_internal_iprop_multiset ) =
  let rec subst_internal_term_aux shift subst_task ({ desc; ity } as tm) =
    match subst_task desc with
    | Some subst_tm -> subst_tm
    | None -> (
        match desc with
        | IConstr (constr, tm_arr) ->
            iConstr
              ( constr,
                subst_internal_term_array_aux shift subst_task tm_arr,
                ity )
        | IFunc (func, tm_arr) ->
            iFunc
              (func, subst_internal_term_array_aux shift subst_task tm_arr, ity)
        | _ -> tm)
  and subst_internal_term_array_aux shift subst_task tm_arr =
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
        iPred (pred, subst_internal_term_array_aux shift subst_task tm_arr)
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
    | ISimple (ipr, ipr_disj) ->
        iSimple
          ( subst_simple_internal_iprop_aux shift subst_task ipr,
            List.map (subst_simple_internal_iprop_aux shift subst_task) ipr_disj
          )
    | IWand (ipr1, ipr2) ->
        iWand
          ( subst_internal_iprop_aux shift subst_task ipr1,
            subst_internal_iprop_aux shift subst_task ipr2 )
    | IHForall (binder_info, ipr) ->
        iHForall
          ( binder_info,
            subst_internal_iprop_aux (shift + binder_info.shift) subst_task ipr
          )
    | IHExists (binder_info, ipr) ->
        iHExists
          ( binder_info,
            subst_internal_iprop_aux (shift + binder_info.shift) subst_task ipr
          )
  and subst_internal_prop_set_aux shift subst_task pr_set =
    PropSet.map (subst_internal_prop_aux shift subst_task) pr_set
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
    subst_simple_internal_iprop_multiset_aux 0 )
