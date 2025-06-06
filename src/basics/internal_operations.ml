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

(** Pattern match with quantifiers. *)

type match_result = internal_term option array

let match_result_complete = Array.for_all Option.is_some

open Monads.ListMonad

let ( internal_term_match,
      internal_term_array_match,
      internal_prop_match,
      internal_iprop_match,
      internal_prop_set_match,
      simple_internal_iprop_multiset_match,
      internal_prop_set_substract_match,
      simple_internal_iprop_multiset_substract_match ) =
  let rec internal_term_match_aux st_opt shift match_result
      ({ desc = pdesc; _ } as ptm) ({ desc; _ } as tm) =
    match (pdesc, desc) with
    | IVar var1, IVar var2 ->
        if VarId.equal var1 var2 then return match_result else fail
    | IBVar ind, _ -> (
        match match_result.(ind - shift) with
        | Some res ->
            if
              compare_internal_term res tm = 0
              || Z3_intf.equality_solver st_opt res tm
            then return match_result
            else fail
        | None ->
            let match_result' = Array.copy match_result in
            let () = match_result'.(ind - shift) <- Some tm in
            return match_result')
    | IConstr (constr1, tm_arr1), IConstr (constr2, tm_arr2) ->
        if ConstrId.equal constr1 constr2 then
          internal_term_array_match_aux st_opt shift match_result tm_arr1
            tm_arr2
        else fail
    | IFunc (func1, tm_arr1), IFunc (func2, tm_arr2) ->
        if FuncId.equal func1 func2 then
          internal_term_array_match_aux st_opt shift match_result tm_arr1
            tm_arr2
        else fail
    | _, _ -> fail
  and internal_term_array_match_aux st_opt shift match_result ptm_arr tm_arr =
    let match_result = ref (return match_result) in
    Array.iter2
      (fun ptm tm ->
        match_result :=
          let* match_result' = !match_result in
          internal_term_match_aux st_opt shift match_result' ptm tm)
      ptm_arr tm_arr;
    !match_result
  in
  let rec internal_prop_match_aux st_opt shift match_result ppr pr =
    match (ppr, pr) with
    | IPersistent ipr1, IPersistent ipr2 ->
        internal_iprop_match_aux st_opt shift match_result ipr1 ipr2
    | INot pr1, INot pr2 ->
        internal_prop_match_aux st_opt shift match_result pr1 pr2
    | IAnd pr_set1, IAnd pr_set2 ->
        if PropSet.cardinal pr_set1 = PropSet.cardinal pr_set2 then
          let+ match_result', _ =
            internal_prop_set_match_aux st_opt shift match_result pr_set1
              pr_set2
          in
          match_result'
        else fail
    | IOr (pr11, pr12), IOr (pr21, pr22)
    | IImply (pr11, pr12), IImply (pr21, pr22) ->
        let* match_result' =
          internal_prop_match_aux st_opt shift match_result pr11 pr21
        in
        internal_prop_match_aux st_opt shift match_result' pr12 pr22
    | IPred (pred1, tm_arr1), IPred (pred2, tm_arr2) ->
        if PredId.equal pred1 pred2 then
          internal_term_array_match_aux st_opt shift match_result tm_arr1
            tm_arr2
        else fail
    | IForall ({ shift = shift1; _ }, pr1), IForall ({ shift = shift2; _ }, pr2)
    | IExists ({ shift = shift1; _ }, pr1), IExists ({ shift = shift2; _ }, pr2)
      ->
        if shift1 = shift2 then
          internal_prop_match_aux st_opt (shift + shift1) match_result pr1 pr2
        else fail
    | IEq (tm11, tm12), IEq (tm21, tm22) | INeq (tm11, tm12), INeq (tm21, tm22)
      ->
        let* match_result' =
          internal_term_match_aux st_opt shift match_result tm11 tm21
        in
        internal_term_match_aux st_opt shift match_result' tm12 tm22
    | _, _ -> fail
  and internal_iprop_match_aux st_opt shift match_result pipr ipr =
    match (pipr, ipr) with
    | ISimple ((ipr_mset1, pr_set1), []), ISimple ((ipr_mset2, pr_set2), []) ->
        if
          SimpleIpropMset.cardinal ipr_mset1
          = SimpleIpropMset.cardinal ipr_mset2
          && PropSet.cardinal pr_set1 = PropSet.cardinal pr_set2
        then
          let* match_result', _, _ =
            simple_internal_iprop_multiset_match_aux st_opt shift match_result
              ipr_mset1 ipr_mset2
          in
          let+ match_result'', _ =
            internal_prop_set_match_aux st_opt shift match_result' pr_set1
              pr_set2
          in
          match_result''
        else fail
    | IWand (ipr11, ipr12), IWand (ipr21, ipr22) ->
        let* match_result' =
          internal_iprop_match_aux st_opt shift match_result ipr11 ipr21
        in
        internal_iprop_match_aux st_opt shift match_result' ipr12 ipr22
    | ( IHForall ({ shift = shift1; _ }, ipr1),
        IHForall ({ shift = shift2; _ }, ipr2) )
    | ( IHExists ({ shift = shift1; _ }, ipr1),
        IHExists ({ shift = shift2; _ }, ipr2) ) ->
        if shift1 = shift2 then
          internal_iprop_match_aux st_opt (shift + shift1) match_result ipr1
            ipr2
        else fail
    | _, _ -> fail
  and internal_prop_set_match_aux st_opt shift match_result ppr_set pr_set =
    let match_result_and_pr_set = ref (return (match_result, pr_set)) in
    PropSet.iter
      (fun ppr ->
        match ppr with
        | IEq ({ desc = IBVar _; _ }, _) | IEq (_, { desc = IBVar _; _ }) -> ()
        | _ ->
            match_result_and_pr_set :=
              let* match_result', pr_set' = !match_result_and_pr_set in
              internal_prop_set_substract_match_aux st_opt shift match_result'
                ppr pr_set')
      ppr_set;
    PropSet.iter
      (fun ppr ->
        let _ =
          let* match_result', _ = !match_result_and_pr_set in
          match ppr with
          | IEq ({ desc = IBVar ind1; _ }, { desc = IBVar ind2; _ }) ->
              if
                not
                  (Z3_intf.equality_solver st_opt
                     (Option.get match_result'.(ind1))
                     (Option.get match_result'.(ind2)))
              then match_result_and_pr_set := fail;
              fail
          | IEq ({ desc = IBVar ind1; _ }, tm2) ->
              if
                not
                  (Z3_intf.equality_solver st_opt
                     (Option.get match_result'.(ind1))
                     tm2)
              then match_result_and_pr_set := fail;
              fail
          | IEq (tm1, { desc = IBVar ind2; _ }) ->
              if
                not
                  (Z3_intf.equality_solver st_opt tm1
                     (Option.get match_result'.(ind2)))
              then match_result_and_pr_set := fail;
              fail
          | _ -> fail
        in
        ())
      ppr_set;
    !match_result_and_pr_set
  and simple_internal_iprop_multiset_match_aux st_opt shift match_result
      pipr_mset ipr_mset :
      (match_result * simple_internal_iprop_multiset * bool) t =
    let match_result_and_ipr_mset_and_is_inf =
      ref (return (match_result, ipr_mset, true))
    in
    SimpleIpropMset.iter
      (fun pipr count ->
        let count_is_inf = Multiplicity.is_infinite count in
        for _ = 1 to Multiplicity.to_int_default 1 count do
          match_result_and_ipr_mset_and_is_inf :=
            let* match_result', ipr_mset', is_inf =
              !match_result_and_ipr_mset_and_is_inf
            in
            let+ match_result'', ipr_mset'', is_inf' =
              simple_internal_iprop_multiset_substract_match_aux st_opt shift
                match_result' pipr count_is_inf ipr_mset'
            in
            (match_result'', ipr_mset'', is_inf && is_inf')
        done)
      pipr_mset;
    !match_result_and_ipr_mset_and_is_inf
  and internal_prop_set_substract_match_aux st_opt shift match_result ppr pr_set
      : (match_result * internal_prop_set) t =
    PropSet.fold
      (fun pr acc ->
        (let+ match_result' =
           internal_prop_match_aux st_opt shift match_result ppr pr
         in
         (match_result', PropSet.remove pr pr_set))
        |> choose acc)
      pr_set fail
  and simple_internal_iprop_multiset_substract_match_aux st_opt shift
      match_result (phpred, ptm_arr) is_inf ipr_mset :
      (match_result * simple_internal_iprop_multiset * bool) t =
    let pcount = if is_inf then Multiplicity.inf else Multiplicity.one in
    SimpleIpropMset.fold
      (fun ((hpred, tm_arr) as ipr) count acc ->
        (if HPredId.equal phpred hpred && Multiplicity.compare pcount count <= 0
         then
           let+ match_result' =
             internal_term_array_match_aux st_opt shift match_result ptm_arr
               tm_arr
           in
           ( match_result',
             SimpleIpropMset.remove ipr pcount ipr_mset,
             Multiplicity.is_infinite count )
         else fail)
        |> choose acc)
      ipr_mset fail
  in

  ( (fun st_opt -> internal_term_match_aux st_opt 0),
    (fun st_opt -> internal_term_array_match_aux st_opt 0),
    (fun st_opt -> internal_prop_match_aux st_opt 0),
    (fun st_opt -> internal_iprop_match_aux st_opt 0),
    (fun st_opt -> internal_prop_set_match_aux st_opt 0),
    (fun st_opt -> simple_internal_iprop_multiset_match_aux st_opt 0),
    (fun st_opt -> internal_prop_set_substract_match_aux st_opt 0),
    fun st_opt -> simple_internal_iprop_multiset_substract_match_aux st_opt 0 )
