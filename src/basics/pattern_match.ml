open Internal
open State

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
