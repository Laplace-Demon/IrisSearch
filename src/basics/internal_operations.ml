open Ast
open Internal
open Format

let term_to_internal, prop_to_internal, iprop_to_internal =
  let term_to_internal_aux ~env : term -> internal_term = function
    | Var var -> (
        match List.find_index (String.equal var) env with
        | Some i -> iBVar i
        | None -> iVar_str var)
  in
  let rec prop_to_internal_aux ~env : prop -> internal_prop = function
    | Persistent ipr -> iPersistent (iprop_to_internal_aux ~env ipr)
    | Not pr -> iNot (prop_to_internal_aux ~env pr)
    | And (pr1, pr2) ->
        let pr_set1 =
          match prop_to_internal_aux ~env pr1 with
          | IAnd pr_set -> pr_set
          | _ as pr -> PropSet.singleton pr
        in
        let pr_set2 =
          match prop_to_internal_aux ~env pr2 with
          | IAnd pr_set -> pr_set
          | _ as pr -> PropSet.singleton pr
        in
        iAnd (PropSet.union pr_set1 pr_set2)
    | Or (pr1, pr2) ->
        iOr (prop_to_internal_aux ~env pr1, prop_to_internal_aux ~env pr2)
    | Imply (pr1, pr2) ->
        iImply (prop_to_internal_aux ~env pr1, prop_to_internal_aux ~env pr2)
    | Pred (pred, param_list) ->
        iPred_str (pred, List.map (term_to_internal_aux ~env) param_list)
    | Forall (typed_str_list, pr) ->
        iForall_raw
          ( typed_str_list,
            prop_to_internal_aux
              ~env:
                (List.fold_left
                   (fun acc (str, _) -> str :: acc)
                   env typed_str_list)
              pr )
    | Eq (tm1, tm2) ->
        iEq (term_to_internal_aux ~env tm1, term_to_internal_aux ~env tm2)
    | Neq (tm1, tm2) ->
        iNeq (term_to_internal_aux ~env tm1, term_to_internal_aux ~env tm2)
  and iprop_to_internal_aux ~env : iprop -> internal_iprop = function
    | False -> iFalse
    | Atom atom -> iAtom_str atom
    | Pure pr -> iPure (prop_to_internal_aux ~env pr)
    | Star (ipr1, ipr2) ->
        let ipr_mset1 =
          match iprop_to_internal_aux ~env ipr1 with
          | IStar ipr_mset -> ipr_mset
          | _ as ipr -> IpropMset.singleton ipr Multiplicity.one
        in
        let ipr_mset2 =
          match iprop_to_internal_aux ~env ipr2 with
          | IStar ipr_mset -> ipr_mset
          | _ as ipr -> IpropMset.singleton ipr Multiplicity.one
        in
        iStar (IpropMset.union ipr_mset1 ipr_mset2)
    | Wand (ipr1, ipr2) ->
        iWand (iprop_to_internal_aux ~env ipr1, iprop_to_internal_aux ~env ipr2)
    | Box ipr -> (
        match iprop_to_internal_aux ~env ipr with
        | IStar ipr_mset ->
            iStar
              (IpropMset.map_multiplicity
                 (fun _ _ -> Multiplicity.inf)
                 ipr_mset)
        | _ as ipr -> iStar (IpropMset.singleton ipr Multiplicity.inf))
    | HPred (hpred, param_list) ->
        iHPred_str (hpred, List.map (term_to_internal_aux ~env) param_list)
    | HForall (typed_str_list, ipr) ->
        iHForall_raw
          ( typed_str_list,
            iprop_to_internal_aux
              ~env:
                (List.fold_left
                   (fun acc (str, _) -> str :: acc)
                   env typed_str_list)
              ipr )
  in
  ( term_to_internal_aux ~env:[],
    prop_to_internal_aux ~env:[],
    iprop_to_internal_aux ~env:[] )

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

(** Substitution of quantified variable. *)

type subst_task = (int * internal_term) list

let ( subst_internal_term,
      subst_internal_prop,
      subst_internal_iprop,
      subst_internal_prop_set,
      subst_internal_iprop_mset ) =
  let subst_internal_term_aux shift subst_task tm =
    match tm with
    | IBVar ind -> (
        match List.assoc_opt (ind + shift) subst_task with
        | Some subst_tm -> subst_tm
        | None -> tm)
    | _ -> tm
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
    | IPred (pred, param_list) ->
        iPred
          (pred, List.map (subst_internal_term_aux shift subst_task) param_list)
    | IForall (binder_info, pr) ->
        iForall
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
  and subst_internal_iprop_aux shift subst_task = function
    | IPure pr -> iPure (subst_internal_prop_aux shift subst_task pr)
    | IStar ipr_mset ->
        iStar (subst_internal_iprop_mset_aux shift subst_task ipr_mset)
    | IWand (ipr1, ipr2) ->
        iWand
          ( subst_internal_iprop_aux shift subst_task ipr1,
            subst_internal_iprop_aux shift subst_task ipr2 )
    | IHPred (hpred, param_list) ->
        iHPred
          (hpred, List.map (subst_internal_term_aux shift subst_task) param_list)
    | IHForall (binder_info, ipr) ->
        iHForall
          ( binder_info,
            subst_internal_iprop_aux (shift + binder_info.shift) subst_task ipr
          )
    | _ as ipr -> ipr
  and subst_internal_prop_set_aux shift subst_task pr_set =
    PropSet.map (subst_internal_prop_aux shift subst_task) pr_set
  and subst_internal_iprop_mset_aux shift subst_task ipr_mset =
    IpropMset.map (subst_internal_iprop_aux shift subst_task) ipr_mset
  in
  ( subst_internal_term_aux 0,
    subst_internal_prop_aux 0,
    subst_internal_iprop_aux 0,
    subst_internal_prop_set_aux 0,
    subst_internal_iprop_mset_aux 0 )

(** Pattern match with quantifiers. *)

open Monads.OptionMonad

let rec list_fold_left2_option (f : 'acc -> 'a -> 'b -> 'acc option)
    (acc : 'acc) (la : 'a list) (lb : 'b list) : 'acc option =
  match (la, lb) with
  | [], [] -> return acc
  | a :: la', b :: lb' ->
      let* acc' = f acc a b in
      list_fold_left2_option f acc' la' lb'
  | _, _ -> None

let rec list_iter_option (f : 'a -> unit option) (l : 'a list) : unit option =
  match l with
  | [] -> Some ()
  | a :: l' ->
      let* res = f a in
      list_iter_option f l'

let array_fold_left_i f x a =
  let open Array in
  let r = ref x in
  for i = 0 to length a - 1 do
    r := f i !r (unsafe_get a i)
  done;
  !r

type match_result = (int * internal_term) list

(* intermediate list *)

let internal_term_match, internal_prop_match, internal_iprop_match =
  let rec internal_term_match_aux range shift acc ptm tm =
    printf "@.find: %a %a@." pp_internal_term ptm pp_internal_term tm;
    match (ptm, tm) with
    | IBVar ind, _ when shift <= ind && ind < shift + range ->
        return ((ind - shift, tm) :: acc)
    | IVar var1, IVar var2 -> if VarId.equal var1 var2 then return acc else None
    | IBVar ind1, IBVar ind2 -> if Int.equal ind1 ind2 then return acc else None
    | _, _ -> None
  in
  let rec internal_prop_match_aux range shift acc ppr pr =
    match (ppr, pr) with
    | IPersistent ipr1, IPersistent ipr2 ->
        internal_iprop_match_aux range shift acc ipr1 ipr2
    | INot pr1, INot pr2 -> internal_prop_match_aux range shift acc pr1 pr2
    | IAnd pr_set1, IAnd pr_set2 ->
        list_fold_left2_option
          (internal_prop_match_aux range shift)
          acc (PropSet.to_list pr_set1) (PropSet.to_list pr_set2)
    | IOr (pr11, pr12), IOr (pr21, pr22) ->
        let* acc' = internal_prop_match_aux range shift acc pr11 pr21 in
        internal_prop_match_aux range shift acc' pr12 pr22
    | IImply (pr11, pr12), IImply (pr21, pr22) ->
        let* acc' = internal_prop_match_aux range shift acc pr11 pr21 in
        internal_prop_match_aux range shift acc' pr12 pr22
    | IPred (pred1, param_list1), IPred (pred2, param_list2) ->
        if PredId.equal pred1 pred2 then
          list_fold_left2_option
            (internal_term_match_aux range shift)
            acc param_list1 param_list2
        else None
    | IForall ({ shift = shift1 }, pr1), IForall ({ shift = shift2 }, pr2) ->
        if Int.equal shift1 shift2 then
          internal_prop_match_aux range (shift + shift1) acc pr1 pr2
        else None
    | IEq (tm11, tm12), IEq (tm21, tm22) | INeq (tm11, tm12), INeq (tm21, tm22)
      ->
        let* acc' = internal_term_match_aux range shift acc tm11 tm21 in
        internal_term_match_aux range shift acc' tm12 tm22
    | _, _ -> None
  and internal_iprop_match_aux range shift acc pipr ipr =
    match (pipr, ipr) with
    | IFalse, IFalse -> return acc
    | IAtom atom1, IAtom atom2 ->
        if AtomId.equal atom1 atom2 then return acc else None
    | IPure pr1, IPure pr2 -> internal_prop_match_aux range shift acc pr1 pr2
    | IStar ipr_mset1, IStar ipr_mset2 ->
        list_fold_left2_option
          (fun acc (ipr1, count1) (ipr2, count2) ->
            if Multiplicity.equal count1 count2 then
              internal_iprop_match_aux range shift acc ipr1 ipr2
            else None)
          acc
          (IpropMset.to_list ipr_mset1)
          (IpropMset.to_list ipr_mset2)
    | IWand (ipr11, ipr12), IWand (ipr21, ipr22) ->
        let* acc' = internal_iprop_match_aux range shift acc ipr11 ipr21 in
        internal_iprop_match_aux range shift acc' ipr12 ipr22
    | IHPred (hpred1, param_list1), IHPred (hpred2, param_list2) ->
        if HPredId.equal hpred1 hpred2 then
          list_fold_left2_option
            (internal_term_match_aux range shift)
            acc param_list1 param_list2
        else None
    | IHForall ({ shift = shift1 }, ipr1), IHForall ({ shift = shift2 }, ipr2)
      ->
        if Int.equal shift1 shift2 then
          internal_iprop_match_aux range (shift + shift1) acc ipr1 ipr2
        else None
    | _, _ -> None
  in
  let internal_term_match range ptm tm =
    let* raw_match_result = internal_term_match_aux range 0 [] ptm tm in
    let match_result = Array.init range (fun _ -> None) in
    let* () =
      list_iter_option
        (fun (ind, tm) ->
          match match_result.(ind) with
          | None ->
              match_result.(ind) <- Some tm;
              Some ()
          | Some tm' -> if internal_term_eqb tm tm' then Some () else None)
        raw_match_result
    in
    return
      (array_fold_left_i
         (fun i acc tm_opt ->
           match tm_opt with None -> acc | Some tm -> (i, tm) :: acc)
         [] match_result)
  in
  let internal_prop_match range ppr pr =
    let* raw_match_result = internal_prop_match_aux range 0 [] ppr pr in
    let match_result = Array.init range (fun _ -> None) in
    let* () =
      list_iter_option
        (fun (ind, tm) ->
          match match_result.(ind) with
          | None ->
              match_result.(ind) <- Some tm;
              Some ()
          | Some tm' -> if internal_term_eqb tm tm' then Some () else None)
        raw_match_result
    in
    return
      (array_fold_left_i
         (fun i acc tm_opt ->
           match tm_opt with None -> acc | Some tm -> (i, tm) :: acc)
         [] match_result)
  in
  let internal_iprop_match range pipr ipr =
    match ipr with
    | IHForall (_, ipr) | (_ as ipr) ->
        let* raw_match_result = internal_iprop_match_aux range 0 [] pipr ipr in
        printf "@.raw match result:@.%a@."
          (pp_print_list (fun fmt (i, tm) ->
               fprintf fmt "%i:%a" i pp_internal_term tm))
          raw_match_result;

        let match_result = Array.init range (fun _ -> None) in
        let* () =
          list_iter_option
            (fun (ind, tm) ->
              match match_result.(ind) with
              | None ->
                  match_result.(ind) <- Some tm;
                  Some ()
              | Some tm' -> if internal_term_eqb tm tm' then Some () else None)
            raw_match_result
        in
        return
          (array_fold_left_i
             (fun i acc tm_opt ->
               match tm_opt with None -> acc | Some tm -> (i, tm) :: acc)
             [] match_result)
  in
  (internal_term_match, internal_prop_match, internal_iprop_match)

(** These two functions are extremely fragile in that they exploit the implicit
    ordering of constructors. *)

let rec internal_prop_shape_lower_bound = function
  | IPersistent _ -> iPersistent iIPropMin
  | INot _ -> iNot iPropMin
  | IAnd _ -> iNot iPropMax
  | IOr _ -> iOr (iPropMin, iPropMin)
  | IImply _ -> iImply (iPropMin, iPropMin)
  | IPred (pred, _) -> iPred (pred, [])
  | IForall (_, pr) -> internal_prop_shape_lower_bound pr
  | IEq _ -> iEq (iTermMin, iTermMin)
  | INeq _ -> iNeq (iTermMin, iTermMin)

let rec internal_prop_shape_upper_bound = function
  | IPersistent _ -> iPersistent iIPropMax
  | INot _ -> iNot iPropMax
  | IAnd _ -> iOr (iPropMin, iPropMin)
  | IOr _ -> iOr (iPropMax, iPropMax)
  | IImply _ -> iImply (iPropMax, iPropMax)
  | IPred (pred, _) -> iPred (pred, [ iTermMax ])
  | IForall (_, pr) -> internal_prop_shape_upper_bound pr
  | IEq _ -> iEq (iTermMax, iTermMax)
  | INeq _ -> iNeq (iTermMax, iTermMax)

let rec internal_iprop_shape_lower_bound = function
  | IFalse -> iIPropMin
  | IAtom atom -> iFalse
  | IPure _ -> iPure iPropMin
  | IStar _ -> iPure iPropMax
  | IWand _ -> iWand (iIPropMin, iIPropMin)
  | IHPred (hpred, _) -> iHPred (hpred, [])
  | IHForall (_, ipr) -> internal_iprop_shape_lower_bound ipr

let rec internal_iprop_shape_upper_bound = function
  | IFalse -> iAtom AtomId.min
  | IAtom atom -> iFalse
  | IPure _ -> iPure iPropMax
  | IStar _ -> iWand (iIPropMin, iIPropMin)
  | IWand _ -> iWand (iIPropMax, iIPropMax)
  | IHPred (hpred, _) -> iHPred (hpred, [ iTermMax ])
  | IHForall (_, ipr) -> internal_iprop_shape_upper_bound ipr

let reduce_internal_prop_set pr pr_set =
  let lower_bound = internal_prop_shape_lower_bound pr in
  let upper_bound = internal_prop_shape_upper_bound pr in

  printf "@.original set:@.%a@."
    (pp_internal_prop_set ~pp_sep:pp_print_newline)
    pr_set;

  let _, _, reduced_pr_set = PropSet.split lower_bound pr_set in

  printf "@.reduced set:@.%a@."
    (pp_internal_prop_set ~pp_sep:pp_print_newline)
    reduced_pr_set;

  let reduced_pr_set, _, _ = PropSet.split upper_bound reduced_pr_set in

  printf "@.reduced set:@.%a@."
    (pp_internal_prop_set ~pp_sep:pp_print_newline)
    reduced_pr_set;

  return reduced_pr_set

let reduce_internal_iprop_multiset ipr ipr_mset =
  let lower_bound = internal_iprop_shape_lower_bound ipr in
  let upper_bound = internal_iprop_shape_upper_bound ipr in

  printf "@.original multiset:@.%a@."
    (pp_internal_iprop_multiset ~pp_sep:pp_print_newline)
    ipr_mset;

  let _, _, reduced_ipr_mset = IpropMset.split lower_bound ipr_mset in

  printf "@.reduced multiset:@.%a@."
    (pp_internal_iprop_multiset ~pp_sep:pp_print_newline)
    reduced_ipr_mset;

  let reduced_ipr_mset, _, _ = IpropMset.split upper_bound reduced_ipr_mset in

  printf "@.reduced multiset:@.%a@."
    (pp_internal_iprop_multiset ~pp_sep:pp_print_newline)
    reduced_ipr_mset;

  return reduced_ipr_mset

let internal_prop_match_multiple range pr pr_set =
  match reduce_internal_prop_set pr pr_set with
  | None -> []
  | Some reduced_pr_set ->
      PropSet.fold
        (fun pr' acc ->
          match internal_prop_match range pr pr' with
          | Some match_result -> match_result :: acc
          | None -> acc)
        reduced_pr_set []

let internal_iprop_match_multiple range ipr ipr_mset =
  match reduce_internal_iprop_multiset ipr ipr_mset with
  | None -> []
  | Some reduced_ipr_mset ->
      IpropMset.fold
        (fun ipr' _ acc ->
          match internal_iprop_match range ipr ipr' with
          | Some match_result -> match_result :: acc
          | None -> acc)
        reduced_ipr_mset []
