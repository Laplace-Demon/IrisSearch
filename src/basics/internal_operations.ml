open Ast
open Internal
open Format

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

type subst_task = (int * internal_term) list

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
        match List.assoc_opt (ind + shift) subst_task with
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

(* * Pattern match with quantifiers.

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
  done;open Monads.OptionMonad

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
        reduced_ipr_mset [] *)
