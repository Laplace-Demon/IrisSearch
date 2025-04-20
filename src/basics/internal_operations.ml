open Ast
open Internal

let term_to_internal, prop_to_internal, iprop_to_internal =
  let term_to_internal ?(env = []) : term -> internal_term = function
    | Var var -> (
        match List.find_index (String.equal var) env with
        | Some i -> iVar (Int.to_string i)
        | None -> iVar var)
  in
  let rec prop_to_internal ?(env = []) : prop -> internal_prop = function
    | Persistent ipr -> iPersistent (iprop_to_internal ~env ipr)
    | Not pr -> iNot (prop_to_internal ~env pr)
    | And (pr1, pr2) ->
        let pr_set1 =
          match prop_to_internal ~env pr1 with
          | IAnd pr_set -> pr_set
          | _ as pr -> PropSet.singleton pr
        in
        let pr_set2 =
          match prop_to_internal ~env pr2 with
          | IAnd pr_set -> pr_set
          | _ as pr -> PropSet.singleton pr
        in
        iAnd (PropSet.union pr_set1 pr_set2)
    | Or (pr1, pr2) -> iOr (prop_to_internal ~env pr1, prop_to_internal ~env pr2)
    | Imply (pr1, pr2) ->
        iImply (prop_to_internal ~env pr1, prop_to_internal ~env pr2)
    | Pred (pred, param_list) ->
        iPred (pred, List.map (term_to_internal ~env) param_list)
    | Forall (typed_str_list, pr) ->
        iForall
          ( typed_str_list,
            prop_to_internal
              ~env:
                (List.fold_left
                   (fun acc (str, _) -> str :: acc)
                   env typed_str_list)
              pr )
    | Eq (tm1, tm2) -> iEq (term_to_internal ~env tm1, term_to_internal ~env tm2)
    | Neq (tm1, tm2) ->
        iNeq (term_to_internal ~env tm1, term_to_internal ~env tm2)
  and iprop_to_internal ?(env = []) : iprop -> internal_iprop = function
    | False -> iFalse
    | Atom atom -> iAtom atom
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
    | HPred (hpred, param_list) ->
        iHPred (hpred, List.map term_to_internal param_list)
    | HForall (typed_str_list, ipr) ->
        iHForall
          ( typed_str_list,
            iprop_to_internal
              ~env:
                (List.fold_left
                   (fun acc (str, _) -> str :: acc)
                   env typed_str_list)
              ipr )
  in
  (term_to_internal ~env:[], prop_to_internal ~env:[], iprop_to_internal ~env:[])

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

(** Pattern match with quantifiers. *)

(* let rec prop_match ppr pr =
  match ppr, pr with
  | IPersistent of internal_iprop
  | INot of internal_prop
  | IAnd of PropSet.t
  | IOr of internal_prop * internal_prop
  | IImply of internal_prop * internal_prop
  | IPred of pred_id * internal_term list
  | IForall of binder_info * internal_prop
  | IEq of internal_term * internal_term
  | INeq of internal_term * internal_term
  | _, _ -> None *)
