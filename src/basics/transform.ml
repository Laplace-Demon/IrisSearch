open Ast

let rec uncurry_prop = function
  | Persistent ipr -> Persistent (uncurry_iprop ipr)
  | Not pr -> Not (uncurry_prop pr)
  | And (pr1, pr2) -> And (uncurry_prop pr1, uncurry_prop pr2)
  | Or (pr1, pr2) -> Or (uncurry_prop pr1, uncurry_prop pr2)
  | Imply (pr1, Imply (pr21, pr22)) ->
      uncurry_prop (Imply (And (pr1, pr21), pr22))
  | Imply (pr1, pr2) -> Imply (uncurry_prop pr1, uncurry_prop pr2)
  | Forall (typed_str_list, pr) -> Forall (typed_str_list, uncurry_prop pr)
  | Exists (typed_str_list, pr) -> Exists (typed_str_list, uncurry_prop pr)
  | _ as pr -> pr

and uncurry_iprop = function
  | Pure pr -> Pure (uncurry_prop pr)
  | Star (ipr1, ipr2) -> Star (uncurry_iprop ipr1, uncurry_iprop ipr2)
  | Wand (ipr1, Wand (ipr21, ipr22)) ->
      uncurry_iprop (Wand (Star (ipr1, ipr21), ipr22))
  | Wand (ipr1, ipr2) -> Wand (uncurry_iprop ipr1, uncurry_iprop ipr2)
  | Box ipr -> Box (uncurry_iprop ipr)
  | HForall (typed_str_list, ipr) -> HForall (typed_str_list, uncurry_iprop ipr)
  | HExists (typed_str_list, ipr) -> HExists (typed_str_list, uncurry_iprop ipr)
  | _ as ipr -> ipr

let uncurry_transformation ins =
  let decl_facts = List.map uncurry_prop ins.decl_facts in
  let decl_laws =
    List.map
      (fun (name_opt, ipr) -> (name_opt, uncurry_iprop ipr))
      ins.decl_laws
  in
  let decl_init = List.map uncurry_iprop ins.decl_init in
  { ins with decl_facts; decl_laws; decl_init }

let rec merge_quantifier_prop = function
  | Persistent ipr -> Persistent (merge_quantifier_iprop ipr)
  | Not pr -> Not (merge_quantifier_prop pr)
  | And (pr1, pr2) -> And (merge_quantifier_prop pr1, merge_quantifier_prop pr2)
  | Or (pr1, pr2) -> Or (merge_quantifier_prop pr1, merge_quantifier_prop pr2)
  | Imply (pr1, pr2) ->
      Imply (merge_quantifier_prop pr1, merge_quantifier_prop pr2)
  | Forall (typed_str_list1, Forall (typed_str_list2, pr)) ->
      merge_quantifier_prop (Forall (typed_str_list1 @ typed_str_list2, pr))
  | Forall (typed_str_list, pr) ->
      Forall (typed_str_list, merge_quantifier_prop pr)
  | Exists (typed_str_list1, Exists (typed_str_list2, pr)) ->
      merge_quantifier_prop (Exists (typed_str_list1 @ typed_str_list2, pr))
  | Exists (typed_str_list, pr) ->
      Exists (typed_str_list, merge_quantifier_prop pr)
  | _ as pr -> pr

and merge_quantifier_iprop = function
  | Pure pr -> Pure (merge_quantifier_prop pr)
  | Star (ipr1, ipr2) ->
      Star (merge_quantifier_iprop ipr1, merge_quantifier_iprop ipr2)
  | Wand (ipr1, ipr2) -> Wand (merge_quantifier_iprop ipr1, uncurry_iprop ipr2)
  | Box ipr -> Box (merge_quantifier_iprop ipr)
  | HForall (typed_str_list1, HForall (typed_str_list2, ipr)) ->
      merge_quantifier_iprop (HForall (typed_str_list1 @ typed_str_list2, ipr))
  | HForall (typed_str_list, ipr) ->
      HForall (typed_str_list, merge_quantifier_iprop ipr)
  | HExists (typed_str_list1, HExists (typed_str_list2, ipr)) ->
      merge_quantifier_iprop (HExists (typed_str_list1 @ typed_str_list2, ipr))
  | HExists (typed_str_list, ipr) ->
      HExists (typed_str_list, merge_quantifier_iprop ipr)
  | _ as ipr -> ipr

let merge_quantifier_transformation ins =
  let decl_facts = List.map merge_quantifier_prop ins.decl_facts in
  let decl_laws =
    List.map
      (fun (name_opt, ipr) -> (name_opt, merge_quantifier_iprop ipr))
      ins.decl_laws
  in
  let decl_init = List.map merge_quantifier_iprop ins.decl_init in
  { ins with decl_facts; decl_laws; decl_init }
