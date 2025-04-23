open Ast

let rec uncurry_prop = function
  | Persistent ipr -> Persistent (uncurry_iprop ipr)
  | Not pr -> Not (uncurry_prop pr)
  | And (pr1, pr2) -> And (uncurry_prop pr1, uncurry_prop pr2)
  | Or (pr1, pr2) -> Or (uncurry_prop pr1, uncurry_prop pr2)
  | Imply (pr1, Imply (pr21, pr22)) ->
      uncurry_prop (Imply (And (pr1, pr21), pr22))
  | Imply (pr1, pr2) -> Imply (uncurry_prop pr1, uncurry_prop pr2)
  | _ as pr -> pr

and uncurry_iprop = function
  | Pure pr -> Pure (uncurry_prop pr)
  | Star (ipr1, ipr2) -> Star (uncurry_iprop ipr1, uncurry_iprop ipr2)
  | Wand (ipr1, Wand (ipr21, ipr22)) ->
      uncurry_iprop (Wand (Star (ipr1, ipr21), ipr22))
  | Wand (ipr1, ipr2) -> Wand (uncurry_iprop ipr1, uncurry_iprop ipr2)
  | Box ipr -> Box (uncurry_iprop ipr)
  | _ as ipr -> ipr

let uncurry_transformation
    { decl_types; decl_preds; decl_consts; decl_facts; decl_laws; decl_init } =
  let decl_facts = List.map uncurry_prop decl_facts in
  let decl_laws = List.map uncurry_iprop decl_laws in
  let decl_init = List.map uncurry_iprop decl_init in
  { decl_types; decl_preds; decl_consts; decl_facts; decl_laws; decl_init }

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
  | _ as ipr -> ipr

let merge_quantifier_transformation
    { decl_types; decl_preds; decl_consts; decl_facts; decl_laws; decl_init } =
  let decl_facts = List.map merge_quantifier_prop decl_facts in
  let decl_laws = List.map merge_quantifier_iprop decl_laws in
  let decl_init = List.map merge_quantifier_iprop decl_init in
  { decl_types; decl_preds; decl_consts; decl_facts; decl_laws; decl_init }

(* TODO: also substitute quantified Persistent *)
let eliminate_persistent_transformation ({ decl_facts } as ins) =
  let rec prop_subst_positive_var src dest = function
    | Persistent ipr -> Persistent (iprop_subst_positive_var src dest ipr)
    | Not pr -> Not (prop_subst_positive_var src dest pr)
    | And (pr1, pr2) ->
        And
          ( prop_subst_positive_var src dest pr1,
            prop_subst_positive_var src dest pr2 )
    | Or (pr1, pr2) ->
        Or
          ( prop_subst_positive_var src dest pr1,
            prop_subst_positive_var src dest pr2 )
    | Imply (pr1, pr2) ->
        Imply
          ( prop_subst_positive_var src dest pr1,
            prop_subst_positive_var src dest pr2 )
    | Forall (typed_str_list, pr) ->
        Forall (typed_str_list, prop_subst_positive_var src dest pr)
    | _ as pr -> pr
  and prop_subst_negative_var src dest = function
    | Persistent ipr -> Persistent (iprop_subst_negative_var src dest ipr)
    | Not pr -> Not (prop_subst_negative_var src dest pr)
    | And (pr1, pr2) ->
        And
          ( prop_subst_negative_var src dest pr1,
            prop_subst_negative_var src dest pr2 )
    | Or (pr1, pr2) ->
        Or
          ( prop_subst_negative_var src dest pr1,
            prop_subst_negative_var src dest pr2 )
    | Imply (pr1, pr2) ->
        Imply
          ( prop_subst_negative_var src dest pr1,
            prop_subst_negative_var src dest pr2 )
    | Forall (typed_str_list, pr) ->
        Forall (typed_str_list, prop_subst_negative_var src dest pr)
    | _ as pr -> pr
  and iprop_subst_positive_var src dest = function
    | Atom str -> if String.equal str src then dest else Atom str
    | Pure pr -> Pure (prop_subst_positive_var src dest pr)
    | Star (ipr1, ipr2) ->
        Star
          ( iprop_subst_positive_var src dest ipr1,
            iprop_subst_positive_var src dest ipr2 )
    | Wand (ipr1, ipr2) ->
        Wand
          ( iprop_subst_negative_var src dest ipr1,
            iprop_subst_positive_var src dest ipr2 )
    | Box ipr -> Box (iprop_subst_positive_var src dest ipr)
    | HForall (typed_str_list, ipr) ->
        HForall (typed_str_list, iprop_subst_positive_var src dest ipr)
    | _ as ipr -> ipr
  and iprop_subst_negative_var src dest = function
    | Atom str -> Atom str
    | Pure pr -> Pure (prop_subst_negative_var src dest pr)
    | Star (ipr1, ipr2) ->
        Star
          ( iprop_subst_negative_var src dest ipr1,
            iprop_subst_negative_var src dest ipr2 )
    | Wand (ipr1, ipr2) ->
        Wand
          ( iprop_subst_positive_var src dest ipr1,
            iprop_subst_negative_var src dest ipr2 )
    | Box ipr -> Box (iprop_subst_negative_var src dest ipr)
    | HForall (typed_str_list, ipr) ->
        HForall (typed_str_list, iprop_subst_negative_var src dest ipr)
    | _ as ipr -> ipr
  in
  let instance_subst_positive_var src dest
      { decl_types; decl_preds; decl_consts; decl_facts; decl_laws; decl_init }
      =
    {
      decl_types;
      decl_preds;
      decl_consts;
      decl_facts =
        List.map (fun pr -> prop_subst_positive_var src dest pr) decl_facts;
      decl_laws =
        List.map (fun ipr -> iprop_subst_positive_var src dest ipr) decl_laws;
      decl_init =
        List.map (fun ipr -> iprop_subst_positive_var src dest ipr) decl_init;
    }
  in
  List.fold_right
    (fun pr ins ->
      match pr with
      | Persistent (Atom str) ->
          instance_subst_positive_var str (Box (Atom str)) ins
      | _ -> { ins with decl_facts = pr :: ins.decl_facts })
    decl_facts
    { ins with decl_facts = [] }
