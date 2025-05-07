open Format
open Type

(** Definition of abstract syntax tree returned by the parser. *)

type term = Var of string | App of string * term list

let rec term_eqb tm1 tm2 =
  match (tm1, tm2) with
  | Var str1, Var str2 -> String.equal str1 str2
  | App (str1, tm_list1), App (str2, tm_list2) ->
      String.equal str1 str2 && List.equal term_eqb tm_list1 tm_list2
  | _, _ -> false

let rec pp_term fmt = function
  | Var str -> fprintf fmt "%s" str
  | App (str, tm_list) -> (
      match tm_list with
      | [] -> assert false
      | _ ->
          fprintf fmt "%s(%a)" str
            (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_term)
            tm_list)

type prop =
  | Persistent of iprop
  | Not of prop
  | And of prop * prop
  | Or of prop * prop
  | Imply of prop * prop
  | Pred of string * term list
  | Forall of (string * itype) list * prop
  | Exists of (string * itype) list * prop
  | Eq of term * term
  | Neq of term * term

and iprop =
  | False
  | Atom of string
  | Pure of prop
  | Star of iprop * iprop
  | Wand of iprop * iprop
  | Box of iprop
  | HPred of string * term list
  | HForall of (string * itype) list * iprop
  | HExists of (string * itype) list * iprop

let rec prop_eqb pr1 pr2 =
  match (pr1, pr2) with
  | Persistent ipr1, Persistent ipr2 -> iprop_eqb ipr1 ipr2
  | Not pr1, Not pr2 -> prop_eqb pr1 pr2
  | And (pr11, pr12), And (pr21, pr22)
  | Or (pr11, pr12), Or (pr21, pr22)
  | Imply (pr11, pr12), Imply (pr21, pr22) ->
      prop_eqb pr11 pr21 && prop_eqb pr12 pr22
  | Pred (str1, tm_list1), Pred (str2, tm_list2) ->
      String.equal str1 str2 && List.equal term_eqb tm_list1 tm_list2
  | Forall (typed_str_list1, pr1), Forall (typed_str_list2, pr2)
  | Exists (typed_str_list1, pr1), Exists (typed_str_list2, pr2) ->
      List.equal
        (fun (str1, ity1) (str2, ity2) ->
          String.equal str1 str2 && itype_eqb ity1 ity2)
        typed_str_list1 typed_str_list2
      && prop_eqb pr1 pr2
  | Eq (tm11, tm12), Eq (tm21, tm22) | Neq (tm11, tm12), Neq (tm21, tm22) ->
      term_eqb tm11 tm21 && term_eqb tm12 tm22
  | _, _ -> false

and iprop_eqb ipr1 ipr2 =
  match (ipr1, ipr2) with
  | False, False -> true
  | Atom str1, Atom str2 -> String.equal str1 str2
  | Pure pr1, Pure pr2 -> prop_eqb pr1 pr2
  | Star (ipr11, ipr12), Star (ipr21, ipr22)
  | Wand (ipr11, ipr12), Wand (ipr21, ipr22) ->
      iprop_eqb ipr11 ipr21 && iprop_eqb ipr12 ipr22
  | Box ipr1, Box ipr2 -> iprop_eqb ipr1 ipr2
  | HPred (str1, tm_list1), HPred (str2, tm_list2) ->
      String.equal str1 str2 && List.equal term_eqb tm_list1 tm_list2
  | HForall (typed_str_list1, ipr1), HForall (typed_str_list2, ipr2)
  | HExists (typed_str_list1, ipr1), HExists (typed_str_list2, ipr2) ->
      List.equal
        (fun (str1, ity1) (str2, ity2) ->
          String.equal str1 str2 && itype_eqb ity1 ity2)
        typed_str_list1 typed_str_list2
      && iprop_eqb ipr1 ipr2
  | _, _ -> false

let rec pp_prop fmt = function
  | Persistent ipr -> fprintf fmt "Persistent %a" pp_iprop ipr
  | Not pr -> fprintf fmt "¬ %a" pp_prop pr
  | And (pr1, pr2) -> fprintf fmt "%a ∧ %a" pp_prop pr1 pp_prop pr2
  | Or (pr1, pr2) -> fprintf fmt "%a ∨ %a" pp_prop pr1 pp_prop pr2
  | Imply (pr1, pr2) -> fprintf fmt "%a → %a" pp_prop pr1 pp_prop pr2
  | Pred (str, tm_list) ->
      fprintf fmt "%s %a" str
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_term)
        tm_list
  | Forall (typed_str_list, pr) ->
      fprintf fmt "forall %a, %a"
        (pp_typed_strs_list
           ~pp_sep:(fun fmt () -> fprintf fmt " ")
           ~pp_paren:true ())
        (group_typed_str typed_str_list)
        pp_prop pr
  | Exists (typed_str_list, pr) ->
      fprintf fmt "exists %a, %a"
        (pp_typed_strs_list
           ~pp_sep:(fun fmt () -> fprintf fmt " ")
           ~pp_paren:true ())
        (group_typed_str typed_str_list)
        pp_prop pr
  | Eq (tm1, tm2) -> fprintf fmt "%a = %a" pp_term tm1 pp_term tm2
  | Neq (tm1, tm2) -> fprintf fmt "%a ≠ %a" pp_term tm1 pp_term tm2

and pp_iprop fmt = function
  | False -> fprintf fmt "⊥"
  | Atom str -> fprintf fmt "%s" str
  | Pure pr -> fprintf fmt "⌜ %a ⌝" pp_prop pr
  | Star (ipr1, ipr2) -> fprintf fmt "%a * %a" pp_iprop ipr1 pp_iprop ipr2
  | Wand (ipr1, ipr2) -> fprintf fmt "(%a -* %a)" pp_iprop ipr1 pp_iprop ipr2
  | Box ipr -> fprintf fmt "□ %a" pp_iprop ipr
  | HPred (str, tm_list) ->
      fprintf fmt "%s %a" str
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_term)
        tm_list
  | HForall (typed_str_list, ipr) ->
      fprintf fmt "forall %a, %a"
        (pp_typed_strs_list
           ~pp_sep:(fun fmt () -> fprintf fmt " ")
           ~pp_paren:true ())
        (group_typed_str typed_str_list)
        pp_iprop ipr
  | HExists (typed_str_list, ipr) ->
      fprintf fmt "exists %a, %a"
        (pp_typed_strs_list
           ~pp_sep:(fun fmt () -> fprintf fmt " ")
           ~pp_paren:true ())
        (group_typed_str typed_str_list)
        pp_iprop ipr

(** Free term variables occurring in iprop, prop and term. *)

let rec term_free_var = function
  | Var var -> [ var ]
  | App (str, tm_list) -> str :: List.concat_map term_free_var tm_list

let rec prop_free_var = function
  | Persistent ipr -> iprop_free_var ipr
  | Not pr -> prop_free_var pr
  | And (pr1, pr2) | Or (pr1, pr2) | Imply (pr1, pr2) ->
      prop_free_var pr1 @ prop_free_var pr2
  | Pred (_, tm_list) -> List.concat_map term_free_var tm_list
  | Forall (typed_str_list, pr) | Exists (typed_str_list, pr) ->
      let binded_var_list = List.map fst typed_str_list in
      List.filter
        (fun var -> not (List.mem var binded_var_list))
        (prop_free_var pr)
  | Eq (tm1, tm2) | Neq (tm1, tm2) -> term_free_var tm1 @ term_free_var tm2

and iprop_free_var = function
  | False | Atom _ -> []
  | Pure pr -> prop_free_var pr
  | Star (ipr1, ipr2) | Wand (ipr1, ipr2) ->
      iprop_free_var ipr1 @ iprop_free_var ipr2
  | Box ipr -> iprop_free_var ipr
  | HPred (_, tm_list) -> List.concat_map term_free_var tm_list
  | HForall (typed_str_list, ipr) | HExists (typed_str_list, ipr) ->
      let binded_var_list = List.map fst typed_str_list in
      List.filter
        (fun var -> not (List.mem var binded_var_list))
        (iprop_free_var ipr)

(** The problem instance is represented as a record holding:
    - type declarations
    - predicate declarations
    - constant declarations
    - pure facts
    - persistent laws
    - initial atoms *)

type instance = {
  decl_types : (string * (string * itype) list) list;
  decl_funcs : (string * itype) list;
  decl_preds : (string * itype) list;
  decl_consts : (string * itype) list;
  decl_facts : prop list;
  decl_laws : iprop list;
  decl_init : iprop list;
}

let pp_instance fmt
    {
      decl_types;
      decl_funcs;
      decl_preds;
      decl_consts;
      decl_facts;
      decl_laws;
      decl_init;
    } =
  let () =
    if not (List.is_empty decl_types) then
      fprintf fmt "@[<v 4>types@,%a@]@."
        (pp_print_list (fun fmt (str, constr_list) ->
             match constr_list with
             | [] -> fprintf fmt "%s" str
             | _ ->
                 fprintf fmt "@[<v 4>%s =@,%a@]" str
                   (pp_print_list (fun fmt (constr, ity) ->
                        fprintf fmt "| %s : %a" constr pp_itype ity))
                   constr_list))
        decl_types
  in
  let () =
    if not (List.is_empty decl_funcs) then
      fprintf fmt "@[<v 4>funcs@,%a@]@."
        (pp_print_list (fun fmt (str, ity) ->
             fprintf fmt "%s : %a" str pp_itype ity))
        decl_funcs
  in
  let () =
    if not (List.is_empty decl_preds) then
      fprintf fmt "@[<v 4>preds@,%a@]@."
        (pp_print_list (fun fmt (str, ity) ->
             fprintf fmt "%s : %a" str pp_itype ity))
        decl_preds
  in
  let () =
    if not (List.is_empty decl_consts) then
      fprintf fmt "@[<v 4>consts@,%a@]@." (pp_typed_strs_list ())
        (group_typed_str decl_consts)
  in
  let () =
    if not (List.is_empty decl_facts) then
      fprintf fmt "@[<v 4>facts@,%a@]@."
        (pp_print_list
           ~pp_sep:(fun fmt () ->
             pp_print_char fmt ',';
             pp_print_cut fmt ())
           pp_prop)
        decl_facts
  in
  let () =
    if not (List.is_empty decl_laws) then
      fprintf fmt "@[<v 4>laws@,%a@]@."
        (pp_print_list
           ~pp_sep:(fun fmt () ->
             pp_print_char fmt ',';
             pp_print_cut fmt ())
           pp_iprop)
        decl_laws
  in
  if List.is_empty decl_init then fprintf fmt "@[<v 4>init@,%%empty@]@."
  else
    fprintf fmt "@[<v 4>init@,%a@]@."
      (pp_print_list
         ~pp_sep:(fun fmt () ->
           pp_print_char fmt ',';
           pp_print_cut fmt ())
         pp_iprop)
      decl_init
