open Format
open Type

(** Definition of abstract syntax tree returned by the parser. *)

type term = Var of string

let term_eqb tm1 tm2 =
  match (tm1, tm2) with Var str1, Var str2 -> String.equal str1 str2

let pp_term fmt = function Var str -> fprintf fmt "%s" str

type prop =
  | Persistent of iprop
  | Not of prop
  | And of prop * prop
  | Or of prop * prop
  | Imply of prop * prop
  | Pred of string * term list
  | Forall of (string * itype) list * prop
  | Eq of term * term
  | Neq of term * term

and iprop =
  | False
  | Atom of string
  | Star of iprop * iprop
  | Wand of iprop * iprop
  | Box of iprop
  | Pure of prop
  | HPred of string * term list
  | HForall of (string * itype) list * iprop

(** These functions do not consider alpha-equivalence. *)

let rec prop_eqb pr1 pr2 =
  match (pr1, pr2) with
  | Persistent ipr1, Persistent ipr2 -> iprop_eqb ipr1 ipr2
  | Not pr1, Not pr2 -> prop_eqb pr1 pr2
  | And (pr11, pr12), And (pr21, pr22)
  | Or (pr11, pr12), Or (pr21, pr22)
  | Imply (pr11, pr12), Imply (pr21, pr22) ->
      prop_eqb pr11 pr21 && prop_eqb pr12 pr22
  | Pred (str1, param_list1), Pred (str2, param_list2) ->
      String.equal str1 str2 && List.equal term_eqb param_list1 param_list2
  | Forall (typed_str_list1, pr1), Forall (typed_str_list2, pr2) ->
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
  | Star (ipr11, ipr12), Star (ipr21, ipr22)
  | Wand (ipr11, ipr12), Wand (ipr21, ipr22) ->
      iprop_eqb ipr11 ipr21 && iprop_eqb ipr12 ipr22
  | Box ipr1, Box ipr2 -> iprop_eqb ipr1 ipr2
  | Pure pr1, Pure pr2 -> prop_eqb pr1 pr2
  | HPred (str1, param_list1), HPred (str2, param_list2) ->
      String.equal str1 str2 && List.equal term_eqb param_list1 param_list2
  | HForall (typed_str_list1, ipr1), HForall (typed_str_list2, ipr2) ->
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
  | Pred (str, param_list) ->
      fprintf fmt "%s %a" str
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_term)
        param_list
  | Forall (typed_str_list, pr) ->
      fprintf fmt "forall %a, %a"
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
  | Star (ipr1, ipr2) -> fprintf fmt "%a * %a" pp_iprop ipr1 pp_iprop ipr2
  | Wand (ipr1, ipr2) -> fprintf fmt "(%a -* %a)" pp_iprop ipr1 pp_iprop ipr2
  | Box ipr -> fprintf fmt "□ %a" pp_iprop ipr
  | Pure pr -> fprintf fmt "⌜ %a ⌝" pp_prop pr
  | HPred (str, param_list) ->
      fprintf fmt "%s %a" str
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_term)
        param_list
  | HForall (typed_str_list, ipr) ->
      fprintf fmt "forall %a, %a"
        (pp_typed_strs_list
           ~pp_sep:(fun fmt () -> fprintf fmt " ")
           ~pp_paren:true ())
        (group_typed_str typed_str_list)
        pp_iprop ipr

(** The problem instance is represented as a record holding:
    - type declarations
    - predicate declarations
    - constant declarations
    - pure facts
    - persistent laws
    - initial atoms *)

type instance = {
  decl_types : string list;
  decl_preds : (string * itype) list;
  decl_consts : (string * itype) list;
  decl_facts : prop list;
  decl_laws : iprop list;
  decl_init : iprop list;
}

let pp_instance fmt
    { decl_types; decl_preds; decl_consts; decl_facts; decl_laws; decl_init } =
  let () =
    if not (List.is_empty decl_types) then
      fprintf fmt "@[<v 4>types@,%a@]@."
        (pp_print_list pp_print_string)
        decl_types
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
