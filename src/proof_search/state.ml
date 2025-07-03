open Format
open Internal
open Type

(** Definition of law, global state, local state. *)

type law = {
  index : int;
  name_opt : string option;
  intern : internal_iprop;
  extern : internal_iprop;
}

let pp_law_internal fmt { name_opt; intern; _ } =
  Ast.pp_named pp_internal_iprop fmt (name_opt, intern)

let pp_law fmt { name_opt; extern; _ } =
  Ast.pp_named pp_internal_iprop fmt (name_opt, extern)

type global_state = {
  mutable persistent : internal_prop_set;
  mutable facts : internal_prop_set;
  mutable laws : law list;
}

let global_state =
  { persistent = PropSet.empty; facts = PropSet.empty; laws = [] }

let pp_global_state fmt () =
  fprintf fmt "@[<v 4>facts@,%a@]@\n"
    (pp_internal_prop_set ~pp_sep:(fun fmt () ->
         pp_print_char fmt ',';
         pp_print_cut fmt ()))
    (PropSet.union global_state.facts global_state.persistent);
  fprintf fmt "@[<v 4>laws@,%a@]@\n"
    (fun fmt -> function
      | [] -> pp_print_string fmt "%empty"
      | _ as laws ->
          (pp_print_list
             ~pp_sep:(fun fmt () ->
               pp_print_char fmt ',';
               pp_print_cut fmt ())
             pp_law)
            fmt laws)
    global_state.laws

type state = {
  index : int;
  local_var_list : (string * itype) list;
  ipr_mset : simple_internal_iprop_multiset;
  pr_set : internal_prop_set;
  disj_list : simple_internal_iprop list list;
}

let get_index { index; _ } = index

let empty_state =
  {
    index = -1;
    local_var_list = [];
    ipr_mset = SimpleIpropMset.empty;
    pr_set = PropSet.empty;
    disj_list = [];
  }

let pp_local_var_list fmt = function
  | [] -> fprintf fmt "%%empty"
  | _ as local_var_list ->
      fprintf fmt "%a" (pp_typed_strs_list ()) (group_typed_str local_var_list)

let pp_state ?(pp_index = false) fmt
    { index; local_var_list; ipr_mset; pr_set; disj_list; _ } =
  let local_varname_list_rev = List.rev_map fst local_var_list in
  if pp_index then fprintf fmt "state %i@\n@\n" index;
  match (SimpleIpropMset.is_empty ipr_mset, List.is_empty disj_list) with
  | _, true ->
      fprintf fmt
        "@[<v 4>locals@,%a@]@\n@[<v 4>atoms@,%a@]@\n@[<v 4>pures@,%a@]@\n"
        pp_local_var_list local_var_list
        (pp_simple_internal_iprop_multiset_env local_varname_list_rev
           ~pp_sep:pp_print_cut)
        ipr_mset
        (pp_internal_prop_set_env local_varname_list_rev ~pp_sep:pp_print_cut)
        (PropSet.union global_state.facts pr_set)
  | true, false ->
      fprintf fmt
        "@[<v 4>locals@,%a@]@\n@[<v 4>atoms@,%a@]@\n@[<v 4>pures@,%a@]@\n"
        pp_local_var_list local_var_list
        (pp_print_list
           (pp_print_list
              ~pp_sep:(fun fmt () -> fprintf fmt " ∨ ")
              (fun fmt ipr ->
                (if simple_internal_iprop_cardinal ipr > 1 then
                   fprintf fmt "(%a)"
                 else fprintf fmt "%a")
                  (pp_simple_internal_iprop_env local_varname_list_rev)
                  ipr)))
        disj_list
        (pp_internal_prop_set_env local_varname_list_rev ~pp_sep:pp_print_cut)
        (PropSet.union global_state.facts pr_set)
  | false, false ->
      fprintf fmt
        "@[<v 4>locals@,%a@]@\n@[<v 4>atoms@,%a@,%a@]@\n@[<v 4>pures@,%a@]@\n"
        pp_local_var_list local_var_list
        (pp_simple_internal_iprop_multiset_env local_varname_list_rev
           ~pp_sep:pp_print_cut)
        ipr_mset
        (pp_print_list
           (pp_print_list
              ~pp_sep:(fun fmt () -> fprintf fmt " ∨ ")
              (fun fmt ipr ->
                (if simple_internal_iprop_cardinal ipr > 1 then
                   fprintf fmt "(%a)"
                 else fprintf fmt "%a")
                  (pp_simple_internal_iprop_env local_varname_list_rev)
                  ipr)))
        disj_list
        (pp_internal_prop_set_env local_varname_list_rev ~pp_sep:pp_print_cut)
        (PropSet.union global_state.facts pr_set)

let pp_state_path = Path.pp_path (pp_state ~pp_index:true)

(* Print states and laws in .dot file. *)

let pp_state_debug fmt { index; local_var_list; ipr_mset; pr_set; disj_list } =
  let pp_space = fun fmt () -> fprintf fmt " " in
  let pp_newline = fun fmt () -> fprintf fmt "\\n" in
  let local_varname_list_rev = List.rev_map fst local_var_list in
  let () = fprintf fmt "{<index>%i|" index in
  let () =
    if not (List.is_empty local_varname_list_rev) then
      fprintf fmt "<locals>∃ %a|"
        (pp_typed_strs_list ~pp_sep:pp_space ~pp_paren:true ())
        (group_typed_str local_var_list)
  in
  let has_atoms =
    not (SimpleIpropMset.is_empty ipr_mset && List.is_empty disj_list)
  in
  let () =
    match (SimpleIpropMset.is_empty ipr_mset, List.is_empty disj_list) with
    | true, true -> fprintf fmt "{"
    | false, true ->
        fprintf fmt "{<atoms>%a"
          (pp_simple_internal_iprop_multiset_env local_varname_list_rev
             ~pp_sep:pp_newline)
          ipr_mset
    | true, false ->
        fprintf fmt "{<atoms>%a"
          (pp_print_list ~pp_sep:pp_newline
             (pp_print_list
                ~pp_sep:(fun fmt () -> fprintf fmt " ∨ ")
                (fun fmt ipr ->
                  (if simple_internal_iprop_cardinal ipr > 1 then
                     fprintf fmt "(%a)"
                   else fprintf fmt "%a")
                    (pp_simple_internal_iprop_env local_varname_list_rev)
                    ipr)))
          disj_list
    | false, false ->
        fprintf fmt "{<atoms>%a\\n%a"
          (pp_simple_internal_iprop_multiset_env local_varname_list_rev
             ~pp_sep:pp_newline)
          ipr_mset
          (pp_print_list ~pp_sep:pp_newline
             (pp_print_list
                ~pp_sep:(fun fmt () -> fprintf fmt " ∨ ")
                (fun fmt ipr ->
                  (if simple_internal_iprop_cardinal ipr > 1 then
                     fprintf fmt "(%a)"
                   else fprintf fmt "%a")
                    (pp_simple_internal_iprop_env local_varname_list_rev)
                    ipr)))
          disj_list
  in
  let pures = PropSet.union global_state.facts pr_set in
  match PropSet.is_empty pures with
  | true -> fprintf fmt "}}"
  | false ->
      if has_atoms then
        fprintf fmt "|<pures> %a}}"
          (pp_internal_prop_set_env local_varname_list_rev ~pp_sep:pp_newline)
          pures
      else
        fprintf fmt "<pures> %a}}"
          (pp_internal_prop_set_env local_varname_list_rev ~pp_sep:pp_newline)
          pures

let pp_laws_debug fmt () =
  fprintf fmt "\"laws\" [ shape=plaintext label=\"%a\" ]@,"
    (pp_print_list
       ~pp_sep:(fun fmt () -> fprintf fmt "\\n")
       (fun fmt { index; extern; _ } ->
         fprintf fmt "law %i: %a" index pp_internal_iprop extern))
    global_state.laws
