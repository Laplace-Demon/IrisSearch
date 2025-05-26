open Format
open Internal
open Type

(** Definition of law, local and global states. *)

type law = {
  name_opt : string option;
  intern : internal_iprop;
  extern : internal_iprop;
}

let pp_law_internal fmt { name_opt; intern } =
  Ast.pp_named pp_internal_iprop fmt (name_opt, intern)

let pp_law fmt { name_opt; extern } =
  Ast.pp_named pp_internal_iprop fmt (name_opt, extern)

type global_state = {
  mutable persistent : internal_prop_set;
  mutable facts : internal_prop_set;
  mutable laws : law list;
}

let global_state =
  { persistent = PropSet.empty; facts = PropSet.empty; laws = [] }

let pp_global_state fmt () =
  fprintf fmt "@[<v 4>facts@,%a@,%a@]@.@."
    (pp_internal_prop_set ~pp_sep:(fun fmt () ->
         pp_print_char fmt ',';
         pp_print_cut fmt ()))
    global_state.persistent
    (pp_internal_prop_set ~pp_sep:(fun fmt () ->
         pp_print_char fmt ',';
         pp_print_cut fmt ()))
    global_state.facts;
  fprintf fmt "@[<v 4>laws@,%a@]@.@."
    (pp_print_list
       ~pp_sep:(fun fmt () ->
         pp_print_char fmt ',';
         pp_print_cut fmt ())
       pp_law)
    global_state.laws

type state = {
  local_var_list : (string * itype) list;
  ipr_mset : simple_internal_iprop_multiset;
  pr_set : internal_prop_set;
  log : string;
}

let empty_state =
  {
    local_var_list = [];
    ipr_mset = SimpleIpropMset.empty;
    pr_set = PropSet.empty;
    log = "empty";
  }

let pp_local_var_list fmt = function
  | [] -> fprintf fmt "%%empty"
  | _ as local_var_list ->
      fprintf fmt "%a" (pp_typed_strs_list ()) (group_typed_str local_var_list)

let pp_state fmt { local_var_list; ipr_mset; pr_set } =
  let local_varname_list_rev = List.rev_map fst local_var_list in
  fprintf fmt "@[<v 4>locals@,%a@]@.@[<v 4>atoms@,%a@]@.@[<v 4>pures@,%a@]@."
    pp_local_var_list local_var_list
    (pp_simple_internal_iprop_multiset_env local_varname_list_rev
       ~pp_sep:pp_print_cut)
    ipr_mset
    (pp_internal_prop_set_env local_varname_list_rev ~pp_sep:pp_print_cut)
    pr_set

(** Definition of inconsistent exception. *)

exception Inconsistent of string
