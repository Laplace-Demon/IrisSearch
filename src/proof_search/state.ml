open Format
open Branch
open Internal
open Type

(** Definition of law, local and global states. *)

type law = {
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
  local_var_list : (string * itype) list;
  ipr_mset : simple_internal_iprop_multiset;
  pr_set : internal_prop_set;
  disj_list : simple_internal_iprop list list;
  branch : state branch;
  log : string;
}

let init_branch = create_br ()

let empty_state =
  {
    local_var_list = [];
    ipr_mset = SimpleIpropMset.empty;
    pr_set = PropSet.empty;
    disj_list = [];
    branch = init_branch;
    log = "empty";
  }

let state_br { branch; _ } = branch
let state_info { log; _ } = log

let pp_local_var_list fmt = function
  | [] -> fprintf fmt "%%empty"
  | _ as local_var_list ->
      fprintf fmt "%a" (pp_typed_strs_list ()) (group_typed_str local_var_list)

let pp_state fmt { local_var_list; ipr_mset; pr_set; disj_list; _ } =
  let local_varname_list_rev = List.rev_map fst local_var_list in
  (if SimpleIpropMset.is_empty ipr_mset || List.is_empty disj_list then
     fprintf fmt
       "@[<v 4>locals@,%a@]@\n@[<v 4>atoms@,%a%a@]@\n@[<v 4>pures@,%a@]@\n"
   else
     fprintf fmt
       "@[<v 4>locals@,%a@]@\n@[<v 4>atoms@,%a@,%a@]@\n@[<v 4>pures@,%a@]@\n")
    pp_local_var_list local_var_list
    (pp_simple_internal_iprop_multiset_env local_varname_list_rev
       ~pp_sep:pp_print_cut)
    ipr_mset
    (pp_print_list
       (pp_print_list
          ~pp_sep:(fun fmt () -> fprintf fmt " ∨ ")
          (fun fmt ipr ->
            (if simple_internal_iprop_cardinal ipr > 1 then fprintf fmt "(%a)"
             else fprintf fmt "%a")
              (pp_simple_internal_iprop_env local_varname_list_rev)
              ipr)))
    disj_list
    (pp_internal_prop_set_env local_varname_list_rev ~pp_sep:pp_print_cut)
    (PropSet.union global_state.facts pr_set)

let pp_state_path = Path.pp_path state_info pp_state

(** Definition of inconsistent exception. *)

exception Inconsistent of state option * string
