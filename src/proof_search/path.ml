open State
open Format

type 'a path = Path of 'a list * string * 'a path list

let pp_state_list fmt = function
  | [] -> assert false
  | [ st ] -> fprintf fmt "%a" pp_state st
  | st :: st_list ->
      fprintf fmt "%a@.%a" pp_state st
        (pp_print_list (fun fmt st ->
             fprintf fmt "  ↓ %s@.@.%a" st.log pp_state st))
        st_list

let rec pp_state_path fmt (path : state path) =
  match path with
  | Path (st_list, msg, []) -> fprintf fmt "%a@.%s@." pp_state_list st_list msg
  | Path (st_list, msg, pt_list) ->
      fprintf fmt "%a@.%a" pp_state_list st_list
        (pp_print_list (fun fmt path ->
             fprintf fmt "- %s@.@.%a" msg pp_state_path path))
        pt_list
  | _ -> assert false
