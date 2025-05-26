open State
open Format

type 'a path = Path of 'a list * string * 'a path list

let rec pp_state_path fmt (path : state path) =
  match path with
  | Path ([ st ], msg, pt_list) ->
      fprintf fmt "@[<v>%a%s%a@]" pp_state st msg
        (pp_print_list (fun fmt path ->
             fprintf fmt "@[<v 2>- %a@]" pp_state_path path))
        pt_list
  | Path (st :: st_list, msg, pt_list) ->
      fprintf fmt "@[<v>%a@.%a%s%a@]" pp_state st
        (pp_print_list (fun fmt st ->
             fprintf fmt "  ↓ %s@.@.%a" st.log pp_state st))
        st_list msg
        (pp_print_list (fun fmt path ->
             fprintf fmt "@[<v 2>- %a@]" pp_state_path path))
        pt_list
