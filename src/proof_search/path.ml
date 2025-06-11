open State
open Format

type 'a path = Path of 'a list * string * 'a path list

let pp_state_list fmt = function
  | [] -> assert false
  | st_list ->
      fprintf fmt "%a"
        (pp_print_list (fun fmt st -> fprintf fmt "%s@,@,%a" st.log pp_state st))
        st_list

let rec pp_state_path fmt (path : state path) =
  match path with
  | Path (st_list, msg, []) ->
      fprintf fmt "@[<v 0>%a@,%s@]@\n" pp_state_list st_list msg
  | Path (st_list, _, pt_list) ->
      fprintf fmt "@[<v 0>%a@[<v 2>@,%a@]@]" pp_state_list st_list
        (pp_print_list (fun fmt path -> fprintf fmt "%a" pp_state_path path))
        pt_list
  | _ -> assert false
