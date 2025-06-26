open Format

type 'a path = Path of 'a list * string * 'a path list

let rec pp_path path_node_info pp_path_node =
  let pp_path_node_list fmt = function
    | [] -> assert false
    | node_list ->
        fprintf fmt "%a"
          (pp_print_list (fun fmt node ->
               fprintf fmt "%s@,@,%a" (path_node_info node) pp_path_node node))
          node_list
  in
  let rec pp_path_aux fmt = function
    | Path (st_list, msg, []) ->
        fprintf fmt "@[<v 0>%a@,%s@]@\n" pp_path_node_list st_list msg
    | Path (st_list, _, pt_list) ->
        fprintf fmt "@[<v 0>%a@[<v 2>@,%a@]@]" pp_path_node_list st_list
          (pp_print_list (fun fmt path -> fprintf fmt "%a" pp_path_aux path))
          pt_list
    | _ -> assert false
  in
  pp_path_aux
