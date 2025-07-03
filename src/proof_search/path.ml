open Format
open Edge_info

type 'a path = Path of ('a * edge_info) list * edge_info * 'a path list

let add_path_node path_node (Path (path_node_list, final_info, path_list)) =
  Path (path_node :: path_node_list, final_info, path_list)

let rec pp_path pp_path_node =
  let pp_path_node_list fmt = function
    | [] -> assert false
    | node_list ->
        fprintf fmt "%a"
          (pp_print_list (fun fmt (node, edge_info) ->
               fprintf fmt "%s@,@,%a" edge_info.verbose pp_path_node node))
          node_list
  in
  let rec pp_path_aux fmt = function
    | Path (path_node_list, final_info, []) ->
        fprintf fmt "@[<v 0>%a@,%s@]@\n" pp_path_node_list path_node_list
          final_info.verbose
    | Path (path_node_list, _, path_list) ->
        fprintf fmt "@[<v 0>%a@[<v 2>@,%a@]@]" pp_path_node_list path_node_list
          (pp_print_list (fun fmt path -> fprintf fmt "%a" pp_path_aux path))
          path_list
    | _ -> assert false
  in
  pp_path_aux
