open Format
open Path

type branch_state = Solved | Not_solved | Redundant

let pp_branch_state fmt = function
  | Solved -> fprintf fmt "Solved"
  | Not_solved -> fprintf fmt "Not_solved"
  | Redundant -> fprintf fmt "Redundant"

type 'a branch = {
  index : int;
  mutable element_list : int list;
  mutable state : branch_state;
  pred : 'a branch option;
  mutable succ : 'a branch list list;
  mutable path : 'a list * string;
}

let br_cnt = ref 0

let create_br () =
  let br =
    {
      index = !br_cnt;
      element_list = [];
      state = Not_solved;
      pred = None;
      succ = [];
      path = ([], "");
    }
  in
  br_cnt := !br_cnt + 1;
  br

let add_br br cnt =
  let succ = List.init cnt (fun _ -> { (create_br ()) with pred = Some br }) in
  br.succ <- succ :: br.succ;
  succ

let br_is_solved { state; _ } =
  match state with Solved -> true | Redundant | Not_solved -> false

let br_is_marked { state; _ } =
  match state with Solved | Redundant -> true | Not_solved -> false

let br_fwd_propagate br =
  let rec br_fwd_propagate_aux br =
    match br.state with
    | Solved | Redundant -> ()
    | Not_solved ->
        br.state <- Redundant;
        List.iter (fun succ' -> List.iter br_fwd_propagate_aux succ') br.succ
  in
  match br.state with
  | Solved | Redundant -> assert false
  | Not_solved ->
      br.state <- Solved;
      List.iter (List.iter br_fwd_propagate_aux) br.succ

let rec br_bwd_propagate gen_path br =
  if List.exists (List.for_all br_is_solved) br.succ then (
    br.path <- (gen_path (), "");
    br_fwd_propagate br;
    match br.pred with
    | None -> ()
    | Some pred -> br_bwd_propagate gen_path pred)

let mark_br br gen_path msg =
  br.path <- (gen_path (), msg);
  br_fwd_propagate br;
  match br.pred with None -> () | Some pred -> br_bwd_propagate gen_path pred

let add_elements_br br elements = br.element_list <- br.element_list @ elements

let rec get_path { succ; path = path, msg; _ } =
  Path
    ( path,
      msg,
      match List.find_opt (List.for_all br_is_solved) succ with
      | Some succ -> List.map get_path succ
      | None -> [] )

let rec show_branch fmt br =
  match br.succ with
  | [] ->
      fprintf fmt
        "@[<v 4>branch %i: %a@,elements: %a@,successors: %%empty@]@\n@\n"
        br.index pp_branch_state br.state
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_print_int)
        br.element_list
  | _ ->
      let succ = List.rev br.succ in
      fprintf fmt "@[<v 4>branch %i: %a@,elements: %a@,successors: %a@]@\n@\n"
        br.index pp_branch_state br.state
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_print_int)
        br.element_list
        (pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt " ∨ ")
           (fun fmt succ' ->
             fprintf fmt "(%a)"
               (pp_print_list
                  ~pp_sep:(fun fmt () -> fprintf fmt " ∧ ")
                  (fun fmt br -> fprintf fmt "%i" br.index))
               succ'))
        succ;
      List.iter (List.iter (show_branch fmt)) succ
