open Path

type branch_state = Solved | Not_solved | Redundant

type 'a branch = {
  mutable state : branch_state;
  pred : 'a branch option;
  mutable succ : 'a branch list list;
  mutable path : 'a list * string;
}

let create_br () =
  { state = Not_solved; pred = None; succ = []; path = ([], "") }

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

let rec get_path { succ; path = path, msg; _ } =
  match succ with
  | [] -> Path (path, msg, [])
  | _ -> (
      match List.find_opt (List.for_all br_is_solved) succ with
      | Some succ -> Path (path, msg, List.map get_path succ)
      | None -> assert false)
