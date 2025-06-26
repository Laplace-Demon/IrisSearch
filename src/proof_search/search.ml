open Branch
open Format
open Path

module Make (State : sig
  type state

  val pp_state : formatter -> state -> unit
  val source : state
  val state_br : state -> state branch
  val successors : state -> state list

  exception Inconsistent of state option * string
end) =
struct
  let max_depth = ref 20
  let set_max_depth d = max_depth := d

  open State

  type node = {
    (* Graph node associated with this internal record. *)
    this : state;
    (* Length of best known path from a source node to this node. *)
    depth : int;
    (* Best known path from a source node to this node. *)
    mutable path : state list;
  }

  module P = Priority_queue.Make (struct
    type t = node
  end)

  let init_branch = state_br source

  let () =
    let node = { this = source; depth = 0; path = [ source ] } in
    P.add node node.depth

  let get_succ node =
    match node.depth < !max_depth with
    | false -> []
    | true ->
        let succ_states = State.successors node.this in
        List.map
          (fun st ->
            let new_depth = node.depth + 1 in
            assert (0 <= new_depth);
            Statistics.record_depth new_depth;
            { this = st; depth = new_depth; path = st :: node.path })
          succ_states

  let gen_path node =
    let br = ref (state_br node.this) in
    let path = ref node.path in
    let rec gen_path_aux acc =
      match !path with
      | [] -> acc
      | st :: path' ->
          if !br == state_br st then (
            path := path';
            gen_path_aux (st :: acc))
          else (
            br := state_br st;
            acc)
    in
    fun () -> gen_path_aux []

  let rec search () =
    match br_is_solved init_branch with
    | true -> Some (Branch.get_path init_branch)
    | false -> (
        match P.get () with
        | None -> None
        | Some node ->
            let br = state_br node.this in
            if not (br_is_marked br) then (
              Statistics.record_visited_state ();
              try List.iter (fun succ -> P.add succ succ.depth) (get_succ node)
              with Inconsistent (st_opt, msg) ->
                if Option.is_some st_opt then
                  node.path <- Option.get st_opt :: node.path;
                Branch.mark_br br (gen_path node) msg);
            search ())
end
