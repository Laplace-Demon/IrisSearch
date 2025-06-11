open Path

module type Type = sig
  type t
end

module Branch (State : Type) = struct
  type state = State.t

  type t = {
    mutable solved : bool;
    pred : t option;
    mutable succ : t list;
    mutable path : state list * string;
  }

  let create () = { solved = false; pred = None; succ = []; path = ([], "") }

  let add br cnt =
    let succ = List.init cnt (fun _ -> { (create ()) with pred = Some br }) in
    br.succ <- succ;
    succ

  let is_marked { solved; _ } = solved

  let rec fwd_propagate br =
    if not br.solved then (
      br.solved <- true;
      List.iter fwd_propagate br.succ)

  let rec bwd_propagate br =
    if List.for_all is_marked br.succ then (
      br.solved <- true;
      fwd_propagate br;
      match br.pred with None -> () | Some pred -> bwd_propagate pred)

  let mark br path msg =
    br.path <- (List.rev path, msg);
    fwd_propagate br;
    match br.pred with None -> () | Some pred -> bwd_propagate pred

  let rec get_path br =
    let path, msg = br.path in
    Path (path, msg, List.map get_path br.succ)
end

module Make (State : sig
  type state

  val source : state
  val successors : state -> state list * bool

  exception Inconsistent of state option * string
end) =
struct
  let max_depth = ref 20
  let set_max_depth d = max_depth := d

  open State

  module Branch = Branch (struct
    type t = state
  end)

  let root_br = ref (Branch.create ())

  type node = {
    (* Graph node associated with this internal record. *)
    this : state;
    (* Length of best known path from a source node to this node. *)
    depth : int;
    (* Best known path from a source node to this node. *)
    mutable path : state list;
    (* The branch the node belongs to. *)
    mutable branch : Branch.t;
  }

  module Node : Type with type t = node = struct
    type t = node
  end

  module P = Priority_queue.Make (Node)

  let () =
    let node =
      { this = source; depth = 0; path = [ source ]; branch = !root_br }
    in
    P.add node node.depth

  let get_succ node =
    match node.depth < !max_depth with
    | false -> []
    | true ->
        let succ_states, is_branch = State.successors node.this in
        let succ_nodes =
          List.map
            (fun st ->
              let new_depth = node.depth + 1 in
              assert (0 <= new_depth);
              Statistics.record_depth new_depth;
              {
                this = st;
                depth = new_depth;
                path = [ st ];
                branch = node.branch;
              })
            succ_states
        in
        let () =
          match is_branch with
          | true ->
              node.branch.path <- (List.rev node.path, "");
              let br_list = Branch.add node.branch (List.length succ_nodes) in
              List.iter2 (fun node br -> node.branch <- br) succ_nodes br_list
          | false ->
              List.iter
                (fun succ_node -> succ_node.path <- succ_node.this :: node.path)
                succ_nodes
        in
        succ_nodes

  let rec search () =
    match Branch.is_marked !root_br with
    | true -> Some (Branch.get_path !root_br)
    | false -> (
        match P.get () with
        | None -> None
        | Some node ->
            if not (Branch.is_marked node.branch) then (
              Statistics.record_visited_state ();
              try List.iter (fun succ -> P.add succ succ.depth) (get_succ node)
              with Inconsistent (st_opt, msg) ->
                if Option.is_some st_opt then
                  node.path <- Option.get st_opt :: node.path;
                Branch.mark node.branch node.path msg);
            search ())
end
