open Edge_info
open Format
open Path

module Make (State : sig
  type state

  val get_index : state -> int
  val pp_state_debug : formatter -> state -> unit
  val source : state

  type successor

  val get_succ_or : successor -> (state * edge_info) list
  val get_succ_and : successor -> (state * edge_info) list
  val successors : state -> successor

  exception Inconsistent of (state * edge_info) option * edge_info
end) =
struct
  let max_depth = ref 20
  let set_max_depth d = max_depth := d

  open State

  type node_type = And_node | Or_node
  type node_status = Solved of state path | Failed | Pending

  let pp_node_status fmt = function
    | Solved _ -> fprintf fmt "Solved"
    | Failed -> fprintf fmt "Failed"
    | Pending -> fprintf fmt "Pending"

  type node = {
    this : state;
    node_type : node_type;
    mutable node_status : node_status;
    mutable pred : node list;
    mutable succ : (node * edge_info) list;
    mutable inconsistent_info_opt : edge_info option;
  }

  let is_node_solved { node_status; _ } =
    match node_status with Solved _ -> true | _ -> false

  let is_node_solved_opt { node_status; _ } =
    match node_status with Solved path -> Some path | _ -> None

  let is_node_failed { node_status; _ } =
    match node_status with Failed -> true | _ -> false

  let is_node_pending { node_status; _ } =
    match node_status with Pending -> true | _ -> false

  module InfiniteArray = MenhirLib.InfiniteArray

  let node_rank_array : node list InfiniteArray.t = InfiniteArray.make []
  let get_node_rank rank = InfiniteArray.get node_rank_array rank

  let set_node_rank rank node =
    InfiniteArray.set node_rank_array rank (node :: get_node_rank rank)

  let node_num = ref 0
  let node_array : node option InfiniteArray.t = InfiniteArray.make None
  let has_node index = Option.is_some (InfiniteArray.get node_array index)
  let get_node index = Option.get (InfiniteArray.get node_array index)

  let set_node index node =
    node_num := max !node_num (index + 1);
    InfiniteArray.set node_array index (Some node)

  let root =
    {
      this = source;
      node_type = Or_node;
      node_status = Pending;
      pred = [];
      succ = [];
      inconsistent_info_opt = None;
    }

  module P = Priority_queue.Make (struct
    type t = node
  end)

  let () =
    P.add root 0;
    set_node 0 root;
    set_node_rank 0 root

  let get_succ node =
    assert (node.node_status = Pending);
    let succ = State.successors node.this in
    let succ_or_nodes =
      let succ_or_nodes_and_edge_info =
        List.map
          (fun (st, edge_info) ->
            let index = get_index st in
            if has_node index then (
              let succ_node = get_node index in
              succ_node.pred <- node :: succ_node.pred;
              (succ_node, edge_info))
            else
              let succ_node =
                {
                  this = st;
                  node_type = Or_node;
                  node_status = Pending;
                  pred = [ node ];
                  succ = [];
                  inconsistent_info_opt = None;
                }
              in
              (succ_node, edge_info))
          (get_succ_or succ)
      in
      node.succ <- succ_or_nodes_and_edge_info @ node.succ;
      List.map fst succ_or_nodes_and_edge_info
    in
    let succ_and_nodes =
      match get_succ_and succ with
      | [] -> []
      | _ as succ_and ->
          let virtual_and_node =
            { node with node_type = And_node; pred = [ node ] }
          in
          node.succ <- (virtual_and_node, empty_edge_info) :: node.succ;
          let succ_and_nodes_and_edge_info =
            List.map
              (fun (st, edge_info) ->
                let index = get_index st in
                if has_node index then (
                  let succ_node = get_node index in
                  succ_node.pred <- virtual_and_node :: succ_node.pred;
                  (succ_node, edge_info))
                else
                  let succ_node =
                    {
                      this = st;
                      node_type = Or_node;
                      node_status = Pending;
                      pred = [ virtual_and_node ];
                      succ = [];
                      inconsistent_info_opt = None;
                    }
                  in
                  (succ_node, edge_info))
              succ_and
          in
          virtual_and_node.succ <- succ_and_nodes_and_edge_info;
          List.map fst succ_and_nodes_and_edge_info
    in
    succ_or_nodes @ succ_and_nodes

  let bwd_propagate_solved =
    let rec bwd_propagate_solved_aux is_virtual_and_node node =
      match node.node_status with
      | Solved _ | Failed -> ()
      | Pending -> (
          assert (not (List.is_empty node.succ));
          match node.node_type with
          | Or_node -> (
              (* Or_node is solved iff there exists a solved successor *)
              match
                List.find_map
                  (fun (succ_node, edge_info) ->
                    match is_node_solved_opt succ_node with
                    | Some path -> Some (succ_node.this, edge_info, path)
                    | None -> None)
                  node.succ
              with
              | Some (succ_state, edge_info, path) ->
                  if is_virtual_and_node then node.node_status <- Solved path
                  else
                    node.node_status <-
                      Solved (add_path_node (succ_state, edge_info) path);
                  List.iter (bwd_propagate_solved_aux false) node.pred
              | None -> ())
          | And_node -> (
              (* And_node is solved iff every successor is solved *)
              match
                List.for_all
                  (fun (succ_node, _) -> is_node_solved succ_node)
                  node.succ
              with
              | true ->
                  let path_list =
                    List.map
                      (fun (succ_node, edge_info) ->
                        match is_node_solved_opt succ_node with
                        | Some path ->
                            add_path_node (succ_node.this, edge_info) path
                        | None -> assert false)
                      node.succ
                  in
                  node.node_status <-
                    Solved (Path ([], empty_edge_info, path_list));
                  List.iter (bwd_propagate_solved_aux true) node.pred
              | false -> ()))
    in
    bwd_propagate_solved_aux false

  let rec bwd_propagate_failed node =
    match node.node_status with
    | Solved _ | Failed -> ()
    | Pending -> (
        assert (not (List.is_empty node.succ));
        match node.node_type with
        | Or_node -> (
            (* Or_node fails iff every successor fails *)
            match
              List.for_all
                (fun (succ_node, _) -> is_node_failed succ_node)
                node.succ
            with
            | true ->
                node.node_status <- Failed;
                List.iter bwd_propagate_failed node.pred
            | false -> ())
        | And_node -> (
            (* And_node fails iff there exists a failed successor *)
            match
              List.exists
                (fun (succ_node, _) -> is_node_failed succ_node)
                node.succ
            with
            | true ->
                node.node_status <- Failed;
                List.iter bwd_propagate_failed node.pred
            | false -> ()))

  let rec restore_pending_after_fail node =
    match node.node_status with
    | Pending | Solved _ -> ()
    | Failed -> (
        match node.node_type with
        | Or_node -> (
            match
              List.exists
                (fun (succ_node, _) -> not (is_node_failed succ_node))
                node.succ
            with
            | true ->
                node.node_status <- Pending;
                List.iter restore_pending_after_fail node.pred
            | false -> ())
        | And_node -> (
            match
              List.for_all
                (fun (succ_node, _) -> not (is_node_failed succ_node))
                node.succ
            with
            | true ->
                node.node_status <- Pending;
                List.iter restore_pending_after_fail node.pred
            | false -> ()))

  let bwd_propagate node =
    bwd_propagate_solved node;
    bwd_propagate_failed node

  let solve node final_info =
    node.node_status <- Solved (Path ([], final_info, []));
    node.inconsistent_info_opt <- Some final_info;
    List.iter bwd_propagate_solved node.pred

  let fail node =
    node.node_status <- Failed;
    List.iter bwd_propagate_failed node.pred

  let rec search () =
    match root.node_status with
    | Solved path ->
        Some
          (add_path_node
             (root.this, { simple = "  path"; verbose = "  path" })
             path)
    | Failed -> None
    | Pending -> (
        match P.get () with
        | None -> None
        | Some (({ node_status = Solved _ | Failed; _ } as node), _) -> None
        | Some (({ node_status = Pending; _ } as node), depth) ->
            let new_depth = depth + 1 in
            assert (new_depth >= 0);
            Statistics.record_visited_state ();
            (try
               if depth >= !max_depth then fail node
               else
                 let existent_succ, fresh_succ =
                   List.partition
                     (fun { this; _ } -> has_node (get_index this))
                     (get_succ node)
                 in
                 if not (List.is_empty existent_succ) then bwd_propagate node;
                 match node.node_status with
                 | Pending ->
                     if List.is_empty fresh_succ then (
                       fail node;
                       restore_pending_after_fail node)
                     else
                       List.iter
                         (fun succ_node ->
                           let succ_index = get_index succ_node.this in
                           P.add succ_node new_depth;
                           set_node succ_index succ_node;
                           set_node_rank new_depth succ_node)
                         fresh_succ
                 | _ -> ()
             with
            | Inconsistent (None, edge_info) -> solve node edge_info
            | Inconsistent (Some (final_st, edge_info), final_edge_info) ->
                let final_node =
                  {
                    this = final_st;
                    node_type = Or_node;
                    node_status = Pending;
                    pred = [ node ];
                    succ = [];
                    inconsistent_info_opt = None;
                  }
                in
                node.succ <- (final_node, edge_info) :: node.succ;
                let final_index = get_index final_st in
                if not (has_node final_index) then (
                  set_node final_index final_node;
                  set_node_rank new_depth final_node);
                solve final_node final_edge_info);
            search ())

  let pp_graph fmt = fun () -> fprintf fmt "node [ shape=record ]@,"

  let pp_nodes fmt =
   fun () ->
    if is_node_solved root then fprintf fmt "⊥ [ shape=plaintext label=\"⊥\"]";
    for index = 0 to !node_num - 1 do
      match InfiniteArray.get node_array index with
      | Some node -> (
          match node.node_status with
          | Solved _ ->
              fprintf fmt
                "%i [ style=filled fillcolor=\"green\" label=\"%s\" ]@," index
                (asprintf "%a" pp_state_debug node.this)
          | Failed ->
              fprintf fmt "%i [ style=filled fillcolor=\"red\" label=\"%s\" ]@,"
                index
                (asprintf "%a" pp_state_debug node.this)
          | Pending ->
              fprintf fmt "%i [ label=\"%s\" ]@," index
                (asprintf "%a" pp_state_debug node.this))
      | None -> ()
    done

  let pp_subgraph fmt =
   fun () ->
    for rank = 0 to !max_depth do
      let subgraph = List.rev (get_node_rank rank) in
      if not (List.is_empty subgraph) then
        fprintf fmt "@[<v 2>subgraph {@,rank=same@,%a@]@,}@,"
          (pp_print_list
             ~pp_sep:(fun fmt () -> fprintf fmt " ")
             (fun fmt node -> fprintf fmt "%i" (get_index node.this)))
          subgraph
    done

  let pp_edges fmt =
    let rec pp_edges_aux index node =
      (match node.inconsistent_info_opt with
      | Some inconsistent_info ->
          fprintf fmt "%i -> ⊥ [ label=\"%s\" ]" index inconsistent_info.simple
      | None -> ());
      match node.node_type with
      | And_node ->
          List.iter
            (fun (succ_node, edge_info) ->
              let succ_index = get_index succ_node.this in
              fprintf fmt "%i -> %i [ label=\"%s\" style=dashed ]@," index
                succ_index edge_info.simple)
            node.succ
      | Or_node ->
          List.iter
            (fun (succ_node, edge_info) ->
              let succ_index = get_index succ_node.this in
              if is_empty_edge_info edge_info then
                pp_edges_aux succ_index succ_node
              else
                fprintf fmt "%i -> %i [ label=\"%s\" ]@," index succ_index
                  edge_info.simple)
            node.succ
    in
    fun () ->
      for index = 0 to !node_num - 1 do
        pp_edges_aux index (get_node index)
      done

  let pp_search_graph_debug fmt =
   fun () ->
    fprintf fmt "%a@,%a@,%a@,%a" pp_graph () pp_nodes () pp_subgraph () pp_edges
      ()
end
