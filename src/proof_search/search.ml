module Make (G : sig
  type node

  val source : node
  val successors : node -> node list
  val estimate : node -> int

  exception Termination of string
end) =
struct
  type cost = int
  type priority = cost

  type inode = {
    (* Graph node associated with this internal record. *)
    this : G.node;
    (* Cost of the best known path from a source node to this node. (ghat) *)
    mutable cost : cost;
    (* Estimated cost of the best path from this node to a goal node. (hhat) *)
    estimate : cost;
    (* Best known path from a source node to this node. *)
    mutable path : G.node list;
    (* Previous node on doubly linked priority list *)
    mutable prev : inode;
    (* Next node on doubly linked priority list *)
    mutable next : inode;
    (* The node's priority, if the node is in the queue; -1 otherwise *)
    mutable priority : priority;
  }

  (* This auxiliary module maintains a priority queue of
       internal records. *)

  module P : sig
    (* Adds this node to the queue. *)
    val add : inode -> priority -> unit

    (* Retrieve a node with lowest priority of the queue. *)
    val get : unit -> inode option
  end = struct
    module InfiniteArray = MenhirLib.InfiniteArray

    (* Array of pointers to the doubly linked lists, indexed by priorities.
         There is no a priori bound on the size of this array -- its size is
         increased if needed. It is up to the user to use a graph where paths
         have reasonable lengths. *)
    let a = InfiniteArray.make None

    (* Index of lowest nonempty list, if there is one; or lower (sub-optimal,
         but safe). If the queue is empty, [best] is arbitrary. *)
    let best = ref 0

    (* Current number of elements in the queue. Used in [get] to stop the
         search for a nonempty bucket. *)
    let cardinal = ref 0

    (* Adjust node's priority and insert into doubly linked list. *)
    let add inode priority =
      assert (0 <= priority);
      cardinal := !cardinal + 1;
      inode.priority <- priority;
      match InfiniteArray.get a priority with
      | None ->
          InfiniteArray.set a priority (Some inode);
          (* Decrease [best], if necessary, so as not to miss the new element.
               In the special case of A*, this never happens. *)
          assert (!best <= priority)
          (* if priority < !best then best := priority *)
      | Some inode' ->
          inode.next <- inode';
          inode.prev <- inode'.prev;
          inode'.prev.next <- inode;
          inode'.prev <- inode

    (* Takes a node off its doubly linked list. Does not adjust [best],
         as this is not necessary in order to preserve the invariant. *)
    let remove inode =
      cardinal := !cardinal - 1;
      if inode.next == inode then InfiniteArray.set a inode.priority None
      else (
        InfiniteArray.set a inode.priority (Some inode.next);
        inode.next.prev <- inode.prev;
        inode.prev.next <- inode.next;
        inode.next <- inode;
        inode.prev <- inode);
      inode.priority <- -1

    let rec get () = if !cardinal = 0 then None else get_nonempty ()

    and get_nonempty () =
      (* Look for next nonempty bucket. We know there is one. This may
           seem inefficient, because it is a linear search. However, in
           A*, [best] never decreases, so the total cost of this loop is
           the maximum priority ever used. *)
      match InfiniteArray.get a !best with
      | None ->
          best := !best + 1;
          get_nonempty ()
      | Some inode as result ->
          remove inode;
          result
  end

  let () =
    let node = G.source in
    let rec inode =
      {
        this = node;
        cost = 0;
        estimate = G.estimate node;
        path = [ node ];
        prev = inode;
        next = inode;
        priority = -1;
      }
    in
    P.add inode inode.estimate

  let max_depth = ref 20
  let set_max_depth d = max_depth := d
  let end_msg = ref ""
  let get_end_msg () = !end_msg

  let search () =
    let rec aux () =
      (* Pick the open node that currently has lowest fhat,
         that is, lowest estimated distance to a goal node. *)
      match P.get () with
      | None -> None
      | Some inode -> (
          let node = inode.this in
          Statistics.record_visited_state ();
          try
            let successors = G.successors node in
            List.iter
              (fun son ->
                (* Determine the cost of the best known path from the
                 start node, through this node, to this son. *)
                let new_cost = inode.cost + 1 in
                assert (0 <= new_cost);
                if new_cost < !max_depth then (
                  let rec ison =
                    {
                      this = son;
                      cost = new_cost;
                      estimate = G.estimate son;
                      path = son :: inode.path;
                      prev = ison;
                      next = ison;
                      priority = -1;
                    }
                  in
                  let fhat = new_cost + ison.estimate in
                  assert (0 <= fhat);
                  (* failure means overflow *)
                  Statistics.record_depth new_cost;
                  P.add ison fhat))
              successors;
            aux ()
          with G.Termination msg ->
            end_msg := msg;
            Some inode.path)
    in
    aux ()
end
