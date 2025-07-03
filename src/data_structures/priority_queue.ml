module type Type = sig
  type t
end

module type PriorityQueue = sig
  type node

  val add : node -> int -> unit
  val get : unit -> (node * int) option
end

module Make (Node : Type) : PriorityQueue with type node = Node.t = struct
  type node = Node.t

  module InfiniteArray = MenhirLib.InfiniteArray

  type inode = {
    this : node;
    (* Previous node on doubly linked priority list *)
    mutable prev : inode;
    (* Next node on doubly linked priority list *)
    mutable next : inode;
    (* The node's priority, if the node is in the queue; -1 otherwise *)
    mutable priority : int;
  }

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
  let add node priority =
    assert (0 <= priority);
    cardinal := !cardinal + 1;
    let rec inode = { this = node; prev = inode; next = inode; priority } in
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
    | Some inode ->
        let priority = inode.priority in
        remove inode;
        Some (inode.this, priority)
end
