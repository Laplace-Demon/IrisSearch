module Make (G : sig
  type node

  val source : node
  val successors : node -> node list
  val estimate : node -> int

  exception Termination
end) : sig
  val set_max_depth : int -> unit
  val search : unit -> G.node list option
end
