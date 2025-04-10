module Make (G : sig
  type node

  val source : node
  val successors : node -> node list
  val terminate : node -> bool
  val estimate : node -> int
end) : sig
  val search : (G.node -> unit) -> G.node list option
end

exception Timeout
