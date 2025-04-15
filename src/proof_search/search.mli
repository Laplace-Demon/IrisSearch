module Make (G : sig
  type node

  val source : node
  val successors : node -> node list
  val terminate : node -> bool
  val estimate : node -> int
end) : sig
  val set_timeout : int -> unit
  val set_max_depth : int -> unit
  val search : unit -> G.node list option
end

exception Timeout
