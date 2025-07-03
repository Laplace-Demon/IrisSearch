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
end) : sig
  open State

  val set_max_depth : int -> unit
  val search : unit -> state path option
  val pp_search_graph_debug : formatter -> unit -> unit
end
