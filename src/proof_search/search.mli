open Path

module Make (State : sig
  type state

  val source : state
  val successors : state -> state list * bool

  exception Inconsistent
end) : sig
  open State

  val set_max_depth : int -> unit
  val search : unit -> state path option
end
