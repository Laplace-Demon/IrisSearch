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
end) : sig
  open State

  val set_max_depth : int -> unit
  val search : unit -> state path option
end
