open State
open Edge_info

module Successor : sig
  type successor

  val get_succ_or : successor -> (state * edge_info) list
  val get_succ_and : successor -> (state * edge_info) list
end

open Successor

val state_size : state -> int * int

exception Inconsistent of (state * edge_info) option * edge_info

val initial : Ast.instance -> state
val successors : state -> successor
val consistent : state -> bool
