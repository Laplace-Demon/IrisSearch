module type Type = sig
  type t
end

module type PriorityQueue = sig
  type node

  val add : node -> int -> unit
  val get : unit -> (node * int) option
end

module Make (Node : Type) : PriorityQueue with type node = Node.t
