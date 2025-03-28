module Make (G : sig
  type node

  include Hashtbl.HashedType with type t := node

  val sources : (node -> unit) -> unit
  val successors : node -> (int -> node -> unit) -> unit
  val terminate : node -> bool
  val estimate : node -> int
end) : sig
  (* A path (from a target node back to some source node) is described by a
     series of labels and ends in a source node. *)

  type path = Edge of G.node * path | Source of G.node

  (* A path can also be presented as a pair of a source node and a list of
     labels, which describe the edges from the source node to a target node. *)

  val reverse : path -> G.node * G.node list

  (* Search. Newly discovered nodes are presented to the user, in order of
     increasing distance from the source nodes, by invoking the user-supplied
     function [f]. At the end, a mapping of nodes to distances to the source
     nodes and a mapping of nodes to shortest paths are returned. *)
  val search : (G.node * path -> unit) -> path option
end
