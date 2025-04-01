module Make (G : sig
  type node

  val source : node
  val successors : node -> node list
  val terminate : node -> bool
  val estimate : node -> int
end) : sig
  (* A path (from a target node back to some source node) is described by a
     series of labels and ends in a source node. *)

  type path = Edge of G.node * path | Source of G.node

  (* Search. Newly discovered nodes are presented to the user, in order of
     increasing distance from the source nodes, by invoking the user-supplied
     function [f]. At the end, a mapping of nodes to distances to the source
     nodes and a mapping of nodes to shortest paths are returned. *)
  val search : (G.node * path -> unit) -> path option
end
