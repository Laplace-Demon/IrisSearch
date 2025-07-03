(** Definition of datatype storing edge information. *)

type edge_info = { simple : string; verbose : string }

let empty_edge_info = { simple = ""; verbose = "" }

let is_empty_edge_info { simple; verbose } =
  String.equal simple "" && String.equal verbose ""
