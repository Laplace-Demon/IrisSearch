module type Set = sig
  type elt
  type t

  val empty : t
  val is_empty : t -> bool
  val cardinal : t -> int
  val singleton : elt -> t
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val subset : t -> t -> bool
  val map : (elt -> elt) -> t -> t
  val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val to_list : t -> elt list
  val of_list : elt list -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make (Ord : Baby.OrderedType) : Set with type elt = Ord.t
