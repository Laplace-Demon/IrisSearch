module type HashedOrderedType = sig
  include Baby.OrderedType

  val hash : t -> int
end

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
  val partition : (elt -> bool) -> t -> t * t
  val to_list : t -> elt list
  val to_seq : t -> elt Seq.t
  val of_list : elt list -> t
  val of_seq : elt Seq.t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end

module Make (HashOrd : HashedOrderedType) : Set with type elt = HashOrd.t
