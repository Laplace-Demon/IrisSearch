open Baby

module Multiplicity : sig
  type t = Finite of int | Infinite

  val add : t -> t -> t
  val sub : t -> t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val min : t -> t -> t
  val hash : t -> int
end

module type HashedOrderedType = sig
  include OrderedType

  val hash : t -> int
end

module type Multiset = sig
  type elt
  type t

  val empty : t
  val is_empty : t -> bool
  val cardinal : t -> int
  val singleton : elt -> Multiplicity.t -> t
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val subset : t -> t -> bool
  val partition : (elt -> Multiplicity.t -> bool) -> t -> t * t
  val to_list : t -> (elt * Multiplicity.t) list
  val of_list : (elt * Multiplicity.t) list -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end

module Make (HashOrd : HashedOrderedType) : Multiset with type elt = HashOrd.t
