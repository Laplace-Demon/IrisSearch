module type HashedOrderedType = sig
  include Baby.OrderedType

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
  val add : elt -> Multiplicity.t -> t -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val factor : t -> t -> Multiplicity.t
  val diff : t -> t -> t
  val diff_multiple : Multiplicity.t -> t -> t -> t
  val subset : t -> t -> bool
  val partition : (elt -> Multiplicity.t -> bool) -> t -> t * t
  val map : (elt -> Multiplicity.t -> Multiplicity.t) -> t -> t
  val fold : (elt -> Multiplicity.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val to_list : t -> (elt * Multiplicity.t) list
  val to_seq : t -> (elt * Multiplicity.t) Seq.t
  val of_list : (elt * Multiplicity.t) list -> t
  val of_seq : (elt * Multiplicity.t) Seq.t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end

module Make (HashOrd : HashedOrderedType) : Multiset with type elt = HashOrd.t
