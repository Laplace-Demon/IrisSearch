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
  val diff : t -> t -> t * bool
  val subset : t -> t -> bool
  val map : (elt -> elt) -> t -> t
  val map_multiplicity : (elt -> Multiplicity.t -> Multiplicity.t) -> t -> t
  val fold : (elt -> Multiplicity.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val to_list : t -> (elt * Multiplicity.t) list
  val of_list : (elt * Multiplicity.t) list -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make (Ord : Baby.OrderedType) : Multiset with type elt = Ord.t

module type Multiset2 = sig
  type elt1
  type elt2
  type elt = elt1 * elt2
  type t

  val empty : t
  val is_empty : t -> bool
  val cardinal : t -> int
  val singleton : elt -> Multiplicity.t -> t
  val mem : elt -> t -> bool
  val mem1 : elt1 -> t -> bool
  val add : elt -> Multiplicity.t -> t -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t * bool
  val subset : t -> t -> bool
  val map : (elt2 -> elt2) -> t -> t
  val map_multiplicity : (elt -> Multiplicity.t -> Multiplicity.t) -> t -> t
  val fold : (elt -> Multiplicity.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val to_list : t -> (elt * Multiplicity.t) list
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make2 (Ord1 : Baby.OrderedType) (Ord2 : Baby.OrderedType) :
  Multiset2 with type elt1 = Ord1.t and type elt2 = Ord2.t
