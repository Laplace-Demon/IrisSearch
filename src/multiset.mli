open Baby

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
  val singleton : elt -> t
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val subset : t -> t -> bool
  val to_list : t -> (elt * int) list
  val of_list : (elt * int) list -> t
  val equal : t -> t -> bool
  val hash : t -> int
end

module Make (HashOrd : HashedOrderedType) : Multiset with type elt = HashOrd.t
