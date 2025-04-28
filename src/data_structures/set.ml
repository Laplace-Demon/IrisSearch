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
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val to_list : t -> elt list
  val of_list : elt list -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make (Ord : Baby.OrderedType) = struct
  module BabySet = Baby.W.Set.Make (Ord)

  type elt = Ord.t
  type t = BabySet.set

  let empty = BabySet.empty
  let is_empty = BabySet.is_empty
  let cardinal = BabySet.cardinal
  let singleton e = BabySet.singleton e
  let mem = BabySet.mem
  let add = BabySet.add
  let remove = BabySet.remove
  let union = BabySet.union
  let inter = BabySet.inter

  let diff s1 s2 =
    if BabySet.subset s1 s2 then BabySet.diff s1 s2
    else raise Multiplicity.Underflow

  let subset s1 s2 =
    Statistics.record_operation "Set.subset";
    BabySet.subset s1 s2

  let map = BabySet.map
  let fold = BabySet.fold
  let iter = BabySet.iter
  let to_list = BabySet.to_list
  let of_list = BabySet.of_list
  let equal = BabySet.equal
  let compare = BabySet.compare
end
