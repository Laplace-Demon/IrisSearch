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
  val forall : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
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

  let mem e s =
    Statistics.record_operation "Set.mem";
    BabySet.mem e s

  let add e s =
    Statistics.record_operation "Set.add";
    BabySet.add e s

  let remove e s =
    Statistics.record_operation "Set.remove";
    BabySet.remove e s

  let union s1 s2 =
    Statistics.record_operation "Set.union";
    BabySet.union s1 s2

  let inter s1 s2 =
    Statistics.record_operation "Set.inter";
    BabySet.inter s1 s2

  let diff s1 s2 =
    Statistics.record_operation "Set.diff";
    if BabySet.subset s1 s2 then BabySet.diff s1 s2
    else raise Multiplicity.Underflow

  let subset s1 s2 =
    Statistics.record_operation "Set.subset";
    BabySet.subset s1 s2

  let forall f s =
    Statistics.record_operation "Set.forall";
    BabySet.for_all f s

  let exists f s =
    Statistics.record_operation "Set.exists";
    BabySet.exists f s

  let map f s =
    Statistics.record_operation "Set.map";
    BabySet.map f s

  let fold f s init =
    Statistics.record_operation "Set.fold";
    BabySet.fold f s init

  let iter f s =
    Statistics.record_operation "Set.init";
    BabySet.iter f s

  let to_list s =
    Statistics.record_operation "Set.to_list";
    BabySet.to_list s

  let of_list l =
    Statistics.record_operation "Set.of_list";
    BabySet.of_list l

  let equal s1 s2 =
    Statistics.record_operation "Set.equal";
    BabySet.equal s1 s2

  let compare s1 s2 =
    Statistics.record_operation "Set.compare";
    BabySet.compare s1 s2
end
