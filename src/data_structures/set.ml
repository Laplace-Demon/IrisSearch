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
  val split : elt -> t -> t * bool * t
  val find_first_opt : (elt -> bool) -> t -> elt option
  val find_last_opt : (elt -> bool) -> t -> elt option
  val map : (elt -> elt) -> t -> t
  val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val to_list : t -> elt list
  val to_seq : t -> elt Seq.t
  val of_list : elt list -> t
  val of_seq : elt Seq.t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end

module Make (HashOrd : HashedOrderedType) = struct
  module BabySet = Baby.W.Set.Make (HashOrd)

  type elt = HashOrd.t
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
  let diff = BabySet.diff

  let subset s1 s2 =
    Statistics.record_operation "Set.subset";
    BabySet.subset s1 s2

  let split e s =
    Statistics.record_operation "Set.split";
    BabySet.split e s

  let find_first_opt f t =
    Statistics.record_operation "Set.find_first_opt";
    BabySet.find_first_opt f t

  let find_last_opt f t =
    Statistics.record_operation "Set.find_last_opt";
    BabySet.find_last_opt f t

  let map = BabySet.map
  let fold = BabySet.fold
  let to_list = BabySet.to_list
  let to_seq = BabySet.to_seq
  let of_list = BabySet.of_list
  let of_seq = BabySet.of_seq
  let equal = BabySet.equal
  let compare = BabySet.compare

  let hash m =
    let c = 42 in
    let card = BabySet.cardinal m in
    let len = min c card in
    Array.fold_left
      (fun h e -> Hashtbl.hash (h, HashOrd.hash e))
      card
      (Array.sub (BabySet.to_array m) 0 len)
end
