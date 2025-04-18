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
  val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val exists : (elt -> bool) -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val get : t -> int -> elt
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

  let mem e s =
    Statistics.record_operation "Set.mem";
    BabySet.mem e s

  let add e s =
    Statistics.record_operation "Set.add";
    BabySet.add e s

  let remove e s =
    Statistics.record_operation "Set.add";
    BabySet.remove e s

  let union s1 s2 =
    Statistics.record_operation "Set.union";
    BabySet.union s1 s2

  let inter s1 s2 =
    Statistics.record_operation "Set.inter";
    BabySet.inter s1 s2

  let diff s1 s2 =
    Statistics.record_operation "Set.diff";
    BabySet.diff s1 s2

  let subset s1 s2 =
    Statistics.record_operation "Set.subset";
    BabySet.subset s1 s2

  let partition f s =
    Statistics.record_operation "Set.partition";
    BabySet.partition f s

  let fold f s init =
    Statistics.record_operation "Set.fold";
    BabySet.fold f s init

  let exists f s =
    Statistics.record_operation "Set.exists";
    BabySet.exists f s

  let iter f s =
    Statistics.record_operation "Set.iter";
    BabySet.iter f s

  let get s i =
    Statistics.record_operation "Set.get";
    BabySet.get s i

  let to_list s =
    Statistics.record_operation "Set.to_list";
    BabySet.to_list s

  let to_seq s =
    Statistics.record_operation "Set.to_seq";
    BabySet.to_seq s

  let of_list el =
    Statistics.record_operation "Set.of_list";
    BabySet.of_list el

  let of_seq el =
    Statistics.record_operation "Set.of_seq";
    BabySet.of_seq el

  let equal s1 s2 =
    Statistics.record_operation "Set.equal";
    BabySet.equal s1 s2

  let compare s1 s2 =
    Statistics.record_operation "Set.compare";
    BabySet.compare s1 s2

  let hash m =
    Statistics.record_operation "Set.hash";
    let c = 42 in
    let card = BabySet.cardinal m in
    let len = min c card in
    Array.fold_left
      (fun h e -> Hashtbl.hash (h, HashOrd.hash e))
      card
      (Array.sub (BabySet.to_array m) 0 len)
end
