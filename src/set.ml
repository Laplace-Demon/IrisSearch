open Baby

module type HashedOrderedType = sig
  include OrderedType

  val hash : t -> int
end

module Set = struct
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
    val of_list : elt list -> t
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
    let subset = BabySet.subset
    let partition = BabySet.partition
    let to_list = BabySet.to_list
    let of_list = BabySet.of_list
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
end

module Multiset = struct
  module Multiplicity = struct
    type t = Finite of int | Infinite

    let add t1 t2 =
      match (t1, t2) with
      | Infinite, _ | _, Infinite -> Infinite
      | Finite i1, Finite i2 -> Finite (i1 + i2)

    let sub t1 t2 =
      match (t1, t2) with
      | Infinite, _ -> Infinite
      | Finite i1, Finite i2 when i1 > i2 -> Finite (i1 - i2)
      | _, _ -> assert false

    let equal t1 t2 =
      match (t1, t2) with
      | Infinite, Infinite -> true
      | Finite i1, Finite i2 -> Int.equal i1 i2
      | _, _ -> false

    let compare t1 t2 =
      match (t1, t2) with
      | Infinite, Infinite -> 0
      | Infinite, _ -> 1
      | _, Infinite -> -1
      | Finite i1, Finite i2 -> Int.compare i1 i2

    let min t1 t2 =
      match (t1, t2) with
      | Infinite, _ -> t2
      | _, Infinite -> t1
      | Finite i1, Finite i2 -> Finite (Int.min i1 i2)

    let hash = function Infinite -> 42 | Finite i -> i
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
    val diff : t -> t -> t
    val subset : t -> t -> bool
    val partition : (elt -> Multiplicity.t -> bool) -> t -> t * t
    val to_list : t -> (elt * Multiplicity.t) list
    val of_list : (elt * Multiplicity.t) list -> t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
  end

  module Make (HashOrd : HashedOrderedType) = struct
    module BabyMap = Baby.W.Map.Make (HashOrd)

    type elt = HashOrd.t
    type t = Multiplicity.t BabyMap.map

    let empty = BabyMap.empty
    let is_empty = BabyMap.is_empty
    let cardinal = BabyMap.cardinal
    let singleton e = BabyMap.singleton e
    let mem = BabyMap.mem
    let add e v = BabyMap.update e (let open Multiplicity in function None -> Some v | Some v' -> Some (add v v'))
    let remove = BabyMap.remove
    let union = BabyMap.union (fun _ v1 v2 -> Some (Multiplicity.add v1 v2))
    let inter = BabyMap.inter (fun _ v1 v2 -> Some (Multiplicity.min v1 v2))

    let diff =
      BabyMap.merge (fun _ o1 o2 ->
          match (o1, o2) with
          | None, _ -> None
          | Some _, None -> o1
          | Some v1, Some v2 ->
              if Multiplicity.compare v1 v2 > 0 then
                Some (Multiplicity.sub v1 v2)
              else None)

    let subset = BabyMap.sub (fun v1 v2 -> Multiplicity.compare v1 v2 <= 0)
    let partition = BabyMap.partition
    let to_list = BabyMap.to_list
    let of_list = BabyMap.of_list
    let equal = BabyMap.equal Multiplicity.equal
    let compare = BabyMap.compare Multiplicity.compare

    let hash m =
      let c = 42 in
      let card = BabyMap.cardinal m in
      let len = min c card in
      Array.fold_left
        (fun h (e, v) -> Hashtbl.hash (h, HashOrd.hash e, Multiplicity.hash v))
        card
        (Array.sub (BabyMap.to_array m) 0 len)
  end
end
