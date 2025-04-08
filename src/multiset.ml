open Baby

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

module Make (HashOrd : HashedOrderedType) = struct
  module Map = Baby.H.Map.Make (HashOrd)

  type elt = HashOrd.t
  type t = Multiplicity.t Map.map

  let empty = Map.empty
  let is_empty = Map.is_empty
  let cardinal = Map.cardinal
  let singleton e = Map.singleton e
  let mem = Map.mem
  let add e = Map.add e (Multiplicity.Finite 1)
  let remove = Map.remove
  let union = Map.union (fun _ v1 v2 -> Some (Multiplicity.add v1 v2))
  let inter = Map.inter (fun _ v1 v2 -> Some (Multiplicity.min v1 v2))

  let diff =
    Map.merge (fun _ o1 o2 ->
        match (o1, o2) with
        | None, _ -> None
        | Some _, None -> o1
        | Some v1, Some v2 ->
            if Multiplicity.compare v1 v2 > 0 then Some (Multiplicity.sub v1 v2)
            else None)

  let subset = Map.sub (fun v1 v2 -> Multiplicity.compare v1 v2 <= 0)
  let partition = Map.partition
  let to_list = Map.to_list
  let of_list = Map.of_list
  let equal = Map.equal Multiplicity.equal
  let compare = Map.compare Multiplicity.compare

  let hash m =
    let c = 42 in
    let card = Map.cardinal m in
    let len = min c card in
    Array.fold_left
      (fun h (e, v) -> Hashtbl.hash (h, HashOrd.hash e, Multiplicity.hash v))
      card
      (Array.sub (Map.to_array m) 0 len)
end
