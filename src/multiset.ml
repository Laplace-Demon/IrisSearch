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

module Make (HashOrd : HashedOrderedType) = struct
    module Map = Baby.H.Map.Make (HashOrd)

    type elt = HashOrd.t
    type t = int Map.map

    let empty = Map.empty
    let is_empty = Map.is_empty
    let cardinal = Map.cardinal
    let singleton e = Map.singleton e 1
    let mem = Map.mem
    let add e = Map.add e 1
    let remove = Map.remove
    let union = Map.union (fun _ v1 v2 -> Some (v1 + v2))
    let inter = Map.inter (fun _ v1 v2 -> Some (min v1 v2))

    let diff =
      Map.merge (fun _ o1 o2 ->
          match (o1, o2) with
          | None, _ -> None
          | Some _, None -> o1
          | Some i1, Some i2 -> if i1 > i2 then Some (i1 - i2) else None)

    let subset = Map.sub ( <= )
    let to_list = Map.to_list
    let of_list = Map.of_list
    let equal = Map.equal ( = )

    let hash m =
      (* hash a fixed number of elements in the map *)
      let c = 42 in
      let card = Map.cardinal m in
      let len = min c card in
      Array.fold_left (fun h (e, n) -> Hashtbl.hash (h, HashOrd.hash e, n)) card
      (Array.sub (Map.to_array m) 0 len)

end
