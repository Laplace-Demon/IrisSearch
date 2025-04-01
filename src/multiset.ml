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
  module L : Multiset with type elt = HashOrd.t = struct
    type elt = HashOrd.t
    type t = (elt * int) list

    let empty = []
    let is_empty = function [] -> true | _ -> false
    let rec cardinal = function [] -> 0 | _ :: l -> 1 + cardinal l
    let singleton e = [ (e, 1) ]

    let rec mem e = function
      | [] -> false
      | (e', _) :: l ->
          let c = HashOrd.compare e e' in
          if c < 0 then false else if c == 0 then true else mem e l

    let add e l =
      let rec add_aux = function
        | [] -> singleton e
        | p :: l' as l ->
            let e', i = p in
            let c = HashOrd.compare e e' in
            if c == 0 then (e', i + 1) :: l'
            else if c < 0 then (e, 1) :: l
            else p :: add_aux l'
      in
      add_aux l

    let remove e l =
      let rec remove_aux l =
        match l with
        | [] -> []
        | p :: l' as l ->
            let e', i = p in
            let c = HashOrd.compare e e' in
            if c == 0 then l' else if c < 0 then l else p :: remove_aux l'
      in
      remove_aux l

    let rec union l1 l2 =
      match (l1, l2) with
      | [], _ -> l2
      | _, [] -> l1
      | p1 :: l1', p2 :: l2' ->
          let e1, i1 = p1 in
          let e2, i2 = p2 in
          let c = HashOrd.compare e1 e2 in
          if c == 0 then (e1, i1 + i2) :: union l1' l2'
          else if c < 0 then p1 :: union l1' l2
          else p2 :: union l1 l2'

    let rec inter l1 l2 =
      match (l1, l2) with
      | [], _ -> []
      | _, [] -> []
      | p1 :: l1', p2 :: l2' ->
          let e1, i1 = p1 in
          let e2, i2 = p2 in
          let c = HashOrd.compare e1 e2 in
          if c == 0 then (e1, min i1 i2) :: inter l1' l2'
          else if c < 0 then inter l1' l2
          else inter l1 l2'

    let rec diff l1 l2 =
      match (l1, l2) with
      | [], _ -> []
      | _, [] -> l1
      | p1 :: l1', p2 :: l2' ->
          let e1, i1 = p1 in
          let e2, i2 = p2 in
          let c = HashOrd.compare e1 e2 in
          if c == 0 then
            if i1 > i2 then (e1, i1 - i2) :: diff l1' l2' else diff l1' l2'
          else if c < 0 then p1 :: diff l1' l2
          else diff l1 l2'

    let rec subset l1 l2 =
      match (l1, l2) with
      | [], _ -> true
      | _, [] -> false
      | p1 :: l1', p2 :: l2' ->
          let e1, i1 = p1 in
          let e2, i2 = p2 in
          let c = HashOrd.compare e1 e2 in
          if c == 0 then if i1 > i2 then false else subset l1' l2'
          else if c < 0 then false
          else subset l1 l2'

    let to_list l = l

    (* This function is not adaptive, l should be sorted *)
    let of_list l = l

    let rec equal l1 l2 =
      match (l1, l2) with
      | [], [] -> true
      | (e1, i1) :: l1', (e2, i2) :: l2' -> e1 == e2 && i1 = i2 && equal l1' l2'
      | _, _ -> false

    let rec hash = function
      | [] -> 0
      | (e, i) :: l -> Hashtbl.hash (HashOrd.hash e, i, hash l)
  end

  module T : Multiset with type elt = HashOrd.t = struct
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

    (* Should not be called *)
    let hash = fun _ -> assert false
  end

  let l2t l = L.to_list l |> T.of_list
  let t2l t = T.to_list t |> L.of_list
end
