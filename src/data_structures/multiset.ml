module type HashedOrderedType = sig
  include Baby.OrderedType

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
  val add : elt -> Multiplicity.t -> t -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val factor : t -> t -> Multiplicity.t
  val diff : t -> t -> t
  val diff_multiple : Multiplicity.t -> t -> t -> t
  val subset : t -> t -> bool
  val split : elt -> t -> t * Multiplicity.t option * t
  val find_first_opt : (elt -> bool) -> t -> (elt * Multiplicity.t) option
  val find_last_opt : (elt -> bool) -> t -> (elt * Multiplicity.t) option
  val map : (elt -> elt) -> t -> t
  val map_multiplicity : (elt -> Multiplicity.t -> Multiplicity.t) -> t -> t
  val fold : (elt -> Multiplicity.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val to_list : t -> (elt * Multiplicity.t) list
  val to_seq : t -> (elt * Multiplicity.t) Seq.t
  val of_list : (elt * Multiplicity.t) list -> t
  val of_seq : (elt * Multiplicity.t) Seq.t -> t
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

  let add e v =
    BabyMap.update e (function
      | None -> Some v
      | Some v' -> Some (Multiplicity.add v v'))

  let remove = BabyMap.remove

  let union s1 s2 =
    Statistics.record_operation "Multiset.union";
    BabyMap.union (fun _ v1 v2 -> Some (Multiplicity.add v1 v2)) s1 s2

  let inter = BabyMap.inter (fun _ v1 v2 -> Some (Multiplicity.min v1 v2))

  let factor s1 s2 =
    Statistics.record_operation "Multiset.factor";
    let factor = ref Multiplicity.inf in
    let _ =
      BabyMap.merge
        (fun _ o1 o2 ->
          let () =
            match (o1, o2) with
            | _, None -> ()
            | None, _ -> raise Multiplicity.Underflow
            | Some v1, Some v2 ->
                factor := Multiplicity.min !factor (Multiplicity.div v1 v2)
          in
          None)
        s1 s2
    in
    !factor

  let diff s1 s2 =
    Statistics.record_operation "Multiset.diff";
    BabyMap.merge
      (fun _ o1 o2 ->
        match (o1, o2) with
        | None, None -> None
        | None, _ -> raise Multiplicity.Underflow
        | _, None -> o1
        | Some v1, Some v2 -> Multiplicity.sub v1 v2)
      s1 s2

  let diff_multiple factor s1 s2 =
    Statistics.record_operation "Multiset.diff_multiple";
    BabyMap.merge
      (fun _ o1 o2 ->
        match (o1, o2) with
        | None, None -> None
        | None, _ -> raise Multiplicity.Underflow
        | _, None -> o1
        | Some v1, Some v2 -> Multiplicity.sub v1 (Multiplicity.mul v2 factor))
      s1 s2

  let subset s1 s2 =
    Statistics.record_operation "Multiset.subset";
    BabyMap.sub (fun v1 v2 -> Multiplicity.compare v1 v2 <= 0) s1 s2

  let split e s =
    Statistics.record_operation "Multiset.split";
    BabyMap.split e s

  let find_first_opt f s =
    Statistics.record_operation "Multiset.find_first_opt";
    BabyMap.find_first_opt f s

  let find_last_opt f s =
    Statistics.record_operation "Multiset.find_last_opt";
    BabyMap.find_last_opt f s

  let map f s =
    Statistics.record_operation "Multiset.map";
    s |> BabyMap.to_seq |> Seq.map (fun (e, v) -> (f e, v)) |> BabyMap.of_seq

  let map_multiplicity =
    Statistics.record_operation "Multiset.map_multiplicity";
    BabyMap.mapi

  let fold f s init =
    Statistics.record_operation "Multiset.fold";
    BabyMap.fold f s init

  let to_list = BabyMap.to_list
  let to_seq = BabyMap.to_seq
  let of_list = BabyMap.of_list
  let of_seq = BabyMap.of_seq
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
