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
  val partition : (elt -> Multiplicity.t -> bool) -> t -> t * t
  val map : (elt -> Multiplicity.t -> Multiplicity.t) -> t -> t
  val fold : (elt -> Multiplicity.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val iter : (elt -> Multiplicity.t -> unit) -> t -> unit
  val get : t -> int -> elt * Multiplicity.t
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

  let mem e s =
    Statistics.record_operation "Multiset.mem";
    BabyMap.mem e s

  let add e v s =
    Statistics.record_operation "Multiset.add";
    BabyMap.update e
      (function None -> Some v | Some v' -> Some (Multiplicity.add v v'))
      s

  let remove e s =
    Statistics.record_operation "Multiset.remove";
    BabyMap.remove e s

  let union s1 s2 =
    Statistics.record_operation "Multiset.union";
    BabyMap.union (fun _ v1 v2 -> Some (Multiplicity.add v1 v2)) s1 s2

  let inter s1 s2 =
    Statistics.record_operation "Multiset.inter";
    BabyMap.inter (fun _ v1 v2 -> Some (Multiplicity.min v1 v2)) s1 s2

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

  let partition f s =
    Statistics.record_operation "Multiset.partition";
    BabyMap.partition f s

  let map f s =
    Statistics.record_operation "Multiset.map";
    BabyMap.mapi f s

  let fold f s init =
    Statistics.record_operation "Multiset.fold";
    BabyMap.fold f s init

  let iter f s =
    Statistics.record_operation "Multiset.iter";
    BabyMap.iter f s

  let get s i =
    Statistics.record_operation "Multiset.get";
    BabyMap.get s i

  let to_list s =
    Statistics.record_operation "Multiset.to_list";
    BabyMap.to_list s

  let to_seq s =
    Statistics.record_operation "Multiset.to_seq";
    BabyMap.to_seq s

  let of_list el =
    Statistics.record_operation "Multiset.of_list";
    BabyMap.of_list el

  let of_seq el =
    Statistics.record_operation "Multiset.of_seq";
    BabyMap.of_seq el

  let equal s1 s2 =
    Statistics.record_operation "Multiset.equal";
    BabyMap.equal Multiplicity.equal s1 s2

  let compare s1 s2 =
    Statistics.record_operation "Multiset.compare";
    BabyMap.compare Multiplicity.compare s1 s2

  let hash m =
    Statistics.record_operation "Multiset.hash";
    let c = 42 in
    let card = BabyMap.cardinal m in
    let len = min c card in
    Array.fold_left
      (fun h (e, v) -> Hashtbl.hash (h, HashOrd.hash e, Multiplicity.hash v))
      card
      (Array.sub (BabyMap.to_array m) 0 len)
end
