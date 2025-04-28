module type Multiset = sig
  type elt
  type t

  val empty : t
  val is_empty : t -> bool
  val cardinal : t -> int
  val singleton : elt -> Multiplicity.t -> t
  val mem : elt -> t -> bool
  val add : elt -> Multiplicity.t -> t -> t
  val remove : elt -> Multiplicity.t -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t * bool
  val subset : t -> t -> bool
  val map : (elt -> elt) -> t -> t
  val map_multiplicity : (elt -> Multiplicity.t -> Multiplicity.t) -> t -> t
  val iter : (elt -> Multiplicity.t -> unit) -> t -> unit
  val fold : (elt -> Multiplicity.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val to_list : t -> (elt * Multiplicity.t) list
  val of_list : (elt * Multiplicity.t) list -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make (Ord : Baby.OrderedType) = struct
  module BabyMap = Baby.W.Map.Make (Ord)

  type elt = Ord.t
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

  let remove e v s =
    BabyMap.update e
      (function None -> None | Some v' -> Multiplicity.sub_opt v' v)
      s

  let union s1 s2 =
    Statistics.record_operation "Multiset.union";
    BabyMap.union (fun _ v1 v2 -> Some (Multiplicity.add v1 v2)) s1 s2

  let inter = BabyMap.inter (fun _ v1 v2 -> Some (Multiplicity.min v1 v2))

  let diff s1 s2 =
    Statistics.record_operation "Multiset.diff";
    let is_inf = ref true in
    let merged_tree =
      BabyMap.merge
        (fun _ o1 o2 ->
          match (o1, o2) with
          | None, None -> None
          | None, _ -> raise Multiplicity.Underflow
          | _, None -> o1
          | Some v1, Some v2 ->
              is_inf := !is_inf && Multiplicity.is_infinite v1;
              Multiplicity.sub_exn v1 v2)
        s1 s2
    in
    (merged_tree, !is_inf)

  let subset s1 s2 =
    Statistics.record_operation "Multiset.subset";
    BabyMap.sub (fun v1 v2 -> Multiplicity.compare v1 v2 <= 0) s1 s2

  let map f s =
    Statistics.record_operation "Multiset.map";
    s |> BabyMap.to_list |> List.map (fun (e, v) -> (f e, v)) |> BabyMap.of_list

  let map_multiplicity =
    Statistics.record_operation "Multiset.map_multiplicity";
    BabyMap.mapi

  let iter f s =
    Statistics.record_operation "Multiset.iter";
    BabyMap.iter f s

  let fold f s init =
    Statistics.record_operation "Multiset.fold";
    BabyMap.fold f s init

  let to_list = BabyMap.to_list
  let of_list = BabyMap.of_list
  let equal = BabyMap.equal Multiplicity.equal
  let compare = BabyMap.compare Multiplicity.compare
end

module type Multiset2 = sig
  type elt1
  type elt2
  type elt = elt1 * elt2
  type t

  val empty : t
  val is_empty : t -> bool
  val cardinal : t -> int
  val singleton : elt -> Multiplicity.t -> t
  val mem : elt -> t -> bool
  val mem1 : elt1 -> t -> bool
  val add : elt -> Multiplicity.t -> t -> t
  val remove : elt -> Multiplicity.t -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t * bool
  val subset : t -> t -> bool
  val map : (elt2 -> elt2) -> t -> t
  val map_multiplicity : (elt -> Multiplicity.t -> Multiplicity.t) -> t -> t
  val iter : (elt -> Multiplicity.t -> unit) -> t -> unit
  val fold : (elt -> Multiplicity.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val to_list : t -> (elt * Multiplicity.t) list
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make2 (Ord1 : Baby.OrderedType) (Ord2 : Baby.OrderedType) = struct
  module BabyMap = Baby.W.Map.Make (Ord1)
  module Mset = Make (Ord2)

  type elt1 = Ord1.t
  type elt2 = Ord2.t
  type elt = elt1 * elt2
  type t = Mset.t BabyMap.map

  let empty = BabyMap.empty
  let is_empty = BabyMap.is_empty
  let cardinal s = BabyMap.fold (fun e1 t acc -> acc + Mset.cardinal t) s 0
  let singleton (e1, e2) v = BabyMap.singleton e1 (Mset.singleton e2 v)

  let mem (e1, e2) s =
    match BabyMap.find_opt e1 s with None -> false | Some t -> Mset.mem e2 t

  let mem1 e1 s = BabyMap.mem e1 s

  let add (e1, e2) v s =
    BabyMap.update e1
      (function
        | None -> Some (Mset.singleton e2 v) | Some t -> Some (Mset.add e2 v t))
      s

  let remove (e1, e2) v s =
    BabyMap.update e1
      (function
        | None -> None
        | Some t ->
            let t' = Mset.remove e2 v t in
            if Mset.is_empty t' then None else Some t')
      s

  let union s1 s2 = BabyMap.union (fun _ t1 t2 -> Some (Mset.union t1 t2)) s1 s2
  let inter s1 s2 = BabyMap.inter (fun _ t1 t2 -> Some (Mset.inter t1 t2)) s1 s2

  let diff s1 s2 =
    let is_inf = ref true in
    let merged_tree =
      BabyMap.merge
        (fun _ o1 o2 ->
          match (o1, o2) with
          | None, None -> None
          | None, _ -> raise Multiplicity.Underflow
          | _, None -> o1
          | Some t1, Some t2 ->
              let t, is_inf' = Mset.diff t1 t2 in
              is_inf := !is_inf && is_inf';
              Some t)
        s1 s2
    in
    (merged_tree, !is_inf)

  let subset s1 s2 = BabyMap.sub (fun t1 t2 -> Mset.subset t1 t2) s1 s2
  let map f s = BabyMap.map (fun t -> Mset.map f t) s

  let map_multiplicity f s =
    BabyMap.mapi (fun e1 t -> Mset.map_multiplicity (fun e2 -> f (e1, e2)) t) s

  let iter f s =
    BabyMap.iter (fun e1 t -> Mset.iter (fun e2 v -> f (e1, e2) v) t) s

  let fold f s init =
    BabyMap.fold (fun e1 t acc -> Mset.fold (fun e2 -> f (e1, e2)) t acc) s init

  let to_list s =
    List.concat_map
      (fun (e1, t) -> List.map (fun (e2, v) -> ((e1, e2), v)) (Mset.to_list t))
      (BabyMap.to_list s)

  let equal = BabyMap.equal Mset.equal
  let compare = BabyMap.compare Mset.compare
end
