open Hashcons

type 'a elt = 'a hash_consed

let compare_hc x y = compare x.tag y.tag
let rec repeat e l c = if c == 0 then l else e :: repeat e l (c - 1)

module type Multiset = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val cardinal : 'a t -> int
  val singleton : 'a elt -> 'a t
  val mem : 'a elt -> 'a t -> int option
  val add : ?cnt:int -> 'a elt -> 'a t -> 'a t
  val remove : ?cnt:int -> 'a elt -> 'a t -> 'a t
  val union : 'a t -> 'a t -> 'a t
  val inter : 'a t -> 'a t -> 'a t
  val diff : 'a t -> 'a t -> 'a t
  val subset : 'a t -> 'a t -> bool
  val to_list : 'a t -> 'a elt list
  val of_list : 'a elt list -> 'a t
  val equal : 'a t -> 'a t -> bool
  val hash : 'a t -> int
end

module L : Multiset = struct
  type 'a t = 'a elt list

  let empty = []
  let is_empty = function [] -> true | _ -> false
  let rec cardinal = function [] -> 0 | _ :: l -> 1 + cardinal l
  let singleton e = [ e ]

  let rec mem e = function
    | [] -> None
    | e' :: l ->
        let c = compare_hc e e' in
        if c < 0 then None
        else if c == 0 then
          match mem e l with Some i -> Some (i + 1) | None -> Some 1
        else mem e l

  let add ?(cnt = 1) e l =
    let rec add_aux = function
      | [] -> repeat e [] cnt
      | e' :: l' as l ->
          let c = compare_hc e e' in
          if c <= 0 then repeat e l cnt else e' :: add_aux l'
    in
    if cnt <= 0 then
      raise
        (Invalid_argument
           "Multiset.add: optional parameter 'cnt' must be strictly positive.")
    else add_aux l

  let remove ?(cnt = 1) e l =
    let rec remove_aux cnt l =
      if cnt == 0 then l
      else
        match l with
        | [] -> []
        | e' :: l' as l ->
            let c = compare_hc e e' in
            if c == 0 then remove_aux (cnt - 1) l'
            else if c < 0 then l
            else e' :: remove_aux cnt l'
    in
    if cnt <= 0 then
      raise
        (Invalid_argument
           "Multiset.remove: optional parameter 'cnt' must be strictly \
            positive.")
    else remove_aux cnt l

  let rec union l1 l2 =
    match (l1, l2) with
    | [], [] -> []
    | _, [] -> l1
    | [], _ -> l2
    | e1 :: l1', e2 :: l2' ->
        let c = compare_hc e1 e2 in
        if c == 0 then e1 :: e2 :: union l1' l2'
        else if c < 0 then e1 :: union l1' l2
        else e2 :: union l1 l2'

  let rec inter l1 l2 =
    match (l1, l2) with
    | [], _ -> []
    | _, [] -> []
    | e1 :: l1', e2 :: l2' ->
        let c = compare_hc e1 e2 in
        if c == 0 then e1 :: inter l1' l2'
        else if c < 0 then inter l1' l2
        else inter l1 l2'

  let rec diff l1 l2 =
    match (l1, l2) with
    | [], _ -> []
    | _, [] -> l1
    | e1 :: l1', e2 :: l2' ->
        let c = compare_hc e1 e2 in
        if c == 0 then diff l1' l2'
        else if c < 0 then e1 :: diff l1' l2
        else diff l1 l2'

  let rec subset l1 l2 =
    match (l1, l2) with
    | [], _ -> true
    | _, [] -> false
    | e1 :: l1', e2 :: l2' ->
        let c = compare_hc e1 e2 in
        if c == 0 then subset l1' l2'
        else if c < 0 then false
        else subset l1 l2'

  let to_list l = l
  let of_list l = List.fast_sort compare_hc l

  let rec equal l1 l2 =
    match (l1, l2) with
    | [], [] -> true
    | e1 :: l1', e2 :: l2' -> e1 == e2 && equal l1' l2'
    | _, _ -> false

  let hash = function [] -> 0 | e :: l -> 0
end

module R : Multiset = struct
  (** This multiset module is inspired by the integer set implementation in
      Jean-Christophe Filliâtre's Hashcons library
      https://opam.ocaml.org/packages/hashcons/. We reimplement it here in order
      to add an integer counter at leaves to allow duplicate elements. The
      implementation is unsafe because it can overflow. *)

  (** Multiset implemented using Radix tree. It has a nice property of being
      canonical, Consequently, two Radix trees have the same elements if and
      only if they are structurally equal. Invariant: the integer on leaf is
      always strictly positive. *)

  type 'a t =
    | Empty
    | Leaf of int * 'a hash_consed
    | Branch of int * int * 'a t * 'a t

  let empty = Empty
  let is_empty = function Empty -> true | _ -> false
  let singleton k = Leaf (1, k)

  let rec cardinal = function
    | Empty -> 0
    | Leaf (i, _) -> i
    | Branch (_, _, t0, t1) -> cardinal t0 + cardinal t1

  let unsigned_lt n m = n >= 0 && (m < 0 || n < m)
  let zero_bit k m = k land m == 0
  let lowest_bit x = x land -x
  let branching_bit p0 p1 = lowest_bit (p0 lxor p1)
  let mask p m = p land (m - 1)
  let match_prefix k p m = mask k m == p

  let rec mem k = function
    | Empty -> None
    | Leaf (i, j) -> if j == k then Some i else None
    | Branch (_, m, l, r) -> mem k (if zero_bit m k.tag then l else r)

  let join (p0, t0, p1, t1) =
    let m = branching_bit p0 p1 in
    if zero_bit p0 m then Branch (mask p0 m, m, t0, t1)
    else Branch (mask p0 m, m, t1, t0)

  let add ?(cnt = 1) k t =
    let rec add_aux = function
      | Empty -> Leaf (cnt, k)
      | Leaf (i, j) as t ->
          if j == k then Leaf (i + cnt, j)
          else join (k.tag, Leaf (cnt, k), j.tag, t)
      | Branch (p, m, t0, t1) as t ->
          if match_prefix k.tag p m then
            if zero_bit k.tag m then Branch (p, m, add_aux t0, t1)
            else Branch (p, m, t0, add_aux t1)
          else join (k.tag, Leaf (cnt, k), p, t)
    in
    if cnt <= 0 then
      raise
        (Invalid_argument
           "Multiset.add: optional parameter 'cnt' must be strictly positive.")
    else add_aux t

  let branch = function
    | _, _, Empty, t -> t
    | _, _, t, Empty -> t
    | p, m, t0, t1 -> Branch (p, m, t0, t1)

  let remove ?(cnt = 1) k t =
    let rec remove_aux = function
      | Empty -> Empty
      | Leaf (i, j) ->
          if j == k then if i > cnt then Leaf (i - cnt, j) else Empty else t
      | Branch (p, m, t0, t1) as t ->
          if match_prefix k.tag p m then
            if zero_bit k.tag m then branch (p, m, remove_aux t0, t1)
            else branch (p, m, t0, remove_aux t1)
          else t
    in
    if cnt <= 0 then
      raise
        (Invalid_argument
           "Multiset.remove: optional parameter 'cnt' must be strictly \
            positive.")
    else remove_aux t

  let rec merge = function
    | Empty, t | t, Empty -> t
    | Leaf (i, j), t | t, Leaf (i, j) -> add ~cnt:i j t
    | (Branch (p1, m1, l1, r1) as s), (Branch (p2, m2, l2, r2) as t) ->
        if m1 == m2 && match_prefix p2 p1 m1 then
          Branch (p1, m1, merge (l1, l2), merge (r1, r2))
        else if unsigned_lt m1 m2 && match_prefix p2 p1 m1 then
          if zero_bit p2 m1 then Branch (p1, m1, merge (l1, t), r1)
          else Branch (p1, m1, l1, merge (r1, t))
        else if unsigned_lt m2 m1 && match_prefix p1 p2 m2 then
          if zero_bit p1 m2 then Branch (p2, m2, merge (s, l2), r2)
          else Branch (p2, m2, l2, merge (s, r2))
        else join (p1, s, p2, t)

  let union s t = merge (s, t)

  let rec inter t1 t2 =
    match (t1, t2) with
    | Empty, _ -> Empty
    | _, Empty -> Empty
    | Leaf (i1, k1), _ -> (
        match mem k1 t2 with Some i2 -> Leaf (min i1 i2, k1) | None -> Empty)
    | _, Leaf (i2, k2) -> (
        match mem k2 t1 with Some i1 -> Leaf (min i1 i2, k2) | None -> Empty)
    | Branch (p1, m1, l1, r1), Branch (p2, m2, l2, r2) ->
        if m1 == m2 && p1 == p2 then merge (inter l1 l2, inter r1 r2)
        else if unsigned_lt m1 m2 && match_prefix p2 p1 m1 then
          inter (if zero_bit p2 m1 then l1 else r1) t2
        else if unsigned_lt m2 m1 && match_prefix p1 p2 m2 then
          inter t1 (if zero_bit p1 m2 then l2 else r2)
        else Empty

  let rec diff t1 t2 =
    match (t1, t2) with
    | Empty, _ -> Empty
    | _, Empty -> t1
    | Leaf (i1, k1), _ -> (
        match mem k1 t2 with
        | Some i2 -> if i1 > i2 then Leaf (i1 - i2, k1) else Empty
        | None -> t1)
    | _, Leaf (i2, k2) -> remove ~cnt:i2 k2 t1
    | Branch (p1, m1, l1, r1), Branch (p2, m2, l2, r2) ->
        if m1 == m2 && p1 == p2 then merge (diff l1 l2, diff r1 r2)
        else if unsigned_lt m1 m2 && match_prefix p2 p1 m1 then
          if zero_bit p2 m1 then merge (diff l1 t2, r1)
          else merge (l1, diff r1 t2)
        else if unsigned_lt m2 m1 && match_prefix p1 p2 m2 then
          if zero_bit p1 m2 then diff t1 l2 else diff t1 r2
        else t1

  let rec subset t1 t2 =
    match (t1, t2) with
    | Empty, _ -> true
    | _, Empty -> false
    | Leaf (i1, k1), _ -> (
        match mem k1 t2 with Some i2 -> i1 <= i2 | None -> false)
    | Branch _, Leaf _ -> false
    | Branch (p1, m1, l1, r1), Branch (p2, m2, l2, r2) ->
        if m1 == m2 && p1 == p2 then subset l1 l2 && subset r1 r2
        else if unsigned_lt m2 m1 && match_prefix p1 p2 m2 then
          if zero_bit p1 m2 then subset l1 l2 && subset r1 l2
          else subset l1 r2 && subset r1 r2
        else false

  let to_list t =
    let rec to_list_aux acc = function
      | Empty -> acc
      | Leaf (i, k) -> repeat k acc i
      | Branch (_, _, l, r) -> to_list_aux (to_list_aux acc r) l
    in
    to_list_aux [] t

  let of_list l = List.fold_left (fun t elt -> add elt t) Empty l

  let rec equal t1 t2 =
    match (t1, t2) with
    | Empty, Empty -> true
    | Leaf (i1, k1), Leaf (i2, k2) -> k1 == k2 && i1 == i2
    | Branch (p1, m1, l1, r1), Branch (p2, m2, l2, r2) ->
        p1 == p2 && m1 == m2 && equal l1 l2 && equal r1 r2
    | _, _ -> false

  let rec hash = function
    | Empty -> 0
    | Leaf (i, k) -> 0
    | Branch (p, m, l, r) -> 0
end
