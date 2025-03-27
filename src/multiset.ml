open Hashcons

(* hash-consed multiset *)

module type Multiset = sig
  type 'a elt
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val cardinal : 'a t -> int

  val mem : 'a elt -> 'a t -> int option

  val add : ?cnt:int -> 'a elt -> 'a t -> 'a t
  val remove : 'a elt -> 'a t -> 'a t
  val union : 'a t -> 'a t -> 'a t
  val inter : 'a t -> 'a t -> 'a t
  val subset : 'a t -> 'a t -> bool

  val equal : 'a t -> 'a t -> bool
  val hash : 'a t -> int
end

(** There are two implementations of multiset here, using sorted list and Radix tree,
    respectively. I don't know which (or which combination) is better. *)

module L : Multiset = struct
  type 'a elt = 'a hash_consed

  type 'a t = 'a elt list

  let empty = []

  let is_empty = function [] -> true | _ -> false

  let rec cardinal = function
    | [] -> 0
    | _ :: l -> 1 + cardinal l
  
  let rec mem e = function
    | [] -> None
    | e' :: l ->
      let c = compare e.tag e'.tag in
      if c < 0 then None
      else if c == 0
      then
        (match mem e l with
        | Some i -> Some (i + 1)
        | None -> Some 1)
      else mem e l

  let add ?(cnt = 1) e l =
    let rec repeat c l =
      if c == 0 then l
      else e :: (repeat (c - 1) l)
    in
    let rec ins = function
      | [] -> repeat cnt []
      | e' :: l' as l ->
        let c = compare e.tag e'.tag in
        if c <= 0 then repeat cnt l
        else e' :: ins l'
    in
    if cnt <= 0 then raise (Invalid_argument "Multiset.add: optional parameter 'c' must be strictly positive.")
    else ins l

  let rec remove e = function
    | [] -> []
    | e' :: l' as l ->
        let c = compare e.tag e'.tag in
        if c == 0 then l'
        else if c < 0 then l
        else e' :: (remove e l')

  let rec union l1 l2 =
    match l1, l2 with
    | [], [] -> []
    | _, [] -> l1
    | [], _ -> l2
    | e1 :: l1', e2 :: l2' ->
      let c = compare e1.tag e2.tag in
      if c == 0
      then e1 :: e2 :: (union l1' l2')
      else if c < 0
      then e1 :: (union l1' l2)
      else e2 :: (union l1 l2')

  let rec inter l1 l2 =
    match l1, l2 with
    | [], _ -> []
    | _, [] -> []
    | e1 :: l1', e2 :: l2' ->
      let c = compare e1.tag e2.tag in
      if c == 0 then e1 :: (inter l1' l2')
      else if c < 0 then inter l1' l2
      else inter l1 l2'

  let rec subset l1 l2 =
    match l1, l2 with
    | [], _ -> true
    | _, [] -> false
    | e1 :: l1', e2 :: l2' ->
      let c = compare e1.tag e2.tag in
      if c == 0 then subset l1' l2'
      else if c < 0 then false
      else subset l1 l2'
  
  let rec equal l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | e1 :: l1', e2 :: l2' ->
      e1.tag == e2.tag && equal l1' l2'
    | _, _ -> false
  
  let hash = function
    | [] -> 0
    | e :: l -> 0
end

module R : Multiset = struct

  (** This multiset module is inspired by the integer set implementation in Jean-Christophe
      Filliâtre's Hashcons library https://opam.ocaml.org/packages/hashcons/. We reimplement
      it here in order to add an integer counter at leaves to allow duplicate elements. The
      implementation is unsafe because it can overflow. *)

  (** Multiset implemented using Radix tree. It has a nice property of being canonical,
      Consequently, two Radix trees have the same elements if and only if they are
      structurally equal. Invariant: the integer on leaf is always strictly positive. *)

  type 'a elt = 'a hash_consed

  type 'a t =
    | Empty
    | Leaf of int * 'a hash_consed
    | Branch of int * int * 'a t * 'a t
  
  let empty = Empty

  let is_empty = function Empty -> true | _ -> false

  let rec cardinal = function
    | Empty -> 0
    | Leaf (i, _) -> i
    | Branch (_, _, t0, t1) -> cardinal t0 + cardinal t1

  let unsigned_lt n m = n >= 0 && (m < 0 || n < m)

  let zero_bit k m = (k land m) == 0

  let lowest_bit x = x land (-x)

  let branching_bit p0 p1 = lowest_bit (p0 lxor p1)

  let mask p m = p land (m - 1)

  let match_prefix k p m = (mask k m) == p

  let rec mem k = function
    | Empty -> None
    | Leaf (i, j) -> if j.tag == k.tag then Some i else None
    | Branch (_, m, l, r) -> mem k (if zero_bit m k.tag then l else r)

  let join (p0, t0, p1, t1) =
    let m = branching_bit p0 p1 in
    if zero_bit p0 m then
      Branch (mask p0 m, m, t0, t1)
    else
      Branch (mask p0 m, m, t1, t0)

  let add ?(cnt = 1) k t =
    let rec ins = function
      | Empty -> Leaf (1, k)
      | Leaf (i, j) as t ->
        if j.tag == k.tag
        then Leaf (i + cnt, j)
        else join (k.tag, Leaf (1, k), j.tag, t)
      | Branch (p, m, t0, t1) as t ->
        if match_prefix k.tag p m
        then
          if zero_bit k.tag m
          then Branch (p, m, ins t0, t1)
          else Branch (p, m, t0, ins t1)
        else
          join (k.tag, Leaf (cnt, k), p, t)
    in
    if cnt <= 0 then raise (Invalid_argument "Multiset.add: optional parameter 'c' must be strictly positive.")
    else ins t

  let branch = function
    | (_, _, Empty, t) -> t
    | (_, _, t, Empty) -> t
    | (p, m, t0, t1) -> Branch (p, m, t0, t1)

  let remove k t =
    let rec rmv = function
      | Empty -> Empty
      | Leaf (i, j) ->
      if j.tag == k.tag
      then
        if i == 1
        then Empty
        else Leaf (i - 1, j)
      else t
      | Branch (p, m, t0, t1) as t ->
        if match_prefix k.tag p m then
          if zero_bit k.tag m then
            branch (p, m, rmv t0, t1)
          else
            branch (p, m, t0, rmv t1)
        else t
      in rmv t

  let rec merge = function
    | Empty, t | t, Empty -> t
    | Leaf (i, j), t | t, Leaf (i, j) -> add ~cnt:i j t
    | (Branch (p1, m1, l1, r1) as s), (Branch (p2, m2, l2, r2) as t) ->
      if m1 == m2 && match_prefix p2 p1 m1 then
        Branch (p1, m1, merge (l1, l2), merge (r1, r2))
      else if unsigned_lt m1 m2 && match_prefix p2 p1 m1 then
        if zero_bit p2 m1 then
          Branch (p1, m1, merge (l1, t), r1)
        else
          Branch (p1, m1, l1, merge (r1, t))
      else if unsigned_lt m2 m1 && match_prefix p1 p2 m2 then
        if zero_bit p1 m2 then
          Branch (p2, m2, merge(s, l2), r2)
        else
          Branch (p2, m2, l2, merge (s, r2))
      else join (p1, s, p2, t)

  let union s t = merge (s, t)

  let rec inter t1 t2 =
    match t1, t2 with
    | Empty, _ -> Empty
    | _, Empty -> Empty
    | Leaf (i1, k1), _ ->
      (match mem k1 t2 with
      | Some i2 -> Leaf (min i1 i2, k1)
      | None -> Empty)
    | _, Leaf (i2, k2) ->
      (match mem k2 t1 with
      | Some i1 -> Leaf (min i1 i2, k2)
      | None -> Empty)
    | Branch (p1, m1, l1, r1), Branch (p2, m2, l2, r2) ->
      if m1 == m2 && p1 == p2 then
        merge (inter l1 l2, inter r1 r2)
      else if unsigned_lt m1 m2 && match_prefix p2 p1 m1 then
        inter (if zero_bit p2 m1 then l1 else r1) t2
      else if unsigned_lt m2 m1 && match_prefix p1 p2 m2 then
        inter t1 (if zero_bit p1 m2 then l2 else r2)
      else Empty

  let rec subset t1 t2 =
    match t1, t2 with
    | Empty, _ -> true
    | _, Empty -> false
    | Leaf (i1, k1), _ ->
      (match mem k1 t2 with
      | Some i2 -> i1 <= i2
      | None -> false)
    | _, Leaf (i2, k2) ->
      (match mem k2 t1 with
      | Some i1 -> i2 <= i1
      | None -> false)
    | Branch (p1, m1, l1, r1), Branch (p2, m2, l2, r2) ->
      if m1 == m2 && p1 == p2 then
        subset l1 l2 && subset r1 r2
      else if unsigned_lt m2 m1 && match_prefix p1 p2 m2 then
        if zero_bit p1 m2 then
          subset l1 l2 && subset r1 l2
        else
          subset l1 r2 && subset r1 r2
      else false
  
  let rec equal t1 t2 =
    match t1, t2 with
    | Empty, Empty -> true
    | Leaf (i1, k1), Leaf (i2, k2) -> k1.tag == k2.tag && i1 == i2
    | Branch (p1, m1, l1, r1), Branch (p2, m2, l2, r2) ->
      p1 == p2 && m1 == m2 && equal l1 l2 && equal r1 r2
    | _, _ -> false

  let rec hash = function
    | Empty -> 0
    | Leaf (i, k) -> 0
    | Branch (p, m, l, r) -> 0
end
