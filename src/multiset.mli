open Hashcons

(* hash-consed multiset *)

type 'a elt = 'a hash_consed

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

(** There are two implementations of multiset here, using sorted list and Radix
    tree, respectively. I don't know which (or which combination) is better. *)

module L : Multiset
module R : Multiset
