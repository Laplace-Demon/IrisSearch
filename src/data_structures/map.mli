module type Map = sig
  type key
  type 'a t

  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find_opt : key -> 'a t -> 'a option
end

module Make (Ord : Baby.OrderedType) : Map with type key = Ord.t
