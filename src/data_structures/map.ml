module type Map = sig
  type key
  type 'a t

  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find_opt : key -> 'a t -> 'a option
end

module Make (Ord : Baby.OrderedType) = struct
  module BabyMap = Baby.W.Map.Make (Ord)

  type key = Ord.t
  type 'a t = 'a BabyMap.map

  let empty = BabyMap.empty
  let add = BabyMap.add
  let find_opt = BabyMap.find_opt
end
