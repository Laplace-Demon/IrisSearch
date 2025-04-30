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
  let add k e s =
    Statistics.record_operation "Map.add";
    BabyMap.add k e s

  let find_opt k s =
    Statistics.record_operation "Map.find_opt";
    BabyMap.find_opt k s
end
