type t

exception Underflow

val one : t
val inf : t
val is_finite : t -> bool
val is_infinite : t -> bool
val to_int : t -> int
val to_string : t -> string
val add : t -> t -> t
val sub : t -> t -> t option
val mul : t -> t -> t
val div : t -> t -> t
val equal : t -> t -> bool
val compare : t -> t -> int
val min : t -> t -> t
val hash : t -> int
