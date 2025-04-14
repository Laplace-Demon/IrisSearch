val reset : unit -> unit

val generate : ?base:string -> unit -> string

val repeat : (unit -> 'a) -> int -> 'a list
