open Format

type stat

val reset : unit -> unit
val record_generated_state : int * int -> unit
val record_visited_state : unit -> unit
val record_depth : int -> unit
val record_duplication : unit -> unit
val record_operation : string -> unit
val pp_stat : ?avg:int -> formatter -> unit
