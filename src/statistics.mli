open Format

type stat

val record_state : int -> unit

val record_depth : int -> unit

val record_duplication : unit -> unit

val pp_stat : formatter -> unit
