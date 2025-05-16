open Internal
open State

val init : unit -> unit
val equality_solver : state option -> internal_term -> internal_term -> bool
val consistent_solver : state option -> string option
val implication_solver : state option -> internal_prop_set -> bool
