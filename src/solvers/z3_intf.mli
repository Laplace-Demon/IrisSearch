open Internal

val init : unit -> unit

val equality_solver :
  internal_prop_set -> internal_term -> internal_term -> bool

val consistent_solver : internal_prop_set -> string option
