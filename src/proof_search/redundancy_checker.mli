open Internal
open State
open Type

val get_state : int -> state

val make_state :
  (string * itype) list
  * SimpleIpropMset.t
  * PropSet.t
  * simple_internal_iprop list list ->
  state * bool

val get_substates : state -> state list
val get_superstates : state -> state list
