open Ast
open Internal

val term_to_internal : term -> internal_term
val prop_to_internal : prop -> internal_prop
val iprop_to_internal : iprop -> internal_iprop
val prop_list_to_internal : prop list -> internal_prop_set
val iprop_list_to_internal : iprop list -> internal_iprop_multiset
