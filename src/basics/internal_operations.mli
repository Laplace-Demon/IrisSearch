open Ast
open Internal

val term_to_internal : term -> internal_term
val prop_to_internal : prop -> internal_prop
val iprop_to_internal : iprop -> internal_iprop
val prop_list_to_internal : prop list -> internal_prop_set
val iprop_list_to_internal : iprop list -> internal_iprop_multiset

type subst_task = (int * internal_term) list

val subst_internal_term : subst_task -> internal_term -> internal_term
val subst_internal_prop : subst_task -> internal_prop -> internal_prop
val subst_internal_iprop : subst_task -> internal_iprop -> internal_iprop

val subst_internal_prop_set :
  subst_task -> internal_prop_set -> internal_prop_set

val subst_internal_iprop_mset :
  subst_task -> internal_iprop_multiset -> internal_iprop_multiset

type match_result = (int * internal_term) list

val internal_term_match :
  int -> internal_term -> internal_term -> match_result option

val internal_prop_match :
  int -> internal_prop -> internal_prop -> match_result option

val internal_iprop_match :
  int -> internal_iprop -> internal_iprop -> match_result option

val internal_prop_match_multiple :
  int -> internal_prop -> internal_prop_set -> match_result list

val internal_iprop_match_multiple :
  int -> internal_iprop -> internal_iprop_multiset -> match_result list
