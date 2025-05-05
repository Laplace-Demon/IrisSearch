open Ast
open Internal

val term_to_internal_term : term -> internal_term
val prop_to_internal_prop : prop -> internal_prop
val iprop_to_internal_iprop : iprop -> internal_iprop
val prop_list_to_internal_prop_set : prop list -> internal_prop_set
val iprop_list_to_internal_iprop_set : iprop list -> internal_iprop_set

val iprop_list_to_simple_internal_iprop_multiset_and_internal_prop_set :
  iprop list -> simple_internal_iprop_multiset * internal_prop_set

type subst_task = internal_term option array

val subst_internal_term : subst_task -> internal_term -> internal_term

val subst_internal_term_array :
  subst_task -> internal_term array -> internal_term array

val subst_internal_prop : subst_task -> internal_prop -> internal_prop

val subst_simple_internal_iprop :
  subst_task -> simple_internal_iprop -> simple_internal_iprop

val subst_internal_iprop : subst_task -> internal_iprop -> internal_iprop

val subst_internal_prop_set :
  subst_task -> internal_prop_set -> internal_prop_set

val subst_internal_iprop_set :
  subst_task -> internal_iprop_set -> internal_iprop_set

val subst_simple_internal_iprop_multiset :
  subst_task -> simple_internal_iprop_multiset -> simple_internal_iprop_multiset

type match_result = internal_term option array
type knowledge = internal_prop_set

open Monads.ListMonad

val internal_term_match :
  knowledge -> match_result -> internal_term -> internal_term -> match_result t

val internal_term_array_match :
  knowledge ->
  match_result ->
  internal_term array ->
  internal_term array ->
  match_result t

val internal_prop_match :
  knowledge -> match_result -> internal_prop -> internal_prop -> match_result t

val internal_iprop_match :
  knowledge ->
  match_result ->
  internal_iprop ->
  internal_iprop ->
  match_result t

val internal_prop_set_match :
  knowledge ->
  match_result ->
  internal_prop_set ->
  internal_prop_set ->
  (match_result * internal_prop_set) t

val simple_internal_iprop_multiset_match :
  knowledge ->
  match_result ->
  simple_internal_iprop_multiset ->
  simple_internal_iprop_multiset ->
  (match_result * simple_internal_iprop_multiset * bool) t

val internal_prop_set_substract_match :
  knowledge ->
  match_result ->
  internal_prop ->
  internal_prop_set ->
  (match_result * internal_prop_set) t

val simple_internal_iprop_multiset_substract_match :
  knowledge ->
  match_result ->
  SimpleIpropMset.elt ->
  Multiplicity.t ->
  simple_internal_iprop_multiset ->
  (match_result * simple_internal_iprop_multiset * bool) t
