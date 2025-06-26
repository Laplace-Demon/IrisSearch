open Internal
open State

type match_result = internal_term option array

val match_result_complete : match_result -> bool

open Monads.ListMonad

val internal_term_match :
  state option ->
  match_result ->
  internal_term ->
  internal_term ->
  match_result t

val internal_term_array_match :
  state option ->
  match_result ->
  internal_term array ->
  internal_term array ->
  match_result t

val internal_prop_match :
  state option ->
  match_result ->
  internal_prop ->
  internal_prop ->
  match_result t

val internal_iprop_match :
  state option ->
  match_result ->
  internal_iprop ->
  internal_iprop ->
  match_result t

val internal_prop_set_match :
  state option ->
  match_result ->
  internal_prop_set ->
  internal_prop_set ->
  (match_result * internal_prop_set) t

val simple_internal_iprop_multiset_match :
  state option ->
  match_result ->
  simple_internal_iprop_multiset ->
  simple_internal_iprop_multiset ->
  (match_result * simple_internal_iprop_multiset * bool) t

val internal_prop_set_substract_match :
  state option ->
  match_result ->
  internal_prop ->
  internal_prop_set ->
  (match_result * internal_prop_set) t

val simple_internal_iprop_multiset_substract_match :
  state option ->
  match_result ->
  SimpleIpropMset.elt ->
  bool ->
  simple_internal_iprop_multiset ->
  (match_result * simple_internal_iprop_multiset * bool) t
