open Ast
open Internal
open Validate
open State
open Type

val term_to_internal_term :
  (string, symbol_info) Hashtbl.t -> term -> internal_term

val prop_to_internal_prop :
  (string, symbol_info) Hashtbl.t -> prop -> internal_prop

val iprop_to_internal_iprop :
  (string, symbol_info) Hashtbl.t -> iprop -> internal_iprop

val prop_list_to_internal_prop_set :
  (string, symbol_info) Hashtbl.t -> prop list -> internal_prop_set

val iprop_list_to_simple_internal_iprop_and_disj_list :
  (string, symbol_info) Hashtbl.t ->
  iprop list ->
  simple_internal_iprop * simple_internal_iprop list

val free_vars_internal_term : internal_term -> (VarId.t * itype) list

val free_vars_internal_term_array :
  internal_term array -> (VarId.t * itype) list

val free_vars_internal_prop : internal_prop -> (VarId.t * itype) list

val free_vars_simple_internal_iprop :
  simple_internal_iprop -> (VarId.t * itype) list

val free_vars_internal_iprop : internal_iprop -> (VarId.t * itype) list
val free_vars_internal_prop_set : internal_prop_set -> (VarId.t * itype) list

val free_vars_simple_internal_iprop_multiset :
  simple_internal_iprop_multiset -> (VarId.t * itype) list

type subst_task = internal_term_desc -> internal_term option

val subst_internal_term : subst_task -> internal_term -> internal_term

val subst_internal_term_array :
  subst_task -> internal_term array -> internal_term array

val subst_internal_prop : subst_task -> internal_prop -> internal_prop

val subst_simple_internal_iprop :
  subst_task -> simple_internal_iprop -> simple_internal_iprop

val subst_internal_iprop : subst_task -> internal_iprop -> internal_iprop

val subst_internal_prop_set :
  subst_task -> internal_prop_set -> internal_prop_set

val subst_simple_internal_iprop_multiset :
  subst_task -> simple_internal_iprop_multiset -> simple_internal_iprop_multiset
