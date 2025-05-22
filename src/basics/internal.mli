open Format
open Interned_string
open Type

(** Definition of string interning modules. Term variables, predicates, heap
    predicates, atoms are distinguished at the type level. *)

module ConstrId : InternedString
module VarId : InternedString
module FuncId : InternedString
module PredId : InternedString
module HPredId : InternedString

type constr_id = ConstrId.t
type var_id = VarId.t
type func_id = FuncId.t
type pred_id = PredId.t
type hpred_id = HPredId.t

val false_id : HPredId.t

(** Definition of internal representations and data structures storing them. *)

type internal_prop_set
type simple_internal_iprop_multiset
type binder_info = { shift : int; typed_str_list : (string * itype) list }

type internal_term = { desc : internal_term_desc; ity : itype }

and internal_term_desc =
  | IVar of var_id
  | IBVar of int
  | IConstr of constr_id * internal_term array
  | IFunc of func_id * internal_term array

type internal_prop =
  | IPersistent of internal_iprop
  | INot of internal_prop
  | IAnd of internal_prop_set
  | IOr of internal_prop * internal_prop
  | IImply of internal_prop * internal_prop
  | IPred of pred_id * internal_term array
  | IForall of binder_info * internal_prop
  | IExists of binder_info * internal_prop
  | IEq of internal_term * internal_term
  | INeq of internal_term * internal_term

and simple_internal_iprop = simple_internal_iprop_multiset * internal_prop_set

and internal_iprop =
  | ISimple of simple_internal_iprop
  | IWand of internal_iprop * internal_iprop
  | IHForall of binder_info * internal_iprop
  | IHExists of binder_info * internal_iprop

val compare_internal_term : internal_term -> internal_term -> int

val compare_internal_term_array :
  internal_term array -> internal_term array -> int

val compare_internal_prop : internal_prop -> internal_prop -> int

val compare_simple_internal_iprop :
  simple_internal_iprop -> simple_internal_iprop -> int

val compare_internal_iprop : internal_iprop -> internal_iprop -> int

(** Smart internal_term constructors. *)

val iVar : var_id * itype -> internal_term
val iVar_str : string * itype -> internal_term
val iBVar : int * itype -> internal_term
val iConstr : constr_id * internal_term array * itype -> internal_term
val iConstr_str : string * internal_term array * itype -> internal_term
val iFunc : func_id * internal_term array * itype -> internal_term
val iFunc_str : string * internal_term array * itype -> internal_term

(** Smart internal_prop constructors. *)

val iPersistent : internal_iprop -> internal_prop
val iNot : internal_prop -> internal_prop
val iAnd : internal_prop_set -> internal_prop
val iOr : internal_prop * internal_prop -> internal_prop
val iImply : internal_prop * internal_prop -> internal_prop
val iPred : pred_id * internal_term array -> internal_prop
val iPred_str : string * internal_term array -> internal_prop
val iForall : binder_info * internal_prop -> internal_prop
val iForall_raw : (string * itype) list * internal_prop -> internal_prop
val iExists : binder_info * internal_prop -> internal_prop
val iExists_raw : (string * itype) list * internal_prop -> internal_prop
val iEq : internal_term * internal_term -> internal_prop
val iNeq : internal_term * internal_term -> internal_prop

(** Smart internal_iprop constructors. *)

val iSimple :
  simple_internal_iprop_multiset * internal_prop_set -> internal_iprop

val iWand : internal_iprop * internal_iprop -> internal_iprop
val iHForall : binder_info * internal_iprop -> internal_iprop
val iHForall_raw : (string * itype) list * internal_iprop -> internal_iprop
val iHExists : binder_info * internal_iprop -> internal_iprop
val iHExists_raw : (string * itype) list * internal_iprop -> internal_iprop

module PropSet :
  Set.Set with type elt = internal_prop and type t = internal_prop_set

module SimpleIpropMset :
  Multiset.Multiset2
    with type elt1 = hpred_id
     and type elt2 = internal_term array
     and type t = simple_internal_iprop_multiset

val pp_internal_term : formatter -> internal_term -> unit
val pp_internal_term_env : string list -> formatter -> internal_term -> unit
val pp_internal_prop : formatter -> internal_prop -> unit
val pp_internal_prop_env : string list -> formatter -> internal_prop -> unit
val pp_simple_internal_iprop : formatter -> simple_internal_iprop -> unit

val pp_simple_internal_iprop_env :
  string list -> formatter -> simple_internal_iprop -> unit

val pp_internal_iprop : formatter -> internal_iprop -> unit
val pp_internal_iprop_env : string list -> formatter -> internal_iprop -> unit

val pp_internal_prop_set :
  ?pp_sep:(formatter -> unit -> unit) -> formatter -> internal_prop_set -> unit

val pp_internal_prop_set_env :
  string list ->
  ?pp_sep:(formatter -> unit -> unit) ->
  formatter ->
  internal_prop_set ->
  unit

val pp_simple_internal_iprop_multiset :
  ?pp_sep:(formatter -> unit -> unit) ->
  formatter ->
  simple_internal_iprop_multiset ->
  unit

val pp_simple_internal_iprop_multiset_env :
  string list ->
  ?pp_sep:(formatter -> unit -> unit) ->
  formatter ->
  simple_internal_iprop_multiset ->
  unit
