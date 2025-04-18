open Format
open Ast
open Interned_string

(** Definition of string interning modules. Term variables, predicates, heap
    predicates, atoms are distinguished at the type level. *)

module VarId : InternedString
module PredId : InternedString
module HPredId : InternedString
module AtomId : InternedString

type var_id = VarId.t
type pred_id = PredId.t
type hpred_id = HPredId.t
type atom_id = AtomId.t

(** Definition of hash-consed, internal representation of iprops and multisets
    holding them. *)

type internal_prop_set
type internal_iprop_multiset
type internal_term = IVar of var_id

type internal_prop =
  | IPersistent of internal_iprop
  | INot of internal_prop
  | IAnd of internal_prop_set
  | IOr of internal_prop * internal_prop
  | IImply of internal_prop * internal_prop
  | IPred of pred_id * internal_term list
  | IEq of internal_term * internal_term
  | INeq of internal_term * internal_term

and internal_iprop =
  | IFalse
  | IAtom of atom_id
  | IStar of internal_iprop_multiset
  | IWand of internal_iprop * internal_iprop
  | IPure of internal_prop
  | IHPred of hpred_id * internal_term list

(** Smart internal_term constructors. *)

val iVar : string -> internal_term

(** Smart internal_prop constructors. *)

val iPersistent : internal_iprop -> internal_prop
val iNot : internal_prop -> internal_prop
val iAnd : internal_prop_set -> internal_prop
val iOr : internal_prop * internal_prop -> internal_prop
val iImply : internal_prop * internal_prop -> internal_prop
val iPred : string * internal_term list -> internal_prop
val iEq : internal_term * internal_term -> internal_prop
val iNeq : internal_term * internal_term -> internal_prop

(** Smart internal_iprop constructors. *)

val iFalse : internal_iprop
val iAtom : string -> internal_iprop
val iStar : internal_iprop_multiset -> internal_iprop
val iWand : internal_iprop * internal_iprop -> internal_iprop
val iPure : internal_prop -> internal_iprop
val iHPred : string * internal_term list -> internal_iprop

module PropSet :
  Set.Set with type elt = internal_prop and type t = internal_prop_set

module IpropMset :
  Multiset.Multiset
    with type elt = internal_iprop
     and type t = internal_iprop_multiset

val pp_internal_prop : formatter -> internal_prop -> unit
val pp_internal_iprop : formatter -> internal_iprop -> unit

val pp_internal_prop_set :
  ?pp_sep:(formatter -> unit -> unit) -> formatter -> internal_prop_set -> unit

val pp_internal_iprop_multiset :
  ?pp_sep:(formatter -> unit -> unit) ->
  formatter ->
  internal_iprop_multiset ->
  unit

val prop_to_internal : prop -> internal_prop
val iprop_to_internal : iprop -> internal_iprop
val prop_list_to_internal : prop list -> internal_prop_set
val iprop_list_to_internal : iprop list -> internal_iprop_multiset
