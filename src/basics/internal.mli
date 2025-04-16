open Format
open Ast

(** Definition of hash-consed, internal representation of iprops and multisets
    holding them. *)

type internal_prop_set
type internal_iprop_multiset

type internal_term = private
| IVar of string

type internal_prop = private
  | IPersistent of internal_iprop
  | INot of internal_prop
  | IAnd of internal_prop_set
  | IOr of internal_prop * internal_prop
  | IImply of internal_prop * internal_prop
  | IPred of string * internal_term list

and internal_iprop = private
  | IFalse
  | IAtom of string
  | IStar of internal_iprop_multiset
  | IWand of internal_iprop * internal_iprop
  | IPure of internal_prop
  | IHPred of string * internal_term list

val iVar : string -> internal_term
val iPersistent : internal_iprop -> internal_prop
val iNot : internal_prop -> internal_prop
val iAnd : internal_prop_set -> internal_prop
val iOr : internal_prop * internal_prop -> internal_prop
val iImply : internal_prop * internal_prop -> internal_prop
val iPred : string * internal_term list -> internal_prop
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
