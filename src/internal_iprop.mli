open Format
open Ast
open Multiset

(** Definition of hash-consed, internal representation of iprops and multisets
    holding them. *)

type internal_iprop_multiset

type internal_iprop = private
  | IFalse
  | IAtom of string
  | IStar of internal_iprop_multiset
  | IWand of internal_iprop * internal_iprop
  | IBox of internal_iprop

val iFalse : internal_iprop
val iAtom : string -> internal_iprop
val iStar : internal_iprop_multiset -> internal_iprop
val iWand : internal_iprop * internal_iprop -> internal_iprop
val iBox : internal_iprop -> internal_iprop

module M :
  Multiset with type elt = internal_iprop and type t = internal_iprop_multiset

val pp_internal_iprop : formatter -> internal_iprop -> unit

val pp_internal_iprop_multiset :
  ?pp_sep:(formatter -> unit -> unit) ->
  formatter ->
  internal_iprop_multiset ->
  unit

val iprop_to_internal : iprop -> internal_iprop
val iprop_list_to_internal : iprop list -> internal_iprop_multiset
