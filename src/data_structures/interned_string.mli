(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

module type InternedString = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val import : string -> t
  val export : t -> string
  val min : t
end

(** [Make()] creates a new abstract type [t] that is isomorphic to [string]. The
    functions [import] and [export] witness this isomorphism. A value of type
    [t] is internally represented as a unique integer code; a mutable table
    keeps track of all strings that have ever been encountered. *)

module Make () : InternedString
