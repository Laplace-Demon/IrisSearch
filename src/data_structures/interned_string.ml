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
end

module Make () : InternedString = struct
  (* Create a new service for assigning unique integer codes to strings. *)

  type t = int

  let equal (x : t) (y : t) = x = y
  let compare (x : t) (y : t) = x - y
  let hash (x : t) = x

  let new_encode_decode capacity =
    (* Set up a a hash table, mapping strings to unique integers. *)
    let module H = Hashtbl.Make (struct
      type t = string

      let equal = ( = )
      let hash = Hashtbl.hash
    end) in
    let table = H.create capacity in
    (* Set up a resizable array, mapping integers to strings. *)
    let text = MenhirLib.InfiniteArray.make "" in
    (* This counts the calls to [encode]. *)
    let c = ref 0 in
    (* A string is mapped to a unique integer, as follows. *)
    let encode (s : string) : int =
      c := !c + 1;
      try H.find table s
      with Not_found ->
        (* The number of elements in the hash table is the next available
           unique integer code. *)
        let i = H.length table in
        H.add table s i;
        MenhirLib.InfiniteArray.set text i s;
        i
    (* An integer code can be mapped back to a string, as follows. *)
    and decode (i : int) : string = MenhirLib.InfiniteArray.get text i
    and verbose () =
      Printf.fprintf stderr "%d calls to intern; %d unique strings.\n%!" !c
        (H.length table)
    in
    (encode, decode, verbose)

  let import, export, _verbose = new_encode_decode 2048
end
