open Format

(** Definition of abstract syntax tree of iprops returned by the parser. *)

type iprop =
  | False
  | Atom of string
  | Star of iprop * iprop
  | Wand of iprop * iprop
  | Box of iprop

let rec iprop_eqb (ipr1 : iprop) (ipr2 : iprop) =
  match (ipr1, ipr2) with
  | False, False -> true
  | Atom s1, Atom s2 -> String.equal s1 s2
  | Star (ipr11, ipr12), Star (ipr21, ipr22) ->
      iprop_eqb ipr11 ipr21 && iprop_eqb ipr12 ipr22
  | Wand (ipr11, ipr12), Wand (ipr21, ipr22) ->
      iprop_eqb ipr11 ipr21 && iprop_eqb ipr12 ipr22
  | Box ipr1, Box ipr2 -> iprop_eqb ipr1 ipr2
  | _, _ -> false

let rec pp_iprop (fmt : formatter) : iprop -> unit = function
  | False -> fprintf fmt "⊥"
  | Atom s -> fprintf fmt "%s" s
  | Star (ipr1, ipr2) -> fprintf fmt "(%a * %a)" pp_iprop ipr1 pp_iprop ipr2
  | Wand (ipr1, ipr2) -> fprintf fmt "(%a -* %a)" pp_iprop ipr1 pp_iprop ipr2
  | Box ipr -> fprintf fmt "□ %a" pp_iprop ipr

(** The problem instance is represented as a list of iprops. *)

type instance = iprop list

let pp_instance = pp_print_list ~pp_sep:pp_print_newline pp_iprop

let valid =
  let rec no_wand = function
    | False | Atom _ -> true
    | Star (ipr1, ipr2) -> no_wand ipr1 && no_wand ipr2
    | Wand _ -> false
    | Box ipr -> no_wand ipr
  in
  let rec no_box = function
    | False | Atom _ -> true
    | Star (ipr1, ipr2) | Wand (ipr1, ipr2) -> no_box ipr1 && no_box ipr2
    | Box _ -> false
  in
  List.for_all (function
    | Box (Wand (ipr1, ipr2)) -> no_wand ipr1 && no_wand ipr2
    | Wand _ | Box _ -> false
    | _ -> true)
