open Format

type iprop =
  | False
  | Atom of string
  | Star of iprop * iprop
  | Wand of iprop * iprop

let rec iprop_eqb (ipr1 : iprop) (ipr2 : iprop) =
  match (ipr1, ipr2) with
  | False, False -> true
  | Atom s1, Atom s2 -> String.equal s1 s2
  | Star (ipr11, ipr12), Star (ipr21, ipr22) ->
      iprop_eqb ipr11 ipr21 && iprop_eqb ipr12 ipr22
  | Wand (ipr11, ipr12), Wand (ipr21, ipr22) ->
      iprop_eqb ipr11 ipr21 && iprop_eqb ipr12 ipr22
  | _, _ -> false

let rec pp_iprop (fmt : formatter) : iprop -> unit = function
  | False -> fprintf fmt "⊥"
  | Atom s -> fprintf fmt "%s" s
  | Star (ipr1, ipr2) -> fprintf fmt "(%a * %a)" pp_iprop ipr1 pp_iprop ipr2
  | Wand (ipr1, ipr2) -> fprintf fmt "(%a -* %a)" pp_iprop ipr1 pp_iprop ipr2

type instance = iprop list

let pp_instance = pp_print_list ~pp_sep:pp_print_newline pp_iprop
