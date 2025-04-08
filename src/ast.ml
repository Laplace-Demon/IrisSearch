open Format

(** Definition of abstract syntax tree returned by the parser. *)

type itype = Tiprop

let itype_eqb ity1 ity2 =
  match (ity1, ity2) with Tiprop, Tiprop -> true | _, _ -> false

let pp_itype fmt = function Tiprop -> fprintf fmt "iProp"
let pp_typed_id fmt (str, ity) = fprintf fmt "%s : %a" str pp_itype ity

type iprop =
  | False
  | Atom of string
  | Star of iprop * iprop
  | Wand of iprop * iprop
  | Box of iprop

let uncurry_wand (ipr1, ipr2) =
  match ipr2 with
  | Wand (ipr21, ipr22) -> Wand (Star (ipr1, ipr21), ipr22)
  | _ -> Wand (ipr1, ipr2)

let rec iprop_eqb ipr1 ipr2 =
  match (ipr1, ipr2) with
  | False, False -> true
  | Atom str1, Atom str2 -> String.equal str1 str2
  | Star (ipr11, ipr12), Star (ipr21, ipr22) ->
      iprop_eqb ipr11 ipr21 && iprop_eqb ipr12 ipr22
  | Wand (ipr11, ipr12), Wand (ipr21, ipr22) ->
      iprop_eqb ipr11 ipr21 && iprop_eqb ipr12 ipr22
  | Box ipr1, Box ipr2 -> iprop_eqb ipr1 ipr2
  | _, _ -> false

let rec pp_iprop fmt = function
  | False -> fprintf fmt "⊥"
  | Atom str -> fprintf fmt "%s" str
  | Star (ipr1, ipr2) -> fprintf fmt "(%a * %a)" pp_iprop ipr1 pp_iprop ipr2
  | Wand (ipr1, ipr2) -> fprintf fmt "(%a -* %a)" pp_iprop ipr1 pp_iprop ipr2
  | Box ipr -> fprintf fmt "□ %a" pp_iprop ipr

(** The problem instance is represented as a record holding:
    - type definition of constants
    - (persistent) laws
    - initial atoms *)

type instance = {
  decl_consts : (string * itype) list;
  decl_laws : iprop list;
  decl_init : iprop list;
}

let pp_instance fmt { decl_consts; decl_laws; decl_init } =
  fprintf fmt "@[<v>consts@.%a@.laws@.%a@.init@.%a@.@]"
    (pp_print_list pp_typed_id)
    decl_consts (pp_print_list pp_iprop) decl_laws (pp_print_list pp_iprop)
    decl_init

exception DuplicateDeclarationError of string

exception TypeError of string

let validate { decl_consts; decl_laws; decl_init } =
  let symbol_table = Hashtbl.create 17 in
  let rec validate_aux = function
    | False -> ()
    | Atom str -> if not (Hashtbl.mem symbol_table str) then raise (TypeError str)
    | Star (ipr1, ipr2) -> validate_aux ipr1; validate_aux ipr2
    | Wand (ipr1, ipr2) -> validate_aux ipr1; validate_aux ipr2
    | Box ipr -> validate_aux ipr
  in
  List.iter (fun (str, ity) ->
    if Hashtbl.mem symbol_table str
    then raise (DuplicateDeclarationError str)
    else Hashtbl.add symbol_table str ity
    ) decl_consts;
    List.iter validate_aux
    (decl_laws @ decl_init);
    symbol_table
  
