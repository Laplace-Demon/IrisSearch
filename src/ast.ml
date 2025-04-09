open Format

(** Definition of abstract syntax tree returned by the parser. *)

type itype =
  | Tprop
  | Tiprop

let itype_eqb ity1 ity2 =
  match (ity1, ity2) with
  | Tprop, Tprop -> true
  | Tiprop, Tiprop -> true
  | _, _ -> false

let pp_itype fmt = function
  | Tprop -> fprintf fmt "Prop"
  | Tiprop -> fprintf fmt "iProp"

let pp_typed_id fmt (str, ity) = fprintf fmt "%s : %a" str pp_itype ity

type prop =
  | Persistent of iprop
  | Not of prop
  | And of prop * prop
  | Or of prop * prop
  | Imply of prop * prop

and iprop =
  | False
  | Atom of string
  | Star of iprop * iprop
  | Wand of iprop * iprop
  | Box of iprop
  | Pure of prop

let uncurry_wand (ipr1, ipr2) =
  match ipr2 with
  | Wand (ipr21, ipr22) -> Wand (Star (ipr1, ipr21), ipr22)
  | _ -> Wand (ipr1, ipr2)

let rec prop_eqb pr1 pr2 =
  match pr1, pr2 with
  | Persistent ipr1, Persistent ipr2 -> iprop_eqb ipr1 ipr2
  | Not pr1, Not pr2 -> prop_eqb pr1 pr2
  | And (pr11, pr12), And (pr21, pr22) | Or (pr11, pr12), Or (pr21, pr22) | Imply (pr11, pr12), Imply (pr21, pr22) ->
    prop_eqb pr11 pr21 && prop_eqb pr12 pr22
  | _, _ -> false

and iprop_eqb ipr1 ipr2 =
  match (ipr1, ipr2) with
  | False, False -> true
  | Atom str1, Atom str2 -> String.equal str1 str2
  | Star (ipr11, ipr12), Star (ipr21, ipr22) | Wand (ipr11, ipr12), Wand (ipr21, ipr22) ->
      iprop_eqb ipr11 ipr21 && iprop_eqb ipr12 ipr22
  | Box ipr1, Box ipr2 -> iprop_eqb ipr1 ipr2
  | Pure pr1, Pure pr2 -> prop_eqb pr1 pr2
  | _, _ -> false

let rec pp_prop fmt = function
  | Persistent ipr -> fprintf fmt "Persistent %a" pp_iprop ipr
  | Not pr -> fprintf fmt "¬ %a" pp_prop pr
  | And (pr1, pr2) -> fprintf fmt "(%a ∧ %a)" pp_prop pr1 pp_prop pr2
  | Or (pr1, pr2) -> fprintf fmt "(%a ∨ %a)" pp_prop pr1 pp_prop pr2
  | Imply (pr1, pr2) -> fprintf fmt "(%a → %a)" pp_prop pr1 pp_prop pr2

and pp_iprop fmt = function
  | False -> fprintf fmt "⊥"
  | Atom str -> fprintf fmt "%s" str
  | Star (ipr1, ipr2) -> fprintf fmt "(%a * %a)" pp_iprop ipr1 pp_iprop ipr2
  | Wand (ipr1, ipr2) -> fprintf fmt "(%a -* %a)" pp_iprop ipr1 pp_iprop ipr2
  | Box ipr -> fprintf fmt "□ %a" pp_iprop ipr
  | Pure pr -> fprintf fmt "⌜%a⌝" pp_prop pr

(** The problem instance is represented as a record holding:
    - type definition of constants
    - (persistent and pure) laws
    - initial atoms *)

type instance = {
  decl_consts : (string * itype) list;
  decl_laws : (prop, iprop) Either.t list;
  decl_init : iprop list;
}

let pp_instance fmt { decl_consts; decl_laws; decl_init } =
  fprintf fmt "@[<v>consts@.%a@.laws@.%a@.init@.%a@.@]"
    (pp_print_list pp_typed_id)
    decl_consts (pp_print_list (pp_print_either ~left:pp_prop ~right:pp_iprop)) decl_laws (pp_print_list pp_iprop)
    decl_init

exception DuplicateDeclarationError of string

exception TypeError of string * itype * itype

exception NoDeclarationError of string

let validate { decl_consts; decl_laws; decl_init } =
  let symbol_table = Hashtbl.create 17 in
  let rec check_type = function
    | False -> ()
    | Atom str ->
      (match Hashtbl.find_opt symbol_table str with
      | Some ity ->
        if not (itype_eqb ity Tiprop)
        then raise (TypeError (str, Tiprop, ity))
      | None ->
        raise (NoDeclarationError str)
      )
    | Star (ipr1, ipr2) -> check_type ipr1; check_type ipr2
    | Wand (ipr1, ipr2) -> check_type ipr1; check_type ipr2
    | Box ipr -> check_type ipr
  in
  let _ =
    (** Build symbol table. *)
    List.iter (fun (str, ity) ->
    if Hashtbl.mem symbol_table str
    then raise (DuplicateDeclarationError str)
    else Hashtbl.add symbol_table str ity
    ) decl_consts
  in
  let prop_list, persistent_laws =
    (** Separate prop and iprop from decl_laws. *)
    List.partition_map (fun x -> x) decl_laws
  in
  let iprop_list =
    persistent_laws @ decl_init
  in
  let _ =
    (** Check type. *)
    List.iter check_type iprop_list
  in
  symbol_table, prop_list, iprop_list
  
