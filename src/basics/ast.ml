open Format

(** Definition of abstract syntax tree returned by the parser. *)

type itype = Tprop | Tiprop | Tcustom of string | Tarrow of itype list * itype

let rec itype_eqb ity1 ity2 =
  match (ity1, ity2) with
  | Tprop, Tprop -> true
  | Tiprop, Tiprop -> true
  | Tcustom str1, Tcustom str2 -> String.equal str1 str2
  | Tarrow (ity_list1, ity1), Tarrow (ity_list2, ity2) ->
      List.equal itype_eqb ity_list1 ity_list2 && itype_eqb ity1 ity2
  | _, _ -> false

let rec pp_itype fmt = function
  | Tprop -> fprintf fmt "Prop"
  | Tiprop -> fprintf fmt "iProp"
  | Tcustom str -> fprintf fmt "%s" str
  | Tarrow (ity_list, ity) ->
      fprintf fmt "%a -> %a"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " * ") pp_itype)
        ity_list pp_itype ity

let pp_typed_ident fmt (str, ity) = fprintf fmt "%s : %a" str pp_itype ity

type term = Var of string

let pp_term fmt = function Var str -> fprintf fmt "%s" str

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

let uncurry_imply (pr1, pr2) =
  match pr2 with
  | Imply (pr21, pr22) -> Imply (And (pr1, pr21), pr22)
  | _ -> Imply (pr1, pr2)

let uncurry_wand (ipr1, ipr2) =
  match ipr2 with
  | Wand (ipr21, ipr22) -> Wand (Star (ipr1, ipr21), ipr22)
  | _ -> Wand (ipr1, ipr2)

let rec prop_eqb pr1 pr2 =
  match (pr1, pr2) with
  | Persistent ipr1, Persistent ipr2 -> iprop_eqb ipr1 ipr2
  | Not pr1, Not pr2 -> prop_eqb pr1 pr2
  | And (pr11, pr12), And (pr21, pr22)
  | Or (pr11, pr12), Or (pr21, pr22)
  | Imply (pr11, pr12), Imply (pr21, pr22) ->
      prop_eqb pr11 pr21 && prop_eqb pr12 pr22
  | _, _ -> false

and iprop_eqb ipr1 ipr2 =
  match (ipr1, ipr2) with
  | False, False -> true
  | Atom str1, Atom str2 -> String.equal str1 str2
  | Star (ipr11, ipr12), Star (ipr21, ipr22)
  | Wand (ipr11, ipr12), Wand (ipr21, ipr22) ->
      iprop_eqb ipr11 ipr21 && iprop_eqb ipr12 ipr22
  | Box ipr1, Box ipr2 -> iprop_eqb ipr1 ipr2
  | Pure pr1, Pure pr2 -> prop_eqb pr1 pr2
  | _, _ -> false

let rec pp_prop fmt = function
  | Persistent ipr -> fprintf fmt "Persistent %a" pp_iprop ipr
  | Not pr -> fprintf fmt "¬ %a" pp_prop pr
  | And (pr1, pr2) -> fprintf fmt "%a ∧ %a" pp_prop pr1 pp_prop pr2
  | Or (pr1, pr2) -> fprintf fmt "%a ∨ %a" pp_prop pr1 pp_prop pr2
  | Imply (pr1, pr2) -> fprintf fmt "%a → %a" pp_prop pr1 pp_prop pr2

and pp_iprop fmt = function
  | False -> fprintf fmt "⊥"
  | Atom str -> fprintf fmt "%s" str
  | Star (ipr1, ipr2) -> fprintf fmt "%a * %a" pp_iprop ipr1 pp_iprop ipr2
  | Wand (ipr1, ipr2) -> fprintf fmt "(%a -* %a)" pp_iprop ipr1 pp_iprop ipr2
  | Box ipr -> fprintf fmt "□ %a" pp_iprop ipr
  | Pure pr -> fprintf fmt "⌜%a⌝" pp_prop pr

let rec prop_subst_var src dest = function
  | Persistent ipr -> Persistent (iprop_subst_var src dest ipr)
  | Not pr -> Not (prop_subst_var src dest pr)
  | And (pr1, pr2) ->
      And (prop_subst_var src dest pr1, prop_subst_var src dest pr2)
  | Or (pr1, pr2) ->
      Or (prop_subst_var src dest pr1, prop_subst_var src dest pr2)
  | Imply (pr1, pr2) ->
      Imply (prop_subst_var src dest pr1, prop_subst_var src dest pr2)

and iprop_subst_var src dest = function
  | False -> False
  | Atom str -> if String.equal str src then dest else Atom str
  | Star (ipr1, ipr2) ->
      Star (iprop_subst_var src dest ipr1, iprop_subst_var src dest ipr2)
  | Wand (ipr1, ipr2) ->
      Wand (iprop_subst_var src dest ipr1, iprop_subst_var src dest ipr2)
  | Box ipr -> Box (iprop_subst_var src dest ipr)
  | Pure pr -> Pure (prop_subst_var src dest pr)

let rec prop_subst_positive_var src dest = function
  | Persistent ipr -> Persistent (iprop_subst_positive_var src dest ipr)
  | Not pr -> Not (prop_subst_positive_var src dest pr)
  | And (pr1, pr2) ->
      And
        ( prop_subst_positive_var src dest pr1,
          prop_subst_positive_var src dest pr2 )
  | Or (pr1, pr2) ->
      Or
        ( prop_subst_positive_var src dest pr1,
          prop_subst_positive_var src dest pr2 )
  | Imply (pr1, pr2) ->
      Imply
        ( prop_subst_positive_var src dest pr1,
          prop_subst_positive_var src dest pr2 )

and prop_subst_negative_var src dest = function
  | Persistent ipr -> Persistent (iprop_subst_negative_var src dest ipr)
  | Not pr -> Not (prop_subst_negative_var src dest pr)
  | And (pr1, pr2) ->
      And
        ( prop_subst_negative_var src dest pr1,
          prop_subst_negative_var src dest pr2 )
  | Or (pr1, pr2) ->
      Or
        ( prop_subst_negative_var src dest pr1,
          prop_subst_negative_var src dest pr2 )
  | Imply (pr1, pr2) ->
      Imply
        ( prop_subst_negative_var src dest pr1,
          prop_subst_negative_var src dest pr2 )

and iprop_subst_positive_var src dest = function
  | False -> False
  | Atom str -> if String.equal str src then dest else Atom str
  | Star (ipr1, ipr2) ->
      Star
        ( iprop_subst_positive_var src dest ipr1,
          iprop_subst_positive_var src dest ipr2 )
  | Wand (ipr1, ipr2) ->
      Wand
        ( iprop_subst_negative_var src dest ipr1,
          iprop_subst_positive_var src dest ipr2 )
  | Box ipr -> Box (iprop_subst_positive_var src dest ipr)
  | Pure pr -> Pure (prop_subst_positive_var src dest pr)

and iprop_subst_negative_var src dest = function
  | False -> False
  | Atom str -> Atom str
  | Star (ipr1, ipr2) ->
      Star
        ( iprop_subst_negative_var src dest ipr1,
          iprop_subst_negative_var src dest ipr2 )
  | Wand (ipr1, ipr2) ->
      Wand
        ( iprop_subst_positive_var src dest ipr1,
          iprop_subst_negative_var src dest ipr2 )
  | Box ipr -> Box (iprop_subst_negative_var src dest ipr)
  | Pure pr -> Pure (prop_subst_negative_var src dest pr)

(** The problem instance is represented as a record holding:
    - type definition of constants
    - (persistent and pure) laws
    - initial atoms *)

type instance = {
  decl_types : string list;
  decl_preds : (string * itype) list;
  decl_consts : (string * itype) list;
  decl_laws : iprop list;
  decl_init : iprop list;
}

let pp_instance fmt
    { decl_types; decl_preds; decl_consts; decl_laws; decl_init } =
  fprintf fmt
    "@[<v 4>types@,\
     %a@]@.@[<v 4>preds@,\
     %a@]@.@[<v 4>consts@,\
     %a@]@.@[<v 4>laws@,\
     %a@]@.@[<v 4>init@,\
     %a@]@."
    (pp_print_list pp_print_string)
    decl_types
    (pp_print_list (fun fmt (str, ity) ->
         fprintf fmt "%s : %a" str pp_itype ity))
    decl_preds
    (pp_print_list pp_typed_ident)
    decl_consts (pp_print_list pp_iprop) decl_laws (pp_print_list pp_iprop)
    decl_init

let instance_subst_var src dest
    { decl_types; decl_preds; decl_consts; decl_laws; decl_init } =
  {
    decl_types;
    decl_preds;
    decl_consts;
    decl_laws = List.map (fun ipr -> iprop_subst_var src dest ipr) decl_laws;
    decl_init = List.map (fun ipr -> iprop_subst_var src dest ipr) decl_init;
  }

let instance_subst_positive_var src dest
    { decl_types; decl_preds; decl_consts; decl_laws; decl_init } =
  {
    decl_types;
    decl_preds;
    decl_consts;
    decl_laws =
      List.map (fun ipr -> iprop_subst_positive_var src dest ipr) decl_laws;
    decl_init =
      List.map (fun ipr -> iprop_subst_positive_var src dest ipr) decl_init;
  }

let instance_subst_negative_var src dest
    { decl_types; decl_preds; decl_consts; decl_laws; decl_init } =
  {
    decl_types;
    decl_preds;
    decl_consts;
    decl_laws =
      List.map (fun ipr -> iprop_subst_negative_var src dest ipr) decl_laws;
    decl_init =
      List.map (fun ipr -> iprop_subst_negative_var src dest ipr) decl_init;
  }

let replace_persistent_transformation
    { decl_types; decl_preds; decl_consts; decl_laws; decl_init } =
  let facts, laws =
    List.partition_map
      (function
        | Pure pr -> Either.Left pr
        | Box _ as ipr -> Either.Right ipr
        | _ -> assert false)
      decl_laws
  in
  let ins =
    { decl_types; decl_preds; decl_consts; decl_laws = laws; decl_init }
  in
  List.fold_left
    (fun ins pr ->
      match pr with
      | Persistent (Atom str) ->
          instance_subst_positive_var str (Box (Atom str)) ins
      | _ -> ins)
    ins facts

(** Validation. *)

exception IllegalPredicateDeclarationError of string
exception DuplicateTypeDeclarationError of string
exception DuplicatePredicateDeclarationError of string
exception DuplicateConstDeclarationError of string
exception MissingTypeDeclarationError of string
exception MissingPredicateDeclarationError of string
exception MissingConstDeclarationError of string
exception TypeError of string * itype * itype

let validate symbol_table { decl_types; decl_preds; decl_consts; decl_laws; decl_init } =
  let type_table = Hashtbl.create 17 in
  let rec check_iprop = function
    | False -> ()
    | Atom str -> (
        match Hashtbl.find_opt symbol_table str with
        | Some ity ->
            if not (itype_eqb ity Tiprop) then
              raise (TypeError (str, Tiprop, ity))
        | None -> raise (MissingConstDeclarationError str))
    | Star (ipr1, ipr2) ->
        check_iprop ipr1;
        check_iprop ipr2
    | Wand (ipr1, ipr2) ->
        check_iprop ipr1;
        check_iprop ipr2
    | Box ipr -> check_iprop ipr
    | Pure pr -> check_prop pr
  and check_prop = function
    | Persistent ipr -> check_iprop ipr
    | Not pr -> check_prop pr
    | And (pr1, pr2) ->
        check_prop pr1;
        check_prop pr2
    | Or (pr1, pr2) ->
        check_prop pr1;
        check_prop pr2
    | Imply (pr1, pr2) ->
        check_prop pr1;
        check_prop pr2
  in
  let () =
    (* Check type declarations. *)
    List.iter
      (fun ty_str ->
        match ty_str with
        | "Prop" | "iProp" -> raise (DuplicateTypeDeclarationError ty_str)
        | _ ->
            if Hashtbl.mem type_table ty_str then
              raise (DuplicateTypeDeclarationError ty_str)
            else Hashtbl.add type_table ty_str ())
      decl_types
  in
  let () =
    (* Check predicate declarations. *)
    List.iter
      (fun (str, ity) ->
        let param_ity, res_ity =
          match ity with
          | Tarrow (param_ity, res_ity) -> (param_ity, res_ity)
          | _ -> assert false
        in
        let () =
          (* Check if the result type of the predicate is either iProp or Prop. *)
          match res_ity with
          | Tiprop | Tprop -> ()
          | _ -> raise (IllegalPredicateDeclarationError str)
        in
        let () =
          (* Check if the parameter types of the predicate is declared, first-order, and different from iProp and Prop *)
          List.iter
            (function
              | Tiprop | Tprop | Tarrow _ ->
                  raise (IllegalPredicateDeclarationError str)
              | Tcustom ty_str ->
                  if not (Hashtbl.mem type_table ty_str) then
                    raise (MissingTypeDeclarationError ty_str))
            param_ity
        in
        (* Check if the predicate is already declared. *)
        if Hashtbl.mem symbol_table str then
          raise (DuplicatePredicateDeclarationError str)
        else
          (* Build the symbol table. *)
          Hashtbl.add symbol_table str ity)
      decl_preds
  in
  let () =
    (* Check constant declarations. *)
    List.iter
      (fun (str, ity) ->
        let () =
          (* Check if the type of the constant is declared. *)
          match ity with
          | Tcustom ty_str ->
              if not (Hashtbl.mem type_table ty_str) then
                raise (MissingTypeDeclarationError ty_str)
          | _ -> ()
        in
        (* Check if the constant is already declared. *)
        if Hashtbl.mem symbol_table str then
          raise (DuplicateConstDeclarationError str)
        else
          (* Build the symbol table. *)
          Hashtbl.add symbol_table str ity)
      decl_consts
  in
  let () =
    (* Check type. *)
    List.iter check_iprop decl_laws;
    List.iter check_iprop decl_init
  in
  let facts, laws =
    (* Separate pure facts and persistent laws from decl_laws. *)
    List.partition_map
      (function
        | Pure pr -> Either.Left pr
        | Box _ as ipr -> Either.Right ipr
        | _ -> assert false)
      decl_laws
  in
  (facts, laws, decl_init)
