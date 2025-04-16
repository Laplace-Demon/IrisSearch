open Format
open Ast

exception IllegalPredicateDeclarationError of string
exception DuplicateTypeDeclarationError of string
exception DuplicatePredicateDeclarationError of string
exception DuplicateConstDeclarationError of string
exception MissingTypeDeclarationError of string
exception MissingPredicateDeclarationError of string
exception MissingConstDeclarationError of string
exception TypeError of string * itype * itype

let check_term symbol_table = function
  | Var str ->
    match Hashtbl.find_opt symbol_table str with
    | Some ity -> ity
    | None -> raise (MissingConstDeclarationError str)

let rec check_iprop symbol_table = function
  | False -> ()
  | Atom str -> (
      match Hashtbl.find_opt symbol_table str with
      | Some ity ->
          if not (itype_eqb ity Tiprop) then
            raise (TypeError (str, Tiprop, ity))
      | None -> raise (MissingConstDeclarationError str))
  | Star (ipr1, ipr2) ->
      check_iprop symbol_table ipr1;
      check_iprop symbol_table ipr2
  | Wand (ipr1, ipr2) ->
      check_iprop symbol_table ipr1;
      check_iprop symbol_table ipr2
  | Box ipr -> check_iprop symbol_table ipr
  | Pure pr -> check_prop symbol_table pr
  | HPred (str, param_list) ->
    match Hashtbl.find_opt symbol_table str with
    | Some ity ->
      (match ity with
      | Tarrow (param_ity_list, Tiprop) ->
        List.iter2 (fun param param_ity ->
          let arg_ity = check_term symbol_table param in
          if not (itype_eqb arg_ity param_ity)
          then raise (TypeError (asprintf "%a" pp_term param, param_ity, arg_ity))
          ) param_list param_ity_list
      | _ -> raise (TypeError (str, Tarrow ([], Tiprop), ity)))
    | None -> raise (MissingPredicateDeclarationError str)

and check_prop symbol_table = function
  | Persistent ipr -> check_iprop symbol_table ipr
  | Not pr -> check_prop symbol_table pr
  | And (pr1, pr2)
  | Or (pr1, pr2)
  | Imply (pr1, pr2) ->
      check_prop symbol_table pr1;
      check_prop symbol_table pr2
  | Pred (str, param_list) ->
    (match Hashtbl.find_opt symbol_table str with
    | Some ity ->
      (match ity with
      | Tarrow (param_ity_list, Tprop) ->
        List.iter2 (fun param param_ity ->
          let arg_ity = check_term symbol_table param in
          if not (itype_eqb arg_ity param_ity)
          then raise (TypeError (asprintf "%a" pp_term param, param_ity, arg_ity))
          ) param_list param_ity_list
      | _ -> raise (TypeError (str, Tarrow ([], Tprop), ity)))
    | None -> raise (MissingPredicateDeclarationError str))
  | Eq (tm1, tm2)
  | Neq (tm1, tm2) ->
    let ity1 = check_term symbol_table tm1 in
    let ity2 = check_term symbol_table tm2 in
    if not (itype_eqb ity1 ity2) then
    raise (TypeError (asprintf "%a" pp_term tm2, ity1, ity2))

let validate symbol_table
    { decl_types; decl_preds; decl_consts; decl_facts; decl_laws; decl_init } =
  let type_table = Hashtbl.create 17 in
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
        let param_ity_list, res_ity =
          match ity with
          | Tarrow (param_ity_list, res_ity) -> (param_ity_list, res_ity)
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
              param_ity_list
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
    List.iter (check_prop symbol_table) decl_facts;
    List.iter (check_iprop symbol_table) decl_laws;
    List.iter (check_iprop symbol_table) decl_init
  in
  (decl_facts, decl_laws, decl_init)
