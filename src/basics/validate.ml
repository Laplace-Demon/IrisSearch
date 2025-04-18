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
  | Var var -> (
      match Hashtbl.find_opt symbol_table var with
      | Some ity -> ity
      | None -> raise (MissingConstDeclarationError var))

let rec check_iprop symbol_table = function
  | False -> ()
  | Atom atom -> (
      match Hashtbl.find_opt symbol_table atom with
      | Some ity ->
          if not (itype_eqb ity Tiprop) then
            raise (TypeError (atom, Tiprop, ity))
      | None -> raise (MissingConstDeclarationError atom))
  | Star (ipr1, ipr2) ->
      check_iprop symbol_table ipr1;
      check_iprop symbol_table ipr2
  | Wand (ipr1, ipr2) ->
      check_iprop symbol_table ipr1;
      check_iprop symbol_table ipr2
  | Box ipr -> check_iprop symbol_table ipr
  | Pure pr -> check_prop symbol_table pr
  | HPred (hpred, param_list) -> (
      match Hashtbl.find_opt symbol_table hpred with
      | Some ity -> (
          match ity with
          | Tarrow (param_ity_list, Tiprop) ->
              List.iter2
                (fun param param_ity ->
                  let arg_ity = check_term symbol_table param in
                  if not (itype_eqb arg_ity param_ity) then
                    raise
                      (TypeError
                         (asprintf "%a" pp_term param, param_ity, arg_ity)))
                param_list param_ity_list
          | _ -> raise (TypeError (hpred, Tarrow ([], Tiprop), ity)))
      | None -> raise (MissingPredicateDeclarationError hpred))

and check_prop symbol_table = function
  | Persistent ipr -> check_iprop symbol_table ipr
  | Not pr -> check_prop symbol_table pr
  | And (pr1, pr2) | Or (pr1, pr2) | Imply (pr1, pr2) ->
      check_prop symbol_table pr1;
      check_prop symbol_table pr2
  | Pred (pred, param_list) -> (
      match Hashtbl.find_opt symbol_table pred with
      | Some ity -> (
          match ity with
          | Tarrow (param_ity_list, Tprop) ->
              List.iter2
                (fun param param_ity ->
                  let arg_ity = check_term symbol_table param in
                  if not (itype_eqb arg_ity param_ity) then
                    raise
                      (TypeError
                         (asprintf "%a" pp_term param, param_ity, arg_ity)))
                param_list param_ity_list
          | _ -> raise (TypeError (pred, Tarrow ([], Tprop), ity)))
      | None -> raise (MissingPredicateDeclarationError pred))
  | Eq (tm1, tm2) | Neq (tm1, tm2) ->
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
      (fun typ ->
        match typ with
        | "Prop" | "iProp" -> raise (DuplicateTypeDeclarationError typ)
        | _ ->
            if Hashtbl.mem type_table typ then
              raise (DuplicateTypeDeclarationError typ)
            else Hashtbl.add type_table typ ())
      decl_types
  in
  let () =
    (* Check predicate declarations. *)
    List.iter
      (fun (pred, ity) ->
        let param_ity_list, res_ity =
          match ity with
          | Tarrow (param_ity_list, res_ity) -> (param_ity_list, res_ity)
          | _ -> assert false
        in
        let () =
          (* Check if the result type of the predicate is either iProp or Prop. *)
          match res_ity with
          | Tiprop | Tprop -> ()
          | _ -> raise (IllegalPredicateDeclarationError pred)
        in
        let () =
          (* Check if the parameter types of the predicate is declared, first-order, and different from iProp and Prop *)
          List.iter
            (function
              | Tiprop | Tprop | Tarrow _ ->
                  raise (IllegalPredicateDeclarationError pred)
              | Tcustom typ ->
                  if not (Hashtbl.mem type_table typ) then
                    raise (MissingTypeDeclarationError typ))
            param_ity_list
        in
        (* Check if the predicate is already declared. *)
        if Hashtbl.mem symbol_table pred then
          raise (DuplicatePredicateDeclarationError pred)
        else
          (* Build the symbol table. *)
          Hashtbl.add symbol_table pred ity)
      decl_preds
  in
  let () =
    (* Check constant declarations. *)
    List.iter
      (fun (const, ity) ->
        let () =
          (* Check if the type of the constant is declared. *)
          match ity with
          | Tcustom typ ->
              if not (Hashtbl.mem type_table typ) then
                raise (MissingTypeDeclarationError typ)
          | _ -> ()
        in
        (* Check if the constant is already declared. *)
        if Hashtbl.mem symbol_table const then
          raise (DuplicateConstDeclarationError const)
        else
          (* Build the symbol table. *)
          Hashtbl.add symbol_table const ity)
      decl_consts
  in
  (* Check type. *)
  List.iter (check_prop symbol_table) decl_facts;
  List.iter (check_iprop symbol_table) decl_laws;
  List.iter (check_iprop symbol_table) decl_init
