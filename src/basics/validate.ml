open Format
open Ast
open Type

exception IllegalConstrDeclarationError of string
exception IllegalFuncDeclarationError of string
exception IllegalPredDeclarationError of string
exception IllegalConstDeclarationError of string
exception IllegalLawDeclarationError of string
exception IllegalInitDeclarationError of string
exception DuplicateDeclarationError of string * string
exception MissingDeclarationError of string * string
exception TypeError of string * itype * itype
exception ArityError of string * int * int

module Env = Map.Make (String)

type env = itype Env.t

(** symbol_table is a mutable hashtable storing declared consts, env is an
    immutable map storing variables binded by quantifiers, env shadows
    symbol_table. *)

type symbol_kind = Type | Constr | Func | Pred | Const | Law

let pp_symbol_kind fmt = function
  | Type -> fprintf fmt "type"
  | Constr -> fprintf fmt "constructor"
  | Func -> fprintf fmt "function"
  | Pred -> fprintf fmt "predicate"
  | Const -> fprintf fmt "constant"
  | Law -> fprintf fmt "law"

type symbol_info = { ity : itype; kind : symbol_kind }

let symbol_table : (string, symbol_info) Hashtbl.t = Hashtbl.create 17

type constructor = string * itype list

let type_decls : (string, constructor list) Hashtbl.t = Hashtbl.create 17

let rec check_term symbol_table env = function
  | Var var -> (
      match Env.find_opt var env with
      | Some ity -> ity
      | None -> (
          match Hashtbl.find_opt symbol_table var with
          | Some { ity; kind = Const } -> ity
          | Some { ity = Tcustom _ as ity; kind = Constr } -> ity
          | Some { ity; _ } ->
              raise
                (TypeError
                   (var, Tcustom (asprintf "%a" pp_symbol_kind Const), ity))
          | None ->
              raise
                (MissingDeclarationError
                   (asprintf "%a" pp_symbol_kind Const, var))))
  | App (func, tm_list) -> (
      match Env.find_opt func env with
      | Some ity ->
          raise
            (TypeError (func, Tcustom (asprintf "%a" pp_symbol_kind Func), ity))
      | None -> (
          match Hashtbl.find_opt symbol_table func with
          | Some
              { ity = Tarrow (param_ity_list, res_ity); kind = Constr | Func }
            ->
              if List.length tm_list = List.length param_ity_list then
                let () =
                  List.iter2
                    (fun param param_ity ->
                      let arg_ity = check_term symbol_table env param in
                      if not (itype_eqb arg_ity param_ity) then
                        raise
                          (TypeError
                             (asprintf "%a" pp_term param, param_ity, arg_ity)))
                    tm_list param_ity_list
                in
                res_ity
              else
                raise
                  (ArityError
                     (func, List.length param_ity_list, List.length tm_list))
          | Some { ity; _ } ->
              raise
                (TypeError
                   (func, Tcustom (asprintf "%a" pp_symbol_kind Func), ity))
          | None ->
              raise
                (MissingDeclarationError
                   (asprintf "%a" pp_symbol_kind Func, func))))

let rec check_prop symbol_table env = function
  | Persistent ipr -> check_iprop symbol_table env ipr
  | Not pr -> check_prop symbol_table env pr
  | And (pr1, pr2) | Or (pr1, pr2) | Imply (pr1, pr2) ->
      check_prop symbol_table env pr1;
      check_prop symbol_table env pr2
  | Pred (pred, tm_list) -> (
      match Env.find_opt pred env with
      | Some ity ->
          raise
            (TypeError (pred, Tcustom (asprintf "%a" pp_symbol_kind Pred), ity))
      | None -> (
          match Hashtbl.find_opt symbol_table pred with
          | Some { ity = Tarrow (param_ity_list, Tprop); kind = Pred } ->
              if List.length tm_list = List.length param_ity_list then
                List.iter2
                  (fun param param_ity ->
                    let arg_ity = check_term symbol_table env param in
                    if not (itype_eqb arg_ity param_ity) then
                      raise
                        (TypeError
                           (asprintf "%a" pp_term param, param_ity, arg_ity)))
                  tm_list param_ity_list
              else
                raise
                  (ArityError
                     (pred, List.length param_ity_list, List.length tm_list))
          | Some { ity; _ } ->
              raise
                (TypeError
                   (pred, Tcustom (asprintf "%a" pp_symbol_kind Pred), ity))
          | None ->
              raise
                (MissingDeclarationError
                   (asprintf "%a" pp_symbol_kind Pred, pred))))
  | Forall (typed_str_list, pr) | Exists (typed_str_list, pr) ->
      let new_env =
        List.fold_left
          (fun acc (str, ity) -> Env.add str ity acc)
          env typed_str_list
      in
      check_prop symbol_table new_env pr
  | Eq (tm1, tm2) | Neq (tm1, tm2) ->
      let ity1 = check_term symbol_table env tm1 in
      let ity2 = check_term symbol_table env tm2 in
      if not (itype_eqb ity1 ity2) then
        raise (TypeError (asprintf "%a" pp_term tm2, ity1, ity2))

and check_iprop symbol_table env = function
  | False -> ()
  | Atom atom -> (
      match Env.find_opt atom env with
      | Some ity ->
          raise
            (TypeError (atom, Tcustom (asprintf "%a" pp_symbol_kind Const), ity))
      | None -> (
          match Hashtbl.find_opt symbol_table atom with
          | Some { ity = Tiprop; kind = Const } -> ()
          | Some { ity; _ } -> raise (TypeError (atom, Tiprop, ity))
          | None ->
              raise
                (MissingDeclarationError
                   (asprintf "%a" pp_symbol_kind Const, atom))))
  | Pure pr -> check_prop symbol_table env pr
  | Star (ipr1, ipr2) | HOr (ipr1, ipr2) | Wand (ipr1, ipr2) ->
      check_iprop symbol_table env ipr1;
      check_iprop symbol_table env ipr2
  | Box ipr -> check_iprop symbol_table env ipr
  | HPred (hpred, param_list) -> (
      match Env.find_opt hpred env with
      | Some ity ->
          raise
            (TypeError (hpred, Tcustom (asprintf "%a" pp_symbol_kind Pred), ity))
      | None -> (
          match Hashtbl.find_opt symbol_table hpred with
          | Some { ity = Tarrow (param_ity_list, Tiprop); kind = Pred } ->
              if List.length param_list = List.length param_ity_list then
                List.iter2
                  (fun param param_ity ->
                    let arg_ity = check_term symbol_table env param in
                    if not (itype_eqb arg_ity param_ity) then
                      raise
                        (TypeError
                           (asprintf "%a" pp_term param, param_ity, arg_ity)))
                  param_list param_ity_list
              else
                raise
                  (ArityError
                     (hpred, List.length param_ity_list, List.length param_list))
          | Some { ity; _ } ->
              raise
                (TypeError
                   (hpred, Tcustom (asprintf "%a" pp_symbol_kind Pred), ity))
          | None ->
              raise
                (MissingDeclarationError
                   (asprintf "%a" pp_symbol_kind Pred, hpred))))
  | HForall (typed_str_list, ipr) | HExists (typed_str_list, ipr) ->
      let new_env =
        List.fold_left
          (fun acc (str, ity) -> Env.add str ity acc)
          env typed_str_list
      in
      check_iprop symbol_table new_env ipr

let rec is_iprop_simple = function
  | False | Atom _ | Pure _ | HPred _ -> true
  | Wand _ | HForall _ | HExists _ -> false
  | Star (ipr1, ipr2) | HOr (ipr1, ipr2) ->
      is_iprop_simple ipr1 && is_iprop_simple ipr2
  | Box ipr -> is_iprop_simple ipr

let check_law_left ipr =
  if not (is_iprop_simple ipr) then
    raise
      (IllegalLawDeclarationError
         (asprintf "premise \"%a\" of law should be simple iprop" pp_iprop ipr))

let check_law_right = function
  | HExists (typed_str_list, ipr) ->
      let binded_var_list = List.map fst typed_str_list in
      let free_var_list = iprop_free_var ipr in
      let () =
        List.iter
          (fun binded_var ->
            if not (List.mem binded_var free_var_list) then
              raise
                (IllegalLawDeclarationError
                   (asprintf
                      "existentially quantified variable %S should appear in \
                       the conclusion \"%a\" of law"
                      binded_var pp_iprop ipr)))
          binded_var_list
      in
      if not (is_iprop_simple ipr) then
        raise
          (IllegalLawDeclarationError
             (asprintf
                "conclusion \"%a\" of law should be an existentially \
                 quantified simple iprop"
                pp_iprop ipr))
  | _ as ipr ->
      if not (is_iprop_simple ipr) then
        raise
          (IllegalLawDeclarationError
             (asprintf
                "conclusion \"%a\" of law should be an existentially \
                 quantified simple iprop"
                pp_iprop ipr))

let check_law (_, law) =
  match law with
  | HForall (typed_str_list, Wand (ipr1, ipr2)) ->
      let binded_var_list = List.map fst typed_str_list in
      let free_var_list = iprop_free_var ipr1 in
      let () =
        List.iter
          (fun binded_var ->
            if not (List.mem binded_var free_var_list) then
              raise
                (IllegalLawDeclarationError
                   (asprintf
                      "universally quantified variable %S should appear in the \
                       premise \"%a\" of law"
                      binded_var pp_iprop ipr1)))
          binded_var_list
      in
      check_law_left ipr1;
      check_law_right ipr2
  | Wand (ipr1, ipr2) ->
      check_law_left ipr1;
      check_law_right ipr2
  | _ as ipr ->
      raise
        (IllegalLawDeclarationError
           (asprintf "law \"%a\" should be a universally quantified wand"
              pp_iprop ipr))

let check_init ipr =
  if not (is_iprop_simple ipr) then
    raise
      (IllegalInitDeclarationError
         (asprintf "iprop \"%a\" in init declaration should be simple" pp_iprop
            ipr))

let validate symbol_table
    {
      decl_types;
      decl_funcs;
      decl_preds;
      decl_consts;
      decl_facts;
      decl_laws;
      decl_init;
    } =
  let () =
    (* Check type declarations. *)
    List.iter
      (fun (typ, constr_list) ->
        let () =
          (* Check if the type has been defined. *)
          match typ with
          | "Prop" | "iProp" ->
              raise
                (DuplicateDeclarationError
                   (asprintf "%a" pp_symbol_kind Type, typ))
          | _ ->
              if Hashtbl.mem type_decls typ then
                raise
                  (DuplicateDeclarationError
                     (asprintf "%a" pp_symbol_kind Type, typ))
              else Hashtbl.add type_decls typ []
        in
        let () =
          List.iter
            (fun (constr, constr_ity) ->
              let () =
                (* Check if the result type of constructors equal to the above type, and the parameter types are declared,
               first-order, and different from iProp and Prop. *)
                match constr_ity with
                | Tarrow (param_ity_list, Tcustom res_typ)
                  when String.equal typ res_typ ->
                    List.iter
                      (function
                        | Tcustom param_typ ->
                            if not (Hashtbl.mem type_decls param_typ) then
                              raise
                                (MissingDeclarationError
                                   (asprintf "%a" pp_symbol_kind Type, param_typ))
                        | _ -> raise (IllegalConstrDeclarationError constr))
                      param_ity_list
                | Tcustom res_typ when String.equal typ res_typ -> ()
                | _ -> raise (IllegalConstrDeclarationError constr)
              in
              (* Check if the constructor is already declared. *)
              if Hashtbl.mem symbol_table constr then
                raise
                  (DuplicateDeclarationError
                     (asprintf "%a" pp_symbol_kind Constr, constr))
              else
                (* Build the symbol table. *)
                Hashtbl.add symbol_table constr
                  { ity = constr_ity; kind = Constr })
            constr_list
        in
        Hashtbl.replace type_decls typ
          (List.map
             (fun (constr, ity) ->
               match ity with
               | Tarrow (param_ity_list, _) -> (constr, param_ity_list)
               | Tcustom _ -> (constr, [])
               | _ -> assert false)
             constr_list))
      decl_types
  in
  let () =
    (* Check function declarations. *)
    List.iter
      (fun (func, func_ity) ->
        let param_ity_list, res_ity =
          match func_ity with
          | Tarrow (param_ity_list, res_ity) -> (param_ity_list, res_ity)
          | _ -> raise (IllegalFuncDeclarationError func)
        in
        let () =
          (* Check if the result type of the function exist and is a term type. *)
          match res_ity with
          | Tcustom res_typ ->
              if not (Hashtbl.mem type_decls res_typ) then
                raise
                  (MissingDeclarationError
                     (asprintf "%a" pp_symbol_kind Type, res_typ))
          | _ -> raise (IllegalFuncDeclarationError func)
        in
        let () =
          (* Check if the parameter types of the function are declared, first-order, and different from iProp and Prop. *)
          List.iter
            (function
              | Tcustom param_typ ->
                  if not (Hashtbl.mem type_decls param_typ) then
                    raise
                      (MissingDeclarationError
                         (asprintf "%a" pp_symbol_kind Type, param_typ))
              | _ -> raise (IllegalFuncDeclarationError func))
            param_ity_list
        in
        (* Check if the function is already declared. *)
        if Hashtbl.mem symbol_table func then
          raise
            (DuplicateDeclarationError (asprintf "%a" pp_symbol_kind Func, func))
        else
          (* Build the symbol table. *)
          Hashtbl.add symbol_table func { ity = func_ity; kind = Func })
      decl_funcs
  in
  let () =
    (* Check predicate declarations. *)
    List.iter
      (fun (pred, pred_ity) ->
        let param_ity_list, res_ity =
          match pred_ity with
          | Tarrow (param_ity_list, res_ity) -> (param_ity_list, res_ity)
          | _ -> raise (IllegalPredDeclarationError pred)
        in
        let () =
          (* Check if the result type of the predicate is either iProp or Prop. *)
          match res_ity with
          | Tiprop | Tprop -> ()
          | _ -> raise (IllegalPredDeclarationError pred)
        in
        let () =
          (* Check if the parameter types of the predicate are declared, first-order, and different from iProp and Prop. *)
          List.iter
            (function
              | Tcustom param_typ ->
                  if not (Hashtbl.mem type_decls param_typ) then
                    raise
                      (MissingDeclarationError
                         (asprintf "%a" pp_symbol_kind Type, param_typ))
              | _ -> raise (IllegalPredDeclarationError pred))
            param_ity_list
        in
        (* Check if the predicate is already declared. *)
        if Hashtbl.mem symbol_table pred then
          raise
            (DuplicateDeclarationError (asprintf "%a" pp_symbol_kind Pred, pred))
        else
          (* Build the symbol table. *)
          Hashtbl.add symbol_table pred { ity = pred_ity; kind = Pred })
      decl_preds
  in
  let () =
    (* Check constant declarations. *)
    List.iter
      (fun (const, const_ity) ->
        let () =
          (* Check if the type of the constant is declared. *)
          match const_ity with
          | Tcustom typ ->
              if not (Hashtbl.mem type_decls typ) then
                raise
                  (MissingDeclarationError
                     (asprintf "%a" pp_symbol_kind Type, typ))
          | Tarrow _ -> raise (IllegalConstDeclarationError const)
          | _ -> ()
        in
        (* Check if the constant is already declared. *)
        if Hashtbl.mem symbol_table const then
          raise
            (DuplicateDeclarationError
               (asprintf "%a" pp_symbol_kind Const, const))
        else
          (* Build the symbol table. *)
          Hashtbl.add symbol_table const { ity = const_ity; kind = Const })
      decl_consts
  in
  List.iter (check_prop symbol_table Env.empty) decl_facts;
  List.iter
    (fun (name_opt, ipr) ->
      (match name_opt with
      | None -> ()
      | Some name ->
          if Hashtbl.mem symbol_table name then
            raise
              (DuplicateDeclarationError (asprintf "%a" pp_symbol_kind Law, name))
          else Hashtbl.add symbol_table name { ity = Tlaw; kind = Law });
      check_iprop symbol_table Env.empty ipr)
    decl_laws;
  List.iter (check_iprop symbol_table Env.empty) decl_init

let check_form { decl_laws; decl_init; _ } =
  List.iter check_law decl_laws;
  List.iter check_init decl_init
