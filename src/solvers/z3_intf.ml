open Type
open Internal
open Validate
open Z3

let ctx = mk_context [ ("well_sorted_check", "true"); ("auto_config", "true") ]
let sort_table : (itype, Sort.sort) Hashtbl.t = Hashtbl.create 17
let func_table : (string, FuncDecl.func_decl) Hashtbl.t = Hashtbl.create 17

let get_sort ity =
  match Hashtbl.find_opt sort_table ity with
  | Some sort -> sort
  | None -> (
      match ity with
      | Tcustom typ -> Sort.mk_uninterpreted_s ctx typ
      | _ -> assert false)

let get_func str = Hashtbl.find func_table str

let init () =
  let type_decls = List.of_seq (Hashtbl.to_seq type_decls) in
  let sort_name_list = List.map fst type_decls in
  let constr_list_list =
    List.map
      (fun (_, constr_list) ->
        List.map
          (fun (constr, param_ity_list) ->
            let open Either in
            let constr_sym = Symbol.mk_string ctx constr in
            let test_sym =
              Symbol.mk_string ctx (String.concat "-" [ "is"; constr ])
            in
            let destr_syms =
              List.mapi
                (fun i _ ->
                  Symbol.mk_string ctx
                    (String.concat "-" [ constr; Int.to_string i ]))
                param_ity_list
            in
            let param_sorts_indices =
              List.map
                (fun ity ->
                  match ity with
                  | Tcustom typ -> (
                      match
                        List.find_index (String.equal typ) sort_name_list
                      with
                      | Some i -> Right i
                      | None -> Left (Sort.mk_uninterpreted_s ctx typ))
                  | _ -> assert false)
                param_ity_list
            in
            let param_sorts =
              List.map
                (function Left sort -> Some sort | Right _ -> None)
                param_sorts_indices
            in
            let param_indices =
              List.map
                (function Right i -> i | Left _ -> 0)
                param_sorts_indices
            in
            Datatype.mk_constructor ctx constr_sym test_sym destr_syms
              param_sorts param_indices)
          constr_list)
      type_decls
  in
  let sort_list = Datatype.mk_sorts_s ctx sort_name_list constr_list_list in
  let () =
    List.iter2
      (fun str sort -> Hashtbl.add sort_table (Tcustom str) sort)
      sort_name_list sort_list
  in
  let () =
    List.iter2
      (fun (_, constr_list) z3_constr_list ->
        List.iter2
          (fun (str, _) constr ->
            Hashtbl.add func_table str
              (Datatype.Constructor.get_constructor_decl constr))
          constr_list z3_constr_list)
      type_decls constr_list_list
  in
  Hashtbl.iter
    (fun str sym_info ->
      match sym_info with
      | { ity = Tarrow (param_ity_list, res_ity); kind = Func } ->
          Hashtbl.add func_table str
            (FuncDecl.mk_func_decl_s ctx str
               (List.map get_sort param_ity_list)
               (get_sort res_ity))
      | _ -> ())
    symbol_table

let rec internal_term_to_z3 { desc; ity } =
  match desc with
  | IVar var_id ->
      let var = VarId.export var_id in
      Expr.mk_const_s ctx var (get_sort ity)
  | IBVar ind -> Quantifier.mk_bound ctx ind (get_sort ity)
  | IConstr (constr_id, tm_arr) ->
      let constr = ConstrId.export constr_id in
      let arg_list = Array.to_list (Array.map internal_term_to_z3 tm_arr) in
      FuncDecl.apply (get_func constr) arg_list
  | IFunc (func_id, tm_arr) ->
      let func = FuncId.export func_id in
      let arg_list = Array.to_list (Array.map internal_term_to_z3 tm_arr) in
      FuncDecl.apply (get_func func) arg_list

let rec internal_prop_to_z3 =
  let open Monads.OptionMonad in
  function
  | IPersistent _ -> fail
  | INot pr ->
      let+ pr = internal_prop_to_z3 pr in
      Boolean.mk_not ctx pr
  | IAnd _ -> fail
  | IOr (pr1, pr2) ->
      let* pr1 = internal_prop_to_z3 pr1 in
      let+ pr2 = internal_prop_to_z3 pr2 in
      Boolean.mk_or ctx [ pr1; pr2 ]
  | IImply (pr1, pr2) ->
      let* pr1 = internal_prop_to_z3 pr1 in
      let+ pr2 = internal_prop_to_z3 pr2 in
      Boolean.mk_implies ctx pr1 pr2
  | IPred _ -> fail
  | IForall ({ shift; typed_str_list }, pr) ->
      let+ pr = internal_prop_to_z3 pr in
      let sort_list, sym_list =
        List.split
          (List.map
             (fun (str, ity) ->
               let sort = get_sort ity in
               (sort, Expr.mk_const_s ctx str sort))
             typed_str_list)
      in
      Quantifier.expr_of_quantifier
        (Quantifier.mk_forall ctx [] [] pr None [] [] None None)
  | IExists ({ shift; typed_str_list }, pr) ->
      let+ pr = internal_prop_to_z3 pr in
      let sort_list, sym_list =
        List.split
          (List.map
             (fun (str, ity) ->
               let sort = get_sort ity in
               (sort, Expr.mk_const_s ctx str sort))
             typed_str_list)
      in
      Quantifier.expr_of_quantifier
        (Quantifier.mk_exists ctx [] [] pr None [] [] None None)
  | IEq (tm1, tm2) ->
      let tm1 = internal_term_to_z3 tm1 in
      let tm2 = internal_term_to_z3 tm2 in
      return (Boolean.mk_eq ctx tm1 tm2)
  | INeq (tm1, tm2) ->
      let tm1 = internal_term_to_z3 tm1 in
      let tm2 = internal_term_to_z3 tm2 in
      return (Boolean.mk_distinct ctx [ tm1; tm2 ])
