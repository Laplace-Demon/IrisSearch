open Type
open Internal
open Validate
open State
open Z3

let ctx = mk_context []
let facts = ref (Boolean.mk_true ctx)
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

let mute = ref 0

let rec get_forall typed_str_list pr =
  let sort_list, sym_list =
    List.split
      (List.map
         (fun (str, ity) ->
           let sort = get_sort ity in
           (sort, Symbol.mk_string ctx str))
         typed_str_list)
  in
  Quantifier.expr_of_quantifier
    (Quantifier.mk_forall ctx sort_list sym_list pr None [] [] None None)

and get_exists typed_str_list pr =
  let sort_list, sym_list =
    List.split
      (List.map
         (fun (str, ity) ->
           let sort = get_sort ity in
           (sort, Symbol.mk_string ctx str))
         typed_str_list)
  in
  Quantifier.expr_of_quantifier
    (Quantifier.mk_exists ctx sort_list sym_list pr None [] [] None None)

and internal_prop_to_z3 = function
  | IPersistent _ ->
      let ans =
        Expr.mk_const_s ctx
          (String.concat "-" [ "Per"; Int.to_string !mute ])
          (Boolean.mk_sort ctx)
      in
      mute := !mute + 1;
      ans
  | INot pr ->
      let pr = internal_prop_to_z3 pr in
      Boolean.mk_not ctx pr
  | IAnd pr_set -> internal_prop_set_to_z3 pr_set
  | IOr (pr1, pr2) ->
      let pr1 = internal_prop_to_z3 pr1 in
      let pr2 = internal_prop_to_z3 pr2 in
      Boolean.mk_or ctx [ pr1; pr2 ]
  | IImply (pr1, pr2) ->
      let pr1 = internal_prop_to_z3 pr1 in
      let pr2 = internal_prop_to_z3 pr2 in
      Boolean.mk_implies ctx pr1 pr2
  | IPred (pred_id, _) ->
      let pred = PredId.export pred_id in
      let ans =
        Expr.mk_const_s ctx
          (String.concat "-" [ pred; Int.to_string !mute ])
          (Boolean.mk_sort ctx)
      in
      mute := !mute + 1;
      ans
  | IForall ({ typed_str_list }, pr) ->
      let pr = internal_prop_to_z3 pr in
      get_forall typed_str_list pr
  | IExists ({ typed_str_list }, pr) ->
      let pr = internal_prop_to_z3 pr in
      get_exists typed_str_list pr
  | IEq (tm1, tm2) ->
      let tm1 = internal_term_to_z3 tm1 in
      let tm2 = internal_term_to_z3 tm2 in
      Boolean.mk_eq ctx tm1 tm2
  | INeq (tm1, tm2) ->
      let tm1 = internal_term_to_z3 tm1 in
      let tm2 = internal_term_to_z3 tm2 in
      Boolean.mk_distinct ctx [ tm1; tm2 ]

and internal_prop_set_to_z3 pr_set =
  Boolean.mk_and ctx (List.map internal_prop_to_z3 (PropSet.to_list pr_set))

let build_datatype () =
  let type_decls =
    List.filter
      (fun (_, constr_list) -> not (List.is_empty constr_list))
      (List.of_seq (Hashtbl.to_seq type_decls))
  in
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

let build_facts () = facts := internal_prop_set_to_z3 global_state.facts

let init () =
  build_datatype ();
  build_facts ()

let equality_solver st_opt tm1 tm2 =
  let tm1 = internal_term_to_z3 tm1 in
  let tm2 = internal_term_to_z3 tm2 in
  let eq = Boolean.mk_eq ctx tm1 tm2 in
  let solver = Solver.mk_solver ctx None in
  let assums =
    match st_opt with
    | None -> [ !facts; Boolean.mk_not ctx eq ]
    | Some { local_var_list; pr_set } ->
        (* pay attention to the semantics difference between this solver and the others, tm1 and tm2 are supposed to be in the state *)
        [
          !facts;
          get_exists local_var_list
            (Boolean.mk_and ctx
               [ internal_prop_set_to_z3 pr_set; Boolean.mk_not ctx eq ]);
        ]
  in
  match Solver.check solver assums with
  | Solver.SATISFIABLE -> false
  | Solver.UNSATISFIABLE -> true
  | Solver.UNKNOWN -> false

let consistent_solver st_opt =
  let assums =
    match st_opt with
    | None -> [ !facts ]
    | Some { local_var_list; pr_set } ->
        [
          !facts;
          internal_prop_to_z3 (iExists_raw (local_var_list, iAnd pr_set));
        ]
  in
  let solver = Solver.mk_solver ctx None in
  match Solver.check solver assums with
  | Solver.SATISFIABLE -> None
  | Solver.UNSATISFIABLE ->
      let unsat_core = Solver.get_unsat_core solver in
      Some
        Format.(
          asprintf "@.@[<v 4>Unsat core:@,%a@]@."
            (pp_print_list pp_print_string)
            (List.map Expr.to_string unsat_core))
  | Solver.UNKNOWN -> None

let implication_solver st_opt goal =
  if PropSet.is_empty goal then true
  else
    let goal = internal_prop_set_to_z3 goal in
    let assums =
      match st_opt with
      | None -> [ !facts; Boolean.mk_not ctx goal ]
      | Some { local_var_list; pr_set } ->
          [
            !facts;
            internal_prop_to_z3 (iExists_raw (local_var_list, iAnd pr_set));
            Boolean.mk_not ctx goal;
          ]
    in
    let solver = Solver.mk_solver ctx None in
    match Solver.check solver assums with
    | Solver.SATISFIABLE -> false
    | Solver.UNSATISFIABLE -> true
    | Solver.UNKNOWN -> false
