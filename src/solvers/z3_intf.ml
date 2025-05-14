open Type
open Internal
open Validate
open Z3

let ctx = mk_context [ ("well_sorted_check", "true"); ("auto_config", "true") ]

(* let init () =
  (*  *)
  Hashtbl.iter (fun str {ity; kind} ->
    match kind with
    | Func | Const ->
    | _ -> ()
    ) symbol_table *)

(* let rec internal_term_to_z3 = function
  | IVar var_id ->
    let var = VarId.export var_id in
    (match Hashtbl.find symbol_table var with
    | {ity = Tcustom typ} ->
      let sort = Sort.mk_uninterpreted_s ctx typ in
      Expr.mk_const_s ctx var sort
    | _ -> assert false)
  | IBVar ind -> ()
  | IConstr (constr_id, tm_arr) ->
    Z3.Datatype.
  | IFunc (func_id, tm_arr) ->

let rec internal_prop_to_z3 =
  let open Monads.OptionMonad in function
  | IPersistent _ -> fail
  | INot pr ->
    let+ pr = internal_prop_to_z3 pr in
    Boolean.mk_not ctx pr
  | IAnd _ -> fail
  | IOr (pr1, pr2) ->
    let* pr1 = internal_prop_to_z3 pr1 in
    let+ pr2 = internal_prop_to_z3 pr2 in
    Boolean.mk_or ctx [ pr1 ; pr2 ]
  | IImply (pr1, pr2) ->
    ()
  | IPred of pred_id * internal_term array
  | IForall of binder_info * internal_prop
  | IExists of binder_info * internal_prop
  | IEq (tm1, tm2) ->
  | INeq of internal_term * internal_term *)
