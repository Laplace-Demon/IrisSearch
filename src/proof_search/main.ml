open Format
open Ast
open Internal
open State
open State_operations
open Search
open Transform
open Validate

let () = Printexc.record_backtrace true

let solve ?(until_validation = false) ?(until_transformation = false)
    ?(show_transformed_instance = false) ?(show_state = false)
    ?(show_path = false) ?(max_depth = 20) fmt ins =
  let () = validate symbol_table ins in
  if until_validation then fprintf fmt "@.Validation succeeds.@.@."
  else
    let ins = uncurry_transformation ins in
    let () =
      if show_transformed_instance then
        fprintf fmt "instance after uncurry_transformation@.@.%a@." pp_instance
          ins
    in
    let ins = merge_quantifier_transformation ins in
    let () =
      if show_transformed_instance then
        fprintf fmt "instance after merge_quantifier_transformation@.@.%a@."
          pp_instance ins
    in
    if until_transformation then fprintf fmt "@.Transformation succeeds.@.@."
    else
      let () = check_form ins in
      let source = initial ins in
      let () =
        if show_state then (
          fprintf fmt "global state@.@.%a@." pp_global_state ();
          fprintf fmt "initial state@.@.%a@." pp_state source)
      in
      let () = Z3_intf.init () in
      (* check if the initial state is consistent *)
      if not (consistent source) then
        fprintf fmt "@.initial state inconsistent@.@."
      else
        let source =
          {
            source with
            ipr_mset =
              SimpleIpropMset.map_multiplicity
                (fun ipr count ->
                  if Persistent_solver.solve ipr then Multiplicity.inf
                  else count)
                source.ipr_mset;
          }
        in
        let open Make (struct
          type node = state

          let source = source
          let successors = successors
          let estimate = estimate

          exception Termination = Termination
        end) in
        let () = set_max_depth max_depth in
        match search () with
        | Some path ->
            let () =
              if show_path then
                fprintf fmt "path@.@.%a%s"
                  (pp_print_list
                     ~pp_sep:(fun fmt () -> fprintf fmt "@.↓@.@.")
                     pp_state)
                  (List.rev path) (get_end_msg ())
            in
            fprintf fmt "@.find solution@.@."
        | None -> fprintf fmt "@.no solution@.@."
