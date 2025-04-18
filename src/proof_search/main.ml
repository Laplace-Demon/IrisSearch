open Format
open Ast
open State
open State_operations
open Search

let solve ?(until_validation = false) ?(until_transformation = false)
    ?(show_transformed_instance = false) ?(show_global_state = false)
    ?(show_initial_state = false) ?(show_path = false) fmt ins =
  let () = Validate.validate symbol_table ins in
  if until_validation then fprintf fmt "@.Validation succeeds.@.@."
  else
    let ins = uncurry_transformation ins in
    let () =
      if show_transformed_instance then
        fprintf fmt "instance after uncurry_transformation@.@.%a@." pp_instance
          ins
    in
    let ins = eliminate_persistent_transformation ins in
    let () =
      if show_transformed_instance then
        fprintf fmt "instance after eliminate_persistent_transformation@.@.%a@."
          pp_instance ins
    in
    if until_transformation then fprintf fmt "@.Transformation succeeds.@.@."
    else
      let () =
        if show_global_state then
          fprintf fmt "global state@.@.%a@." pp_state !global_state
      in
      let source = initial ins in
      let () =
        if show_initial_state then
          fprintf fmt "initial state@.@.%a@." pp_state source
      in

      let open Make (struct
        type node = state

        let source = source
        let successors = successors
        let terminate = terminate
        let estimate = estimate
      end) in
      try
        match search () with
        | Some path ->
            let () =
              if show_path then
                fprintf fmt "path@.@.%a"
                  (pp_print_list
                     ~pp_sep:(fun fmt () -> fprintf fmt "@.↓@.@.")
                     pp_state)
                  (List.rev path)
            in
            fprintf fmt "@.find solution@.@."
        | None -> fprintf fmt "@.no solution@.@."
      with Timeout -> fprintf fmt "@.timeout@.@."
