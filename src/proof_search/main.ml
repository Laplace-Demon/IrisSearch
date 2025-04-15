open Format
open Ast
open State
open Search

let solve fmt ins =
  let () = fprintf fmt "original instance@.@.%a@." pp_instance ins in
  let ins = replace_persistent_transformation ins in
  let () = fprintf fmt "transformed instance@.@.%a@." pp_instance ins in
  let () =
    fprintf fmt "global state@.@.%a@." pp_state !global_state
  in
  let source = initial ins in
  let () = fprintf fmt "initial state@.@.%a@." pp_state source in
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
        let () = fprintf fmt "path@.@." in
        let () =
          pp_print_list
            ~pp_sep:(fun fmt () -> fprintf fmt "@.↓@.@.")
            pp_state fmt (List.rev path)
        in
        fprintf fmt "@.find solution@.@."
    | None -> fprintf fmt "no solution@.@."
  with Timeout -> fprintf fmt "timeout@.@."
