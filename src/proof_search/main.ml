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
    ?(show_path = false) ?(show_statistics = false) ?(debug = false)
    ?(max_depth = 20) fmt ins =
  let () = validate symbol_table ins in
  if until_validation then fprintf fmt "Validation succeeds.@\n@."
  else
    let ins = uncurry_transformation ins in
    let () =
      if show_transformed_instance then
        fprintf fmt "  instance after uncurry_transformation@\n@\n%a@\n"
          pp_instance ins
    in
    let ins = merge_quantifier_transformation ins in
    let () =
      if show_transformed_instance then
        fprintf fmt
          "  instance after merge_quantifier_transformation@\n@\n%a@\n"
          pp_instance ins
    in
    if until_transformation then fprintf fmt "Transformation succeeds.@\n@."
    else
      let () = check_form ins in
      let source = initial ins in
      let () =
        if show_state then (
          fprintf fmt "  global state@\n@\n%a@\n" pp_global_state ();
          fprintf fmt "  initial state@\n@\n%a@\n" (pp_state ~pp_index:false)
            source)
      in
      let () = Z3_intf.init () in
      (* check if the initial state is consistent *)
      if not (consistent source) then
        fprintf fmt "Initial state is inconsistent.@\n@."
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
          type state = State.state

          let get_index = get_index
          let pp_state_debug = pp_state_debug
          let source = source

          include Successor

          let successors = successors

          exception Inconsistent = Inconsistent
        end) in
        let () = set_max_depth max_depth in
        let path_opt = search () in
        let () =
          if show_statistics then
            fprintf fmt "%a@\n" (Statistics.pp_stat ~avg:1) ()
        in
        let () =
          if debug then
            let debug_filename = "debug" in
            let debug_dot_filename =
              String.concat "." [ debug_filename; "dot" ]
            in
            let debug_svg_filename =
              String.concat "." [ debug_filename; "svg" ]
            in
            let debug_out_channel = open_out debug_dot_filename in
            let debug_formatter = formatter_of_out_channel debug_out_channel in
            let () =
              fprintf debug_formatter "@[<v 2>digraph {@,%a@,%a@]@\n}@."
                pp_search_graph_debug () pp_laws_debug ()
            in
            let () = close_out debug_out_channel in
            let status =
              Sys.command
                (String.concat " "
                   [
                     "dot";
                     "-T";
                     "svg";
                     debug_dot_filename;
                     "-o";
                     debug_svg_filename;
                   ])
            in
            if status = 0 then fprintf fmt "  graph generation succeeded@\n@\n"
            else fprintf fmt "  graph generation failed@\n@\n"
        in
        match path_opt with
        | Some path ->
            let () = if show_path then fprintf fmt "%a@\n" pp_state_path path in
            fprintf fmt "  find refutation@\n@."
        | None -> fprintf fmt "  no refutation@\n@."
