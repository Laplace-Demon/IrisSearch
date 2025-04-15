open Format
open Is

let in_channel, out_channel =
  let input = ref None in
  let output = ref None in
  let set_input s = input := Some s in
  let set_output s = output := Some s in
  let speclist = [ ("-o", Arg.String set_output, "Specify output file") ] in
  let () = Arg.parse speclist set_input "Usage: is input [-o output]" in
  let in_channel =
    match !input with
    | Some f -> open_in f
    | None ->
        eprintf "input file missing@.";
        exit 1
  in
  let out_channel =
    match !output with Some f -> open_out f | None -> stdout
  in
  (in_channel, out_channel)

let () =
  let lexbuf = Lexing.from_channel in_channel in
  let formatter = formatter_of_out_channel out_channel in
  let finally () =
    Statistics.pp_stat formatter;
    close_in in_channel;
    close_out out_channel
  in
  try
    Fun.protect ~finally (fun () ->
        try
          let ins = Parser.instance Lexer.token lexbuf in
          let () =
            fprintf formatter "original instance@.@.%a@." Ast.pp_instance ins
          in
          let ins = Ast.replace_persistent_transformation ins in
          let () =
            fprintf formatter "transformed instance@.@.%a@." Ast.pp_instance ins
          in
          let () =
            fprintf formatter "global state@.@.%a@." State.pp_state
              !State.global_state
          in
          let source = State.initial ins in
          let () =
            fprintf formatter "initial state@.@.%a@." State.pp_state source
          in
          let open Search.Make (struct
            type node = State.state

            let source = source
            let successors = State.successors
            let terminate = State.terminate
            let estimate = State.estimate
          end) in
          match search () with
          | Some path ->
              let () = fprintf formatter "path@.@." in
              let () =
                pp_print_list
                  ~pp_sep:(fun fmt () -> fprintf fmt "@.↓@.@.")
                  State.pp_state formatter (List.rev path)
              in
              fprintf formatter "@.find solution@.@."
          | None -> fprintf formatter "no solution@.@."
        with Search.Timeout -> fprintf formatter "timeout@.@.")
  with
  | Lexer.Lexing_error s ->
      eprintf "lexing error: %s@." s;
      exit 1
  | Parser.Error ->
      eprintf "parsing error@.";
      exit 1
  | Ast.DuplicateDeclarationError str ->
      eprintf "validation error: duplicate declaration of %s@." str;
      exit 1
  | Ast.TypeError (str, ity1, ity2) ->
      eprintf "validation error: %s should have type %a, but it has type %a" str
        Ast.pp_itype ity1 Ast.pp_itype ity2;
      exit 1
  | e ->
      eprintf "exception: %s\n@." (Printexc.to_string e);
      exit 1
