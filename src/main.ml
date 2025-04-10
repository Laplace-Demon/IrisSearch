open Format
open Lexing
open Parser
open Ast
open State
open Search
open Statistics

let in_channel, out_channel =
  let input = ref None in
  let output = ref None in
  let set_input s = input := Some s in
  let set_output s = output := Some s in
  let speclist = [ ("-o", Arg.String set_output, "Specify output file") ] in
  let () = Arg.parse speclist set_input "Usage: main input [-o output]" in
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
    pp_stat formatter;
    close_in in_channel;
    close_out out_channel
  in
  try
    Fun.protect ~finally (fun () ->
        try
          let ins = Parser.instance Lexer.token lexbuf in
          let source = initial ins in
          let () = fprintf formatter "instance@.@.%a@." pp_instance ins in
          let () = fprintf formatter "global state@.@.%a@." pp_state !global_state in
          let () = fprintf formatter "initial state@.@.%a@." pp_state source in
          let open Search.Make (struct
            type node = state

            let source = source
            let successors = successors
            let terminate = terminate
            let estimate = estimate
          end) in
          match search (fun _ -> ()) with
          | Some path ->
              let () = fprintf formatter "path@.@."in
              let () = pp_print_list
                ~pp_sep:(fun fmt () -> fprintf fmt "@.↑@.@.")
                pp_state formatter path in
              fprintf formatter "@.find solution@.@."
          | None -> fprintf formatter "no solution@.@."
        with Timeout -> fprintf formatter "timeout@.@.")
  with
  | Lexer.Lexing_error s ->
      eprintf "lexing error: %s@." s;
      exit 1
  | Parser.Error ->
      eprintf "parsing error@.";
      exit 1
  | DuplicateDeclarationError str ->
      eprintf "validation error: duplicate declaration of %s@." str;
      exit 1
  | TypeError (str, ity1, ity2) ->
      eprintf "validation error: %s should have type %a, but it has type %a" str
        pp_itype ity1 pp_itype ity2;
      exit 1
  | e ->
      eprintf "exception: %s\n@." (Printexc.to_string e);
      exit 1
