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
        let ins = Parser.instance Lexer.token lexbuf in
        Main.solve formatter ins)
  with
  | Lexer.Lexing_error s ->
      eprintf "lexing error: %s@." s;
      exit 1
  | Parser.Error ->
      eprintf "parsing error@.";
      exit 1
  | Ast.DuplicateTypeDeclarationError str ->
      eprintf "validation error: duplicate type declaration of %s@." str;
      exit 1
  | Ast.DuplicateConstDeclarationError str ->
      eprintf "validation error: duplicate const declaration of %s@." str;
      exit 1
  | Ast.MissingTypeDeclarationError str ->
      eprintf "validation error: missing type declaration of %s@." str;
      exit 1
  | Ast.MissingConstDeclarationError str ->
      eprintf "validation error: missing const declaration of %s@." str;
      exit 1
  | Ast.TypeError (str, ity1, ity2) ->
      eprintf "validation error: %s should have type %a, but has type %a@." str
        Ast.pp_itype ity1 Ast.pp_itype ity2;
      exit 1
  | e ->
      eprintf "exception: %s\n@." (Printexc.to_string e);
      exit 1
