open Format
open Is

let until_parsing = ref false
let until_validation = ref false
let until_transformation = ref false
let show_instance = ref false
let show_transformed_instance = ref false
let show_global_state = ref false
let show_initial_state = ref false
let show_path = ref false
let show_statistics = ref false

let input_filename, in_channel, out_channel =
  let input = ref None in
  let output = ref None in
  let set_input s = input := Some s in
  let set_output s = output := Some s in
  let speclist =
    [
      ("-o", Arg.String set_output, "\n\tSpecify output file.\n");
      ("--until-parsing", Arg.Set until_parsing, "\n\tStop after parsing.\n");
      ( "--until-validation",
        Arg.Set until_validation,
        "\n\tStop after validation.\n" );
      ( "--until-transformation",
        Arg.Set until_transformation,
        "\n\ttop after transformation.\n" );
      ("--show-instance", Arg.Set show_instance, "\n\tPrint problem instance.\n");
      ( "--show-transformed-instance",
        Arg.Set show_transformed_instance,
        "\n\tPrint transformed problem instance.\n" );
      ( "--show-global-state",
        Arg.Set show_global_state,
        "\n\tPrint the global state.\n" );
      ( "--show-initial-state",
        Arg.Set show_initial_state,
        "\n\tPrint the initial state.\n" );
      ( "--show-path",
        Arg.Set show_path,
        "\n\tPrint the path when searching succeeds.\n" );
      ( "--show-statistics",
        Arg.Set show_statistics,
        "\n\tPrint the statistics.\n" );
      ( "--show-all",
        Arg.Tuple
          [
            Arg.Set show_instance;
            Arg.Set show_transformed_instance;
            Arg.Set show_global_state;
            Arg.Set show_initial_state;
            Arg.Set show_path;
            Arg.Set show_statistics;
          ],
        "\n\tPrint all the information above.\n" );
    ]
  in
  let () = Arg.parse speclist set_input "Usage: is input [-o output]" in
  let input_filename, in_channel =
    match !input with
    | Some f -> (f, open_in f)
    | None ->
        eprintf "input file missing@.";
        exit 1
  in
  let out_channel =
    match !output with Some f -> open_out f | None -> stdout
  in
  (input_filename, in_channel, out_channel)

let () =
  let lexbuf = Lexing.from_channel in_channel in
  let formatter = formatter_of_out_channel out_channel in
  let finally () =
    if !show_statistics then Statistics.pp_stat formatter;
    close_in in_channel;
    close_out out_channel
  in
  try
    Fun.protect ~finally (fun () ->
        let ins = Parser.instance Lexer.token lexbuf in
        let () =
          if !show_instance then
            fprintf formatter "original instance@.@.%a@." Ast.pp_instance ins
        in
        if !until_parsing then fprintf formatter "@.Parsing succeeds.@.@."
        else
          Main.solve ~until_validation:!until_validation
            ~until_transformation:!until_transformation
            ~show_transformed_instance:!show_transformed_instance
            ~show_global_state:!show_global_state
            ~show_initial_state:!show_initial_state ~show_path:!show_path
            formatter ins)
  with
  | Lexer.Lexing_error s ->
      eprintf "%s: lexing error: %s@." input_filename s;
      exit 1
  | Parser.Error ->
      eprintf "%s%a: parsing error@." input_filename Lexer.print_position lexbuf;
      exit 1
  | Validate.IllegalPredicateDeclarationError str ->
      eprintf "validation error: illegal predicate declaration of %s@." str;
      exit 1
  | Validate.DuplicateTypeDeclarationError str ->
      eprintf "validation error: duplicate type declaration of %s@." str;
      exit 1
  | Validate.DuplicatePredicateDeclarationError str ->
      eprintf "validation error: duplicate predicate declaration of %s@." str;
      exit 1
  | Validate.DuplicateConstDeclarationError str ->
      eprintf "validation error: duplicate const declaration of %s@." str;
      exit 1
  | Validate.MissingTypeDeclarationError str ->
      eprintf "validation error: missing type declaration of %s@." str;
      exit 1
  | Validate.MissingPredicateDeclarationError str ->
      eprintf "validation error: missing predicate declaration of %s@." str;
      exit 1
  | Validate.MissingConstDeclarationError str ->
      eprintf "validation error: missing const declaration of %s@." str;
      exit 1
  | Validate.TypeError (str, ity1, ity2) ->
      eprintf "validation error: %s should have type %a, but has type %a@." str
        Type.pp_itype ity1 Type.pp_itype ity2;
      exit 1
  | e ->
      eprintf "exception: %s\n@." (Printexc.to_string e);
      exit 1
