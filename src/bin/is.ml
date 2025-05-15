open Format
open Is

let () = Printexc.record_backtrace true
let until_parsing = ref false
let until_validation = ref false
let until_transformation = ref false
let show_instance = ref false
let show_transformed_instance = ref false
let show_state = ref false
let show_path = ref false
let show_statistics = ref false
let max_depth = ref 20

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
      ( "--show-state",
        Arg.Set show_state,
        "\n\tPrint the global and initial state.\n" );
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
            Arg.Set show_state;
            Arg.Set show_path;
            Arg.Set show_statistics;
          ],
        "\n\tPrint all the information above.\n" );
      ( "--max-depth",
        Arg.Int (fun d -> max_depth := d),
        "\n\tSet maximum search depth.\n" );
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
            ~show_state:!show_state ~show_path:!show_path ~max_depth:!max_depth
            formatter ins)
  with
  | Lexer.Lexing_error s ->
      eprintf "%s: lexing error: %s@." input_filename s;
      exit 1
  | Parser.Error ->
      eprintf "%s%a: parsing error@." input_filename Lexer.print_position lexbuf;
      exit 1
  | Validate.IllegalConstrDeclarationError str ->
      eprintf "validation error: illegal constructor declaration of %s@." str;
      exit 1
  | Validate.IllegalFuncDeclarationError str ->
      eprintf "validation error: illegal function declaration of %s@." str;
      exit 1
  | Validate.IllegalPredDeclarationError str ->
      eprintf "validation error: illegal predicate declaration of %s@." str;
      exit 1
  | Validate.IllegalConstDeclarationError str ->
      eprintf "validation error: illegal constant declaration of %s@." str;
      exit 1
  | Validate.IllegalLawDeclarationError str ->
      eprintf "validation error: illegal law declaration, %s@." str;
      exit 1
  | Validate.IllegalInitDeclarationError str ->
      eprintf "validation error: illegal init declaration, %s@." str;
      exit 1
  | Validate.DuplicateDeclarationError (kind, str) ->
      eprintf "validation error: duplicate %s declaration of %s@." kind str;
      exit 1
  | Validate.MissingDeclarationError (kind, str) ->
      eprintf "validation error: missing %s declaration of %s@." kind str;
      exit 1
  | Validate.TypeError (str, ity1, ity2) ->
      eprintf "validation error: %s should have type %a, but has type %a@." str
        Type.pp_itype ity1 Type.pp_itype ity2;
      exit 1
  | Validate.ArityError (str, a1, a2) ->
      eprintf "validation error: %s should have arity %i, but has arity %i@."
        str a1 a2;
      exit 1
  | e ->
      let bt = Printexc.get_backtrace () in
      eprintf "exception: %s@.backtrace:@.%s@." (Printexc.to_string e) bt;
      exit 1
