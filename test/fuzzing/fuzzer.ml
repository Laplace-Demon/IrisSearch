open Format
open Is

let output_filename = ref None
let mode = ref "single"
let law_num = ref 10
let law_size = ref 5

let out_channel =
  let set_output s = output_filename := Some s in
  let speclist =
    [
      ("-o", Arg.String set_output, "\n\tSpecify output file.\n");
      ( "--mode",
        Arg.String
          (fun s ->
            match s with
            | "single" | "multiple" -> mode := s
            | _ ->
                eprintf
                  "Invalid argument of option \"--mode\": %s@.Usage: fuzzer \
                   [--mode=single|multiple]@."
                  s;
                exit 1),
        "\n\tSpecify the fuzzing mode.\n" );
      ( "--law-num",
        Arg.Int (fun i -> law_num := i),
        "\n\tSpecify the number of laws in the generated instance.\n" );
      ( "--law-size",
        Arg.Int (fun i -> law_size := i),
        "\n\tSpecify the size of laws in the generated instance.\n" );
    ]
  in
  let parse_arg =
   fun arg ->
    if String.equal "" arg then ()
    else
      eprintf
        "Invalid argument: %s@.Usage: fuzzer [-o output] \
         [--mode=single|multiple] [--law-num=integer] [--law-size=integer]@."
        arg;
    exit 1
  in
  let () =
    Arg.parse speclist parse_arg
      "Usage: fuzzer [-o output] [--mode=single|multiple] [--law-num=integer] \
       [--law-size=integer]@."
  in
  let out_channel =
    match !output_filename with Some f -> open_out f | None -> stdout
  in
  out_channel

let fuzzer_single () =
  let formatter = formatter_of_out_channel out_channel in
  let input_filename = "input" in
  let input_file_channel = open_out input_filename in
  let input_file_formatter = formatter_of_out_channel input_file_channel in
  let finally () =
    close_out out_channel;
    close_out input_file_channel
  in
  try
    Fun.protect ~finally (fun () ->
        let module Fuzzer = Negative.Negative in
        let () = Fuzzer.set_law_num !law_num in
        let () = Fuzzer.set_law_size !law_size in
        let ins = Fuzzer.generate () in
        let () = fprintf input_file_formatter "%a" Ast.pp_instance ins in
        let _ =
          Sys.command
            ("is " ^ input_filename ^ " --show-all"
            ^ match !output_filename with Some f -> " -o " ^ f | None -> "")
        in
        ())
  with e ->
    eprintf "exception: %s\n@." (Printexc.to_string e);
    exit 1

let fuzzer_multiple () =
  let formatter = formatter_of_out_channel out_channel in
  let finally () = close_out out_channel in
  try
    Fun.protect ~finally (fun () ->
        let module Fuzzer = Negative.Negative in
        let law_num_list = List.init 10 (fun i -> (i + 1) * 10) in
        let law_size_list = List.init 9 (fun i -> i + 2) in
        List.iter
          (fun law_num ->
            List.iter
              (fun law_size ->
                Statistics.reset ();
                Fuzzer.set_law_num law_num;
                Fuzzer.set_law_size law_size;
                for _ = 1 to 10 do
                  let ins = Fuzzer.generate () in
                  Main.solve formatter ins
                done;
                fprintf formatter "law_num:%i@.law_size:%i@.@." law_num law_size;
                Statistics.pp_stat ~avg:10 formatter)
              law_size_list)
          law_num_list)
  with e ->
    eprintf "exception: %s\n@." (Printexc.to_string e);
    exit 1

let () =
  let formatter = formatter_of_out_channel out_channel in
  let finally () = close_out out_channel in
  try
    Fun.protect ~finally (fun () ->
        let module Fuzzer = Negative.Negative in
        match !mode with
        | "single" -> fuzzer_single ()
        | "multiple" -> fuzzer_multiple ()
        | _ -> assert false)
  with e ->
    eprintf "exception: %s\n@." (Printexc.to_string e);
    exit 1
