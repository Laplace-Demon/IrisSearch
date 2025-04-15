open Format
open Is

let mode, out_channel =
  let mode = ref None in
  let output = ref None in
  let set_output s = output := Some s in
  let speclist = [ ("-o", Arg.String set_output, "Specify output file") ] in
  let set_mode s =
    match String.split_on_char '=' s with
    | [ "mode"; "negative" ] -> mode := Some "negative"
    | _ ->
        eprintf "Invalid argument: %s@.Usage: fuzz mode=negative [-o output]@."
          s;
        exit 1
  in
  let () = Arg.parse speclist set_mode "Usage: fuzz input [-o output]" in
  let mode =
    match !mode with
    | Some m -> m
    | None ->
        eprintf "Missing required mode=negative argument@.";
        exit 1
  in
  let out_channel =
    match !output with Some f -> open_out f | None -> stdout
  in
  (mode, out_channel)

let () =
  let formatter = formatter_of_out_channel out_channel in
  let finally () = close_out out_channel in
  try
    Fun.protect ~finally (fun () ->
        match mode with
        | "negative" ->
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
                      Main.solve std_formatter ins
                    done;
                    fprintf formatter "law_num:%i@.law_size:%i@.@." law_num
                      law_size;
                    Statistics.pp_stat ~avg:10 formatter)
                  law_size_list)
              law_num_list
        | _ ->
            eprintf
              "Invalid argument: %s@.Usage: fuzz mode=negative [-o output]@."
              mode;
            exit 1)
  with e ->
    eprintf "exception: %s\n@." (Printexc.to_string e);
    exit 1
