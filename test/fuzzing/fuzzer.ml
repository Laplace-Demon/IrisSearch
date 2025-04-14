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
  let tmp = "tmp" in
  let tmp_out_channel = open_out tmp in
  let tmp_formatter = formatter_of_out_channel tmp_out_channel in
  let formatter = formatter_of_out_channel out_channel in
  let finally () =
    close_out tmp_out_channel;
    close_out out_channel
  in
  try
    Fun.protect ~finally (fun () ->
        let ins =
          match mode with
          | "negative" -> Negative.generate ()
          | _ ->
              eprintf
                "Invalid argument: %s@.Usage: fuzz mode=negative [-o output]@."
                mode;
              exit 1
        in
        let () = Ast.pp_instance tmp_formatter ins in
        let _ = Sys.command (String.concat " " [ "is"; tmp ]) in
        ())
  with e ->
    eprintf "exception: %s\n@." (Printexc.to_string e);
    exit 1
