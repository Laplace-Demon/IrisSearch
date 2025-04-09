open Format
open Lexing
open Parser
open Ast
open State
open Search

let file =
  let file = ref None in
  let set_file s = file := Some s in
  Arg.parse [] set_file "main input";
  match !file with Some f -> f | None -> exit 1

let () =
  let in_channel = open_in file in
  let lexbuf = Lexing.from_channel in_channel in
  try
    let ins = Parser.instance Lexer.token lexbuf in
    close_in in_channel;
    let module G = struct
      type node = state
      let source = initial ins
      let successors = successors
      let terminate = terminate
      let estimate = fun _ -> 0
    end in
    let open Search.Make (G) in
    match search (fun _ -> ()) with
    | Some path ->
        pp_print_list
          ~pp_sep:(fun fmt () -> fprintf fmt "↑\n\n")
          pp_state std_formatter path
    | None -> printf "no\n"
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
