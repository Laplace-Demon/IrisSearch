{
open Format

open Lexing
open Parser

(* TODO: add lexing position *)

exception Lexing_error of string
}

let digit = [ '0' - '9' ]
let letter = [ 'a' - 'z' 'A' - 'Z' '_' ]
let ident = letter (letter | digit)*

rule token = parse
  | [' ' '\t' '\n']       { token lexbuf }
  | "Atom"                { ATOM_DECL }
  | "False"               { FALSE }
  | ident as id           { IDENT id }
  | '*'                   { STAR }
  | "-*"                  { WAND }
  | '('                   { LPAREN }
  | ')'                   { RPAREN }
  | ')'                   { RPAREN }
  | ':'                   { COLON }
  | ','                   { COMMA }
  | '%'                   { PERCENT }
  | eof                   { EOF }
  | _ as c                { raise (Lexing_error (sprintf "Unknown character: %c" c)) }

{
}
