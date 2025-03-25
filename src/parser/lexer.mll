{
open Format

open Lexing
open Parser

exception Lexing_error of string
}

let digit = [ '0' - '9' ]
let letter = [ 'a' - 'z' 'A' - 'Z' '_' ]
let atom = letter (letter | digit)*

rule token = parse
  | [' ' '\t' '\n']       { token lexbuf }
  | "False"               { FALSE }
  | atom as at            { ATOM at }
  | '*'                   { STAR }
  | "-*"                  { WAND }
  | '('                   { LPAREN }
  | ')'                   { RPAREN }
  | eof                   { EOF }
  | _ as c                { raise (Lexing_error (sprintf "Unknown character: %c" c)) }

{
}
