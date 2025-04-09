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
  | '('                   { LPAREN }
  | ')'                   { RPAREN }
  | ')'                   { RPAREN }
  | ':'                   { COLON }
  | eof                   { EOF }

  | "consts"              { DECL_CONSTS }
  | "laws"                { DECL_LAWS }
  | "init"                { DECL_INIT }

  | "Prop"                { TYPE_PROP }
  | "iProp"               { TYPE_IPROP }

  | "Persistent"          { PERSISTENT }
  | "Exclusive"           { EXCLUSIVE }

  | "False"               { FALSE }
  | ident as id           { IDENT id }
  | '*'                   { STAR }
  | "-*"                  { WAND }
  | "□"                   { BOX }
  | _ as c                { raise (Lexing_error (sprintf "Unknown character: %c" c)) }

{
}
