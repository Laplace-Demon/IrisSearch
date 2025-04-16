{
open Format

open Lexing
open Parser

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

exception Lexing_error of string
}

let digit = [ '0' - '9' ]
let letter = [ 'a' - 'z' 'A' - 'Z' '_' ]
let ident = letter (letter | digit)*

rule token = parse
  | [' ' '\t']            { token lexbuf }
  | ['\n']                { new_line lexbuf; token lexbuf }
  | '('                   { LPAREN }
  | ')'                   { RPAREN }
  | ':'                   { COLON }
  | ','                   { COMMA }
  | eof                   { EOF }

  | "types"               { DECL_TYPES }
  | "preds"               { DECL_PREDS }
  | "consts"              { DECL_CONSTS }
  | "facts"               { DECL_FACTS }
  | "laws"                { DECL_LAWS }
  | "init"                { DECL_INIT }

  | "Prop"                { TYPE_PROP }
  | "iProp"               { TYPE_IPROP }

  | "Persistent"          { PERSISTENT }
  | "Exclusive"           { EXCLUSIVE }

  | "False"               { FALSE }
  | '*'                   { STAR }
  | "-*"                  { WAND }
  | "□"                   { BOX }
  | "⌜"                   { TOPLEFTCORNER }
  | "⌝"                   { TOPRIGHTCORNER }
  | "¬"                   { NOT }
  | "∧"                   { AND }
  | "∨"                   { OR }
  | "->"                  { ARROW }

  | ident as id           { IDENT id }
  | _ as c                { raise (Lexing_error (sprintf "Unknown character: %c" c)) }

{
}
