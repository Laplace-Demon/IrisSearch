open Format

open State
open Ast

let rec search (st : state) : bool =
  terminate st || List.exists search (transfer st) (* DFS *)

let rec search_and_print_path (st : state) : bool =
  if terminate st
  then (printf "%a\n\n" pp_state st; true)
  else
    if List.exists search_and_print_path (transfer st)
    then (printf "↑\n\n%a\n\n" pp_state st; true)
    else false
