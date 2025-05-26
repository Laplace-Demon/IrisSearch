open State
open Format

type 'a path = Path of 'a list * 'a path list

let pp_state_path : formatter -> state path -> unit = fun fmt path -> ()
