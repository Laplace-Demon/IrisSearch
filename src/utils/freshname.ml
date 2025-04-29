let name_table : (string, int) Hashtbl.t = Hashtbl.create 17
let reset () = Hashtbl.reset name_table

let generate ?(base = "") () =
  match Hashtbl.find_opt name_table base with
  | Some index ->
      Hashtbl.replace name_table base (index + 1);
      String.concat "_" [ base; Int.to_string index ]
  | None ->
      Hashtbl.add name_table base 1;
      String.concat "_" [ base; Int.to_string 0 ]
