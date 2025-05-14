open Internal
open State
open UnionFind

let solve : internal_term -> internal_term -> internal_prop_set -> bool =
 fun tm1 tm2 pr_set ->
  let knowledge = PropSet.union !facts pr_set in
  compare_internal_term tm1 tm2 = 0
  ||
  let term_ref_table = Hashtbl.create 17 in
  let get_ref tm =
    match Hashtbl.find_opt term_ref_table tm with
    | Some ref -> ref
    | None ->
        let ref = make () in
        Hashtbl.add term_ref_table tm ref;
        ref
  in
  let ref_tm1 = get_ref tm1 in
  let ref_tm2 = get_ref tm2 in
  PropSet.iter
    (function
      | IEq (tm1, tm2) ->
          let _ = union (get_ref tm1) (get_ref tm2) in
          ()
      | _ -> ())
    knowledge;
  eq ref_tm1 ref_tm2
