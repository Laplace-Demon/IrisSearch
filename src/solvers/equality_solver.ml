open Internal
open State
open UnionFind

let solve : state -> bool =
 fun { pr_set; _ } ->
  let knowledge = PropSet.union global_state.facts pr_set in
  let term_ref_table = Hashtbl.create 17 in
  let get_ref tm =
    match Hashtbl.find_opt term_ref_table tm with
    | Some ref -> ref
    | None ->
        let ref = make () in
        Hashtbl.add term_ref_table tm ref;
        ref
  in
  let () =
    PropSet.iter
      (function
        | IEq (tm1, tm2) ->
            let _ = union (get_ref tm1) (get_ref tm2) in
            ()
        | _ -> ())
      knowledge
  in
  not
    (PropSet.exists
       (fun pr ->
         match pr with
         | INeq (tm1, tm2) -> eq (get_ref tm1) (get_ref tm2)
         | _ -> false)
       knowledge)
