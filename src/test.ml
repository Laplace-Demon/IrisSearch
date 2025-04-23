open Format
open Is
open Is.Ast
open Is.Internal
open Is.Internal_operations
open Is.Duplication_checker
open Is.State

let () = Printexc.record_backtrace true

open Monads.ListMonad

let apply ipr (pr_set, ipr_mset) =
  match ipr with
  | IHForall ({ shift }, IWand (ipr1, ipr2)) -> (
      match ipr1 with
      | IStar ipr_set -> []
      | _ -> (
          printf "%a@." pp_internal_iprop ipr1;
          let* match_result =
            internal_iprop_match_multiple shift ipr1 ipr_mset
          in
          printf "@.match result:@.%a@."
            (pp_print_list (fun fmt (i, tm) ->
                 fprintf fmt "%i:%a" i pp_internal_term tm))
            match_result;
          let subst_ipr1 = subst_internal_iprop match_result ipr1 in
          let prems =
            match subst_ipr1 with
            | IStar ipr_mset -> ipr_mset
            | _ -> IpropMset.singleton subst_ipr1 Multiplicity.one
          in
          try
            let ipr_mset_prems_elim = IpropMset.diff ipr_mset prems in
            let subst_ipr2 = subst_internal_iprop match_result ipr2 in
            let concls =
              match subst_ipr2 with
              | IStar ipr_mset -> ipr_mset
              | _ -> IpropMset.singleton subst_ipr2 Multiplicity.one
            in
            let new_ipr_mset = IpropMset.union concls ipr_mset_prems_elim in
            let new_st = (pr_set, new_ipr_mset) in
            if is_duplicate new_st then [] else return new_st
          with Multiplicity.Underflow -> assert false))
  | _ -> []

let apply_multiple ipr count (pr_set, ipr_mset) =
  match ipr with
  | IHForall ({ shift }, IWand (ipr1, ipr2)) -> (
      match ipr1 with
      | IStar ipr_set -> []
      | _ -> (
          let* match_result =
            internal_iprop_match_multiple shift ipr1 ipr_mset
          in
          printf "@.match result:@.%a@."
            (pp_print_list (fun fmt (i, tm) ->
                 fprintf fmt "%i:%a" i pp_internal_term tm))
            match_result;
          let subst_ipr1 = subst_internal_iprop match_result ipr1 in
          let prems =
            match subst_ipr1 with
            | IStar ipr_mset -> ipr_mset
            | _ -> IpropMset.singleton subst_ipr1 Multiplicity.one
          in
          try
            let factor =
              Multiplicity.min (IpropMset.factor ipr_mset prems) count
            in
            let ipr_mset_prems_elim =
              IpropMset.diff_multiple factor ipr_mset prems
            in
            let subst_ipr2 = subst_internal_iprop match_result ipr2 in
            let concls =
              match subst_ipr2 with
              | IStar ipr_mset -> ipr_mset
              | _ -> IpropMset.singleton subst_ipr2 Multiplicity.one
            in
            let dup_concls =
              IpropMset.map_multiplicity
                (fun _ count -> Multiplicity.mul count factor)
                concls
            in
            let new_ipr_mset = IpropMset.union dup_concls ipr_mset_prems_elim in
            let new_st = (pr_set, new_ipr_mset) in
            if is_duplicate new_st then [] else return new_st
          with Multiplicity.Underflow -> assert false))
  | _ -> []

module Example1 = struct
  let law =
    HForall
      ( [ ("l", Tcustom "loc"); ("v", Tcustom "val") ],
        Wand
          ( HPred ("pointsto", [ Var "l"; Var "v" ]),
            HPred ("bepointedby", [ Var "v"; Var "l" ]) ) )
    |> iprop_to_internal

  let ipr1 = HPred ("pointsto", [ Var "l1"; Var "v1" ])
  let ipr2 = HPred ("pointsto", [ Var "l2"; Var "v2" ])
  let ipr3 = HPred ("pointsto", [ Var "l3"; Var "v3" ])
  let ipr4 = False
  let ipr5 = HPred ("erwtwre", [ Var "dsaf"; Var "qf" ])
  let ipr6 = HPred ("zzdwe", [ Var "ht"; Var "nfe" ])
  let ipr_list = [ ipr1; ipr2; ipr3; ipr4; ipr5; ipr6 ]
  let ipr_mset = iprop_list_to_internal ipr_list
  let state = (PropSet.empty, ipr_mset)

  let show () =
    printf "Example1@.";
    let* apply_result = apply law state in
    let () = printf "@.%a@.@." pp_state apply_result in
    []
end

module Example2 = struct
  let law =
    HForall
      ( [ ("l", Tcustom "loc"); ("v", Tcustom "val") ],
        Wand
          ( HPred ("pointsto", [ Var "l"; Var "v" ]),
            HPred ("bepointedby", [ Var "v"; Var "l" ]) ) )
    |> iprop_to_internal

  let ipr1 = HPred ("pointsto", [ Var "l1"; Var "v1" ])
  let ipr2 = HPred ("pointsto", [ Var "l2"; Var "v2" ])
  let ipr3 = HPred ("pointsto", [ Var "l3"; Var "v3" ])
  let ipr4 = False
  let ipr5 = HPred ("erwtwre", [ Var "dsaf"; Var "qf" ])
  let ipr6 = HPred ("zzdwe", [ Var "ht"; Var "nfe" ])
  let ipr_list = [ ipr1; ipr2; ipr3; ipr4; ipr5; ipr6; ipr1 ]
  let ipr_mset = iprop_list_to_internal ipr_list
  let state = (PropSet.empty, ipr_mset)

  let show () =
    printf "Example2@.";
    let* apply_result = apply_multiple law Multiplicity.inf state in
    let () = printf "@.%a@.@." pp_state apply_result in
    []
end

module Example3 = struct
  let law =
    HForall
      ( [ ("l", Tcustom "loc"); ("v", Tcustom "val") ],
        Wand
          ( HPred ("pointsto", [ Var "l"; Var "v" ]),
            HPred ("bepointedby", [ Var "v"; Var "l" ]) ) )
    |> iprop_to_internal

  let ipr1 = Box (HPred ("pointsto", [ Var "l1"; Var "v1" ]))
  let ipr2 = HPred ("pointsto", [ Var "l2"; Var "v2" ])
  let ipr3 = HPred ("pointsto", [ Var "l3"; Var "v3" ])
  let ipr4 = False
  let ipr5 = HPred ("erwtwre", [ Var "dsaf"; Var "qf" ])
  let ipr6 = HPred ("zzdwe", [ Var "ht"; Var "nfe" ])
  let ipr_list = [ ipr1; ipr2; ipr3; ipr4; ipr5; ipr6 ]
  let ipr_mset = iprop_list_to_internal ipr_list
  let state = (PropSet.empty, ipr_mset)

  let show () =
    printf "Example3@.";
    let* apply_result = apply_multiple law Multiplicity.inf state in
    let () = printf "@.%a@.@." pp_state apply_result in
    []
end

module Example4 = struct
  let law =
    HForall
      ( [
          ("l", Tcustom "loc");
          ("bar", Tcustom "bar");
          ("v", Tcustom "val");
          ("foo", Tcustom "foo");
        ],
        Wand
          ( HPred ("pointsto", [ Var "l"; Var "v" ]),
            HPred ("bepointedby", [ Var "v"; Var "l" ]) ) )
    |> iprop_to_internal

  let ipr1 = HPred ("pointsto", [ Var "l1"; Var "v1" ])
  let ipr2 = HPred ("pointsto", [ Var "l2"; Var "v2" ])
  let ipr3 = HPred ("pointsto", [ Var "l3"; Var "v3" ])
  let ipr4 = False
  let ipr5 = HPred ("erwtwre", [ Var "dsaf"; Var "qf" ])
  let ipr6 = HPred ("zzdwe", [ Var "ht"; Var "nfe" ])
  let ipr_list = [ ipr1; ipr2; ipr3; ipr4; ipr5; ipr6 ]
  let ipr_mset = iprop_list_to_internal ipr_list
  let state = (PropSet.empty, ipr_mset)

  let show () =
    printf "Example4@.";
    let* apply_result = apply law state in
    let () = printf "@.%a@.@." pp_state apply_result in
    []
end

module Example5 = struct
  let ipr1 = HPred ("eq", [ Var "a"; Var "b" ])
  let ipr2 = HPred ("eq", [ Var "b"; Var "d" ])

  let ipr3 =
    HForall ([ ("i", Tcustom "int") ], HPred ("eq", [ Var "i"; Var "i" ]))

  let ipr4 =
    HForall
      ( [ ("a", Tcustom "int"); ("b", Tcustom "int") ],
        HPred ("eq", [ Var "a"; Var "b" ]) )

  let ipr5 = HPred ("bad", [ Var "5" ])
  let ipr6 = HForall ([ ("i", Tcustom "int") ], HPred ("bad", [ Var "5" ]))

  let ipr7 =
    HForall
      ( [ ("a", Tcustom "int"); ("b", Tcustom "int") ],
        HPred ("bad", [ Var "6" ]) )

  let ipr8 =
    HForall
      ( [ ("a", Tcustom "int"); ("b", Tcustom "int") ],
        HPred ("bad", [ Var "4" ]) )

  let ipr9 = HPred ("apple", [ Var "sad" ])
  let ipr10 = HPred ("eqqq", [ Var "bad" ])
  let ipr11 = HForall ([ ("eqq", Tcustom "int") ], HPred ("eqq", [ Var "bad" ]))
  let ipr12 = HForall ([ ("_", Tcustom "int") ], HPred ("eqq", [ Var "sadaw" ]))

  let ipr_list =
    [
      ipr9;
      ipr8;
      ipr9;
      ipr10;
      ipr1;
      ipr11;
      ipr12;
      ipr7;
      ipr2;
      ipr3;
      ipr4;
      ipr5;
      ipr6;
    ]

  let ipr_mset = iprop_list_to_internal ipr_list
  let state = (PropSet.empty, ipr_mset)

  let law =
    HForall
      ( [ ("i1", Tcustom "int"); ("i2", Tcustom "int") ],
        Wand
          (HPred ("eq", [ Var "i1"; Var "i2" ]), Pure (Eq (Var "i1", Var "i2")))
      )
    |> iprop_to_internal

  let show () =
    printf "Example5@.";
    let* apply_result = apply law state in
    let () = printf "@.%a@.@." pp_state apply_result in
    []
end

let _ = Example5.show ()
