open Z3
open Format

let ctx = mk_context []
let list_ref = Datatype.mk_sort_ref_s ctx "list"

let c_nil =
  Datatype.mk_constructor_s ctx "Nil" (Symbol.mk_string ctx "isNil") [] [] []

let c_cons =
  Datatype.mk_constructor ctx
    (Symbol.mk_string ctx "Cons")
    (Symbol.mk_string ctx "isCons")
    [ Symbol.mk_string ctx "head"; Symbol.mk_string ctx "tail" ]
    [ Some (Arithmetic.Integer.mk_sort ctx); None ]
    [ 0; 0 ]

let [ list_sort ] =
  Datatype.mk_sorts_s ctx
    [ "list" ] (* we’re defining one recursive datatype named “list” *)
    [ [ c_nil; c_cons ] ]

let c_nil_decl = Datatype.Constructor.get_constructor_decl c_nil
let c_cons_decl = Datatype.Constructor.get_constructor_decl c_cons

let l1 =
  Expr.mk_app ctx c_cons_decl
    [ Arithmetic.Integer.mk_numeral_i ctx 1; Expr.mk_const_s ctx "x" list_sort ]

let l2 =
  Expr.mk_app ctx c_cons_decl
    [ Arithmetic.Integer.mk_numeral_i ctx 1; Expr.mk_const_s ctx "y" list_sort ]

let eq1 = Boolean.mk_eq ctx l1 (Expr.mk_const_s ctx "y" list_sort)
let eq2 = Boolean.mk_eq ctx l2 (Expr.mk_const_s ctx "x" list_sort)

let () =
  let f1 =
    FuncDecl.mk_func_decl_s ctx "f"
      [ Arithmetic.Integer.mk_sort ctx ]
      (Arithmetic.Integer.mk_sort ctx)
  in
  let f2 =
    FuncDecl.mk_func_decl_s ctx "f"
      [ Arithmetic.Integer.mk_sort ctx ]
      (Arithmetic.Integer.mk_sort ctx)
  in
  let eq =
    Boolean.mk_eq ctx
      (Expr.mk_app ctx f1
         [ Expr.mk_const_s ctx "a" (Arithmetic.Integer.mk_sort ctx) ])
      (Expr.mk_app ctx f2
         [ Expr.mk_const_s ctx "b" (Arithmetic.Integer.mk_sort ctx) ])
  in
  let negeq = Boolean.mk_not ctx eq in
  let solver = Solver.mk_solver ctx None in
  let () = Solver.add solver [ negeq ] in
  let () =
    match Solver.check solver [] with
    | Solver.SATISFIABLE -> Printf.printf "FALSE\n"
    | Solver.UNSATISFIABLE -> print_endline "TRUE\n"
    | Solver.UNKNOWN -> print_endline "UNKNOWN\n"
  in
  ()
