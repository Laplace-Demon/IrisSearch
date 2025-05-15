open Z3
open Format

let ctx = mk_context []

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
  ()

let () =
  let s = Arithmetic.Integer.mk_sort ctx in
  let v0 = Quantifier.mk_bound ctx 0 s in
  let v1 = Quantifier.mk_bound ctx 1 s in
  let eq_x_y = Boolean.mk_eq ctx v1 v0 in
  let eq_x_x =
    Boolean.mk_eq ctx
      (Quantifier.mk_bound ctx 0 s)
      (Quantifier.mk_bound ctx 0 s)
  in
  let inner =
    Quantifier.expr_of_quantifier
      (Quantifier.mk_forall ctx [ s ]
         [ Symbol.mk_string ctx "y" ]
         eq_x_y None [] [] None None)
  in
  let inner2 = Boolean.mk_and ctx [ eq_x_x; inner ] in
  let outer =
    Quantifier.expr_of_quantifier
      (Quantifier.mk_forall ctx [ s ]
         [ Symbol.mk_string ctx "x" ]
         inner2 None [] [] None None)
  in
  printf "%s" (Expr.to_string outer)
