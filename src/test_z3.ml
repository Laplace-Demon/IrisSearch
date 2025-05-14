open Z3
open Format

let ctx = mk_context []

(* forward ref *)
let list_ref = Datatype.mk_sort_ref_s ctx "list"

(* constructors: Nil | Cons(head: Int, tail: list) *)
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
  let solver = Solver.mk_solver ctx None in
  Solver.add solver [ eq1; eq2 ];
  match Solver.check solver [] with
  | Solver.SATISFIABLE -> Printf.printf "SAT\n"
  | Solver.UNSATISFIABLE -> print_endline "UNSAT\n"
  | Solver.UNKNOWN -> print_endline "UNKNOWN\n"
