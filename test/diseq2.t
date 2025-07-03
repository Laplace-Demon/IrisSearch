  $ is ./diseq2 --show-path
  
    path
  
  state 0
  
  locals
      %empty
  atoms
      □ (P a1 a2)
  pures
      a1 ≠ a2
  
    ↓ Applying law (forall x y : A, P x y -* ⌜ x = y ⌝).
  
  state 1
  
  locals
      %empty
  atoms
      □ (P a1 a2)
  pures
      a1 = a2
      a1 ≠ a2
  
  Unsat core:
      (and (distinct a1 a2))
      (and (= a1 a2))
  
    find refutation
  
