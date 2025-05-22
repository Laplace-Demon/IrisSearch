  $ is ./diseq2 --show-path
  path
  
  locals
      %empty
  atoms
      □ (P a1 a2)
  pures
      %empty
  
  ↓
  
  locals
      %empty
  atoms
      □ (P a1 a2)
  pures
      a1 = a2
  
  Unsat core:
      (and (= a1 a2))
      (and (forall ((a A) (b A)) Per-1) (distinct a1 a2))
  
  find solution
  
