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
      (and (distinct a1 a2))
      (and (= a1 a2))
  
  find solution
  
