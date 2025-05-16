  $ is ./gen_heap1 --show-path
  path
  
  locals
      %empty
  atoms
      pointsto l2 q2 v2
      pointsto l1 q1 v1
      pointsto l3 q3 v3
      pointsto l4 q4 v4
  pures
      %empty
  
  ↓
  
  locals
      %empty
  atoms
      pointsto l3 q3 v3
      pointsto l4 q4 v4
  pures
      v1 = v2
  
  ↓
  
  locals
      %empty
  atoms
      %empty
  pures
      v1 = v2
      v4 = v3
  
  Unsat core:
      (and (= l1 l2) (= v1 (Loc l3)) (= v2 (Loc l4)) (distinct v3 v4))
      (and (= v1 v2) (= v4 v3))
  
  find solution
  
