  $ is ./gen_heap2 --show-path
  
    path
  
  state 0
  
  locals
      %empty
  atoms
      pointsto l2 q2 v2
      pointsto l1 q1 v1
      pointsto l3 q3 v3
      pointsto l4 q4 v4
  pures
      l1 = l2
      v1 = Loc(l3)
      v2 = Loc(l4)
      v3 ≠ v4
  
    ↓ Applying law (forall (l1 l2 : loc) (dq1 dq2 : dfrac) (v1 v2 : val), pointsto l2 dq2 v2 * pointsto l1 dq1 v1 * ⌜ l1 = l2 ⌝ -* ⌜ v1 = v2 ⌝).
  
  state 1
  
  locals
      %empty
  atoms
      pointsto l3 q3 v3
      pointsto l4 q4 v4
  pures
      l1 = l2
      v1 = v2
      v1 = Loc(l3)
      v2 = Loc(l4)
      v3 ≠ v4
  
    ↓ Applying law (forall (l1 l2 : loc) (dq1 dq2 : dfrac) (v1 v2 : val), pointsto l2 dq2 v2 * pointsto l1 dq1 v1 * ⌜ l1 = l2 ⌝ -* ⌜ v1 = v2 ⌝).
  
  state 3
  
  locals
      %empty
  atoms
      %empty
  pures
      l1 = l2
      v1 = v2
      v1 = Loc(l3)
      v2 = Loc(l4)
      v4 = v3
      v3 ≠ v4
  
  Unsat core:
      (and (= l1 l2) (= v1 (Loc l3)) (= v2 (Loc l4)) (distinct v3 v4))
      (and (= v1 v2) (= v4 v3))
  
    find refutation
  
