  $ is ./diseq1 --show-path
  
    path
  
  locals
      %empty
  atoms
      P a
      P b
  pures
      c = b
      a ≠ c
  
  Applying law (forall x y : T, P y * P x * ⌜ x ≠ y ⌝ -* ⊥) yields False.
  
    find refutation
  
