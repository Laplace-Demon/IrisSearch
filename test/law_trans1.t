  $ is ./law_trans1 --show-path
  path
  
  locals
      %empty
  atoms
      □ (P b d)
  pures
      %empty
  
    ↓ applying law forall t : T, (P t t -* Dup t)
  
  locals
      %empty
  atoms
      □ (P b d)
      □ (Dup b)
  pures
      %empty
  
  Applying law
      (Dup a -* ⊥)
  yields False.
  
  find solution
  
