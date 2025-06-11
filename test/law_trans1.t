  $ is ./law_trans1 --show-path
  
    path
  
  locals
      %empty
  atoms
      □ (P b d)
  pures
      b = c
      a = b
      c = d
  
    ↓ Applying law (forall t : T, P t t -* Dup t).
  
  locals
      %empty
  atoms
      □ (P b d)
      □ (Dup b)
  pures
      b = c
      a = b
      c = d
  
  Applying law (Dup a -* ⊥) yields False.
  
    find refutation
  
