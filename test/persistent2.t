  $ is ./persistent2 --show-path
  path
  
  locals
      %empty
  atoms
      □ B
  pures
      %empty
  
    ↓ applying law (B -* A)
  
  locals
      %empty
  atoms
      □ A
      □ B
  pures
      %empty
  
    ↓ applying law (A -* C)
  
  locals
      %empty
  atoms
      □ A
      □ B
      □ C
  pures
      %empty
  
  Applying law
      (A * A * A * A * A * A * A * A * A * A * A * A * A * A * A * B * B * B * B * B * B * B * B * B * C * C * C * C * C * C * C * C * C * C * C -* ⊥)
  yields False.
  
  find solution
  
