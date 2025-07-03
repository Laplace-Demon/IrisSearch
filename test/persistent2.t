  $ is ./persistent2 --show-path
  
    path
  
  state 0
  
  locals
      %empty
  atoms
      □ B
  pures
      %empty
  
    ↓ Applying law (B -* A).
  
  state 1
  
  locals
      %empty
  atoms
      □ A
      □ B
  pures
      %empty
  
    ↓ Applying law (A -* C).
  
  state 2
  
  locals
      %empty
  atoms
      □ A
      □ B
      □ C
  pures
      %empty
  
  Applying law (A * A * A * A * A * A * A * A * A * A * A * A * A * A * A * B * B * B * B * B * B * B * B * B * C * C * C * C * C * C * C * C * C * C * C -* ⊥) yields False.
  
    find refutation
  
