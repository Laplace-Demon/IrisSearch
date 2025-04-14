  $ is ./simple2
  instance
  
  consts
      A : iProp
      B : iProp
      C : iProp
      D : iProp
      E : iProp
      F : iProp
  laws
      □ (A * B -* C * D)
      □ (C -* E)
      □ (D -* F)
      □ (F * E -* ⊥)
  init
      A * B
  
  global state
  
  pures
      %empty
  props
      □ (C -* E)
      □ (D -* F)
      □ ((A * B) -* (C * D))
      □ ((E * F) -* ⊥)
  
  initial state
  
  pures
      %empty
  props
      A
      B
  
  path
  
  pures
      %empty
  props
      ⊥
  
  ↑
  
  pures
      %empty
  props
      E
      F
  
  ↑
  
  pures
      %empty
  props
      D
      E
  
  ↑
  
  pures
      %empty
  props
      C
      D
  
  ↑
  
  pures
      %empty
  props
      A
      B
  
  find solution
  
  statistics
      state count: 5
      maximum search depth: 4
      duplication count: 1
