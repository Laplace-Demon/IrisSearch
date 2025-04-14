  $ is ./simple1
  original instance
  
  consts
      A : iProp
      B : iProp
      C : iProp
      D : iProp
  laws
      □ (A -* B)
      □ (B -* C)
      □ (C -* D)
      □ (D -* ⊥)
  init
      A
  
  transformed instance
  
  consts
      A : iProp
      B : iProp
      C : iProp
      D : iProp
  laws
      □ (A -* B)
      □ (B -* C)
      □ (C -* D)
      □ (D -* ⊥)
  init
      A
  
  global state
  
  pures
      %empty
  props
      □ (A -* B)
      □ (B -* C)
      □ (C -* D)
      □ (D -* ⊥)
  
  initial state
  
  pures
      %empty
  props
      A
  
  path
  
  pures
      %empty
  props
      A
  
  ↓
  
  pures
      %empty
  props
      B
  
  ↓
  
  pures
      %empty
  props
      C
  
  ↓
  
  pures
      %empty
  props
      D
  
  ↓
  
  pures
      %empty
  props
      ⊥
  
  find solution
  
  statistics
      state count: 4
      maximum search depth: 4
      duplication count: 0
