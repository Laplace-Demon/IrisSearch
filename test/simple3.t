  $ is ./simple3
  original instance
  
  consts
      A : iProp
  laws
      □ (A -* A * A)
      □ (A * A * A * A * A * A * A -* ⊥)
  init
      A
  
  transformed instance
  
  consts
      A : iProp
  laws
      □ (A -* A * A)
      □ (A * A * A * A * A * A * A -* ⊥)
  init
      A
  
  global state
  
  pures
      %empty
  props
      □ (A -* (A * A))
      □ ((A * A * A * A * A * A * A) -* ⊥)
  
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
      A
      A
  
  ↓
  
  pures
      %empty
  props
      A
      A
      A
  
  ↓
  
  pures
      %empty
  props
      A
      A
      A
      A
  
  ↓
  
  pures
      %empty
  props
      A
      A
      A
      A
      A
  
  ↓
  
  pures
      %empty
  props
      A
      A
      A
      A
      A
      A
  
  ↓
  
  pures
      %empty
  props
      A
      A
      A
      A
      A
      A
      A
  
  ↓
  
  pures
      %empty
  props
      ⊥
  
  find solution
  
  statistics
      state count: 8
      maximum search depth: 7
      duplication count: 0
