  $ is ./simple3
  instance
  
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
      ⊥
  
  ↑
  
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
  
  ↑
  
  pures
      %empty
  props
      A
      A
      A
      A
      A
      A
  
  ↑
  
  pures
      %empty
  props
      A
      A
      A
      A
      A
  
  ↑
  
  pures
      %empty
  props
      A
      A
      A
      A
  
  ↑
  
  pures
      %empty
  props
      A
      A
      A
  
  ↑
  
  pures
      %empty
  props
      A
      A
  
  ↑
  
  pures
      %empty
  props
      A
  
  find solution
  
  statistics
      state count: 10
      maximum search depth: 8
      duplication count: 0
