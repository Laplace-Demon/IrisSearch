  $ is ./persistent1
  instance
  
  consts
      A : iProp
      W : iProp
  laws
      □ (A * A -* ⊥)
      ⌜Persistent W⌝
      □ (A -* W)
      □ (W -* A)
  init
      A
  
  global state
  
  pures
      Persistent W
  props
      □ (A -* W)
      □ (W -* A)
      □ ((A * A) -* ⊥)
  
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
      □ W
  
  ↑
  
  pures
      %empty
  props
      A
      A
      □ W
  
  ↑
  
  pures
      %empty
  props
      A
      □ W
  
  ↑
  
  pures
      %empty
  props
      □ W
  
  ↑
  
  pures
      %empty
  props
      A
  
  find solution
  
  statistics
      state count: 7
      maximum search depth: 5
      duplication count: 3
