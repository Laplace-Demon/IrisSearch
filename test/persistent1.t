  $ is ./persistent1
  original instance
  
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
  
  transformed instance
  
  consts
      A : iProp
      W : iProp
  laws
      □ (A * A -* ⊥)
      □ (A -* □ W)
      □ (W -* A)
  init
      A
  
  global state
  
  pures
      %empty
  props
      □ (A -* (□ W))
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
      A
  
  ↓
  
  pures
      %empty
  props
      □ W
  
  ↓
  
  pures
      %empty
  props
      A
      □ W
  
  ↓
  
  pures
      %empty
  props
      A
      A
      □ W
  
  ↓
  
  pures
      %empty
  props
      ⊥
      □ W
  
  find solution
  
  statistics
      state count: 5
      maximum search depth: 4
      duplication count: 2
