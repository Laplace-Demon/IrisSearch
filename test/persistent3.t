  $ is ./persistent3
  original instance
  
  consts
      A : iProp
      B : iProp
  laws
      □ (A * A * A * B * B -* ⊥)
      ⌜Persistent A⌝
      ⌜Persistent A⌝
      ⌜Persistent A⌝
      ⌜Persistent B⌝
      ⌜Persistent B⌝
      ⌜Persistent B⌝
      ⌜Persistent B⌝
  init
      A
      B
  
  transformed instance
  
  consts
      A : iProp
      B : iProp
  laws
      □ (A * A * A * B * B -* ⊥)
  init
      □ □ □ A
      □ □ □ □ B
  
  global state
  
  pures
      %empty
  props
      □ ((A * A * A * B * B) -* ⊥)
  
  initial state
  
  pures
      %empty
  props
      □ A
      □ B
  
  path
  
  pures
      %empty
  props
      ⊥
      □ A
      □ B
  
  ↑
  
  pures
      %empty
  props
      □ A
      □ B
  
  find solution
  
  statistics
      state count: 1
      maximum search depth: 1
      duplication count: 0
