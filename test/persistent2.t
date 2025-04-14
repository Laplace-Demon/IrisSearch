  $ is ./persistent2
  original instance
  
  consts
      A : iProp
      B : iProp
      C : iProp
  laws
      □ (A * A * C * C * C * C * A * A * B * B * A * A * A * A * C * B * A * C * A * B * A * C * C * C * A * B * C * A * B * B * C * B * A * B * A -* ⊥)
      ⌜Persistent A⌝
      ⌜Persistent B⌝
      □ (B -* A)
      □ (A -* C)
  init
      B
  
  transformed instance
  
  consts
      A : iProp
      B : iProp
      C : iProp
  laws
      □ (A * A * C * C * C * C * A * A * B * B * A * A * A * A * C * B * A * C * A * B * A * C * C * C * A * B * C * A * B * B * C * B * A * B * A -* ⊥)
      □ (B -* □ A)
      □ (A -* C)
  init
      □ B
  
  global state
  
  pures
      %empty
  props
      □ (A -* C)
      □ (B -* (□ A))
      □ ((A * A * A * A * A * A * A * A * A * A * A * A * A * A * A * B * B * B * B * B * B * B * B * B * C * C * C * C * C * C * C * C * C * C * C) -* ⊥)
  
  initial state
  
  pures
      %empty
  props
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
      C
      C
      C
      C
      C
      C
      C
      C
      C
      C
      C
  
  ↑
  
  pures
      %empty
  props
      □ A
      □ B
      C
      C
      C
      C
      C
      C
      C
      C
      C
      C
  
  ↑
  
  pures
      %empty
  props
      □ A
      □ B
      C
      C
      C
      C
      C
      C
      C
      C
      C
  
  ↑
  
  pures
      %empty
  props
      □ A
      □ B
      C
      C
      C
      C
      C
      C
      C
      C
  
  ↑
  
  pures
      %empty
  props
      □ A
      □ B
      C
      C
      C
      C
      C
      C
      C
  
  ↑
  
  pures
      %empty
  props
      □ A
      □ B
      C
      C
      C
      C
      C
      C
  
  ↑
  
  pures
      %empty
  props
      □ A
      □ B
      C
      C
      C
      C
      C
  
  ↑
  
  pures
      %empty
  props
      □ A
      □ B
      C
      C
      C
      C
  
  ↑
  
  pures
      %empty
  props
      □ A
      □ B
      C
      C
      C
  
  ↑
  
  pures
      %empty
  props
      □ A
      □ B
      C
      C
  
  ↑
  
  pures
      %empty
  props
      □ A
      □ B
      C
  
  ↑
  
  pures
      %empty
  props
      □ A
      □ B
  
  ↑
  
  pures
      %empty
  props
      □ B
  
  find solution
  
  statistics
      state count: 14
      maximum search depth: 13
      duplication count: 12
