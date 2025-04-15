  $ is ./persistent3
  original instance
  
  types
      
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
  
  types
      
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
      %empty
  
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
      □ A
      □ B
  
  ↓
  
  pures
      %empty
  props
      ⊥
      □ A
      □ B
  
  find solution
  
  statistics
      generated state count: 1
      visited state count: 2
      maximum search depth: 1
      duplication count: 0
      operations count:
              Multiset.union: 8
              Multiset.diff: 1
