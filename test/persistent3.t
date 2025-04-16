  $ is ./persistent3
  original instance
  
  types
      
  preds
      
  consts
      A : iProp
      B : iProp
  facts
      Persistent A
      Persistent A
      Persistent A
      Persistent B
      Persistent B
      Persistent B
      Persistent B
  laws
      □ (A * A * A * B * B -* ⊥)
  init
      A
      B
  
  transformed instance
  
  types
      
  preds
      
  consts
      A : iProp
      B : iProp
  facts
      Persistent □ □ □ A
      Persistent □ □ □ A
      Persistent □ □ □ A
      Persistent □ □ □ □ B
      Persistent □ □ □ □ B
      Persistent □ □ □ □ B
      Persistent □ □ □ □ B
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
