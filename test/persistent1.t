  $ is ./persistent1
  original instance
  
  types
      
  preds
      
  consts
      A : iProp
      W : iProp
  facts
      Persistent W
  laws
      □ (A * A -* ⊥)
      □ (A -* W)
      □ (W -* A)
  init
      A
  
  transformed instance
  
  types
      
  preds
      
  consts
      A : iProp
      W : iProp
  facts
      Persistent □ W
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
      %empty
  
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
      generated state count: 5
      visited state count: 5
      maximum search depth: 4
      duplication count: 2
      operations count:
              Set.subset: 12
              Multiset.union: 12
              Multiset.subset: 12
              Multiset.diff: 12
