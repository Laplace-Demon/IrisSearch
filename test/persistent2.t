  $ is ./persistent2
  original instance
  
  types
      
  preds
      
  consts
      A : iProp
      B : iProp
      C : iProp
  facts
      Persistent A
      Persistent B
  laws
      □ (A * A * C * C * C * C * A * A * B * B * A * A * A * A * C * B * A * C * A * B * A * C * C * C * A * B * C * A * B * B * C * B * A * B * A -* ⊥)
      □ (B -* A)
      □ (A -* C)
  init
      B
  
  transformed instance
  
  types
      
  preds
      
  consts
      A : iProp
      B : iProp
      C : iProp
  facts
      Persistent □ A
      Persistent □ B
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
      %empty
  
  initial state
  
  pures
      %empty
  props
      □ B
  
  no solution
  
  statistics
      generated state count: 11
      visited state count: 11
      maximum search depth: 10
      duplication count: 10
      operations count:
              Set.subset: 65
              Multiset.union: 59
              Multiset.subset: 65
              Multiset.diff: 33
