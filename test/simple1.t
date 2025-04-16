  $ is ./simple1
  original instance
  
  types
      
  preds
      
  consts
      A : iProp
      B : iProp
      C : iProp
      D : iProp
  facts
      
  laws
      □ (A -* B)
      □ (B -* C)
      □ (C -* D)
      □ (D -* ⊥)
  init
      A
  
  transformed instance
  
  types
      
  preds
      
  consts
      A : iProp
      B : iProp
      C : iProp
      D : iProp
  facts
      
  laws
      □ (A -* B)
      □ (B -* C)
      □ (C -* D)
      □ (D -* ⊥)
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
      B
  
  ↓
  
  pures
      %empty
  props
      C
  
  ↓
  
  pures
      %empty
  props
      D
  
  ↓
  
  pures
      %empty
  props
      ⊥
  
  find solution
  
  statistics
      generated state count: 4
      visited state count: 5
      maximum search depth: 4
      duplication count: 0
      operations count:
              Set.subset: 6
              Multiset.union: 9
              Multiset.subset: 6
              Multiset.diff: 16
