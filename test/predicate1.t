  $ is ./predicate1
  original instance
  
  types
      loc
      value
  preds
      pointsto : loc * value -> iProp
  consts
      A : loc
      B : value
      C : iProp
  facts
      
  laws
      □ (C -* ⊥)
      □ (pointsto A B -* C)
  init
      pointsto A B
  
  transformed instance
  
  types
      loc
      value
  preds
      pointsto : loc * value -> iProp
  consts
      A : loc
      B : value
      C : iProp
  facts
      
  laws
      □ (C -* ⊥)
      □ (pointsto A B -* C)
  init
      pointsto A B
  
  global state
  
  pures
      %empty
  props
      %empty
  
  initial state
  
  pures
      %empty
  props
      pointsto A B
  
  path
  
  pures
      %empty
  props
      pointsto A B
  
  ↓
  
  pures
      %empty
  props
      C
  
  ↓
  
  pures
      %empty
  props
      ⊥
  
  find solution
  
  statistics
      generated state count: 2
      visited state count: 3
      maximum search depth: 2
      duplication count: 0
      operations count:
              Set.subset: 1
              Multiset.union: 5
              Multiset.subset: 1
              Multiset.diff: 4
