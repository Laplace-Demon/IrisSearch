  $ is ./simple2
  original instance
  
  consts
      A : iProp
      B : iProp
      C : iProp
      D : iProp
      E : iProp
      F : iProp
  laws
      □ (A * B -* C * D)
      □ (C -* E)
      □ (D -* F)
      □ (F * E -* ⊥)
  init
      A * B
  
  transformed instance
  
  consts
      A : iProp
      B : iProp
      C : iProp
      D : iProp
      E : iProp
      F : iProp
  laws
      □ (A * B -* C * D)
      □ (C -* E)
      □ (D -* F)
      □ (F * E -* ⊥)
  init
      A * B
  
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
      B
  
  path
  
  pures
      %empty
  props
      A
      B
  
  ↓
  
  pures
      %empty
  props
      C
      D
  
  ↓
  
  pures
      %empty
  props
      C
      F
  
  ↓
  
  pures
      %empty
  props
      E
      F
  
  ↓
  
  pures
      %empty
  props
      ⊥
  
  find solution
  
  statistics
      generated state count: 5
      visited state count: 6
      maximum search depth: 4
      duplication count: 1
      operations count:
              Set.subset: 11
              Multiset.union: 15
              Multiset.subset: 11
              Multiset.diff: 20
