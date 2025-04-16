  $ is ./type1
  original instance
  
  types
      loc
      value
      int
      expr
  preds
      
  consts
      A : loc
      B : value
      C : int
      D : expr
      E : Prop
      F : iProp
  facts
      
  laws
      □ (F -* ⊥)
  init
      F
  
  transformed instance
  
  types
      loc
      value
      int
      expr
  preds
      
  consts
      A : loc
      B : value
      C : int
      D : expr
      E : Prop
      F : iProp
  facts
      
  laws
      □ (F -* ⊥)
  init
      F
  
  global state
  
  pures
      %empty
  props
      %empty
  
  initial state
  
  pures
      %empty
  props
      F
  
  path
  
  pures
      %empty
  props
      F
  
  ↓
  
  pures
      %empty
  props
      ⊥
  
  find solution
  
  statistics
      generated state count: 1
      visited state count: 2
      maximum search depth: 1
      duplication count: 0
      operations count:
              Multiset.union: 3
              Multiset.diff: 1
  $ is ./type2
  original instance
  
  types
      loc
  preds
      
  consts
      A : loc
      B : int
  facts
      
  laws
      
  init
      ⊥
  
  transformed instance
  
  types
      loc
  preds
      
  consts
      A : loc
      B : int
  facts
      
  laws
      
  init
      ⊥
  
  global state
  
  pures
      %empty
  props
      %empty
  
  statistics
      generated state count: 0
      visited state count: 0
      maximum search depth: 0
      duplication count: 0
      operations count:
              
  validation error: missing type declaration of int
  [1]
  $ is ./type3
  original instance
  
  types
      loc
      loc
  preds
      
  consts
      
  facts
      
  laws
      
  init
      ⊥
  
  transformed instance
  
  types
      loc
      loc
  preds
      
  consts
      
  facts
      
  laws
      
  init
      ⊥
  
  global state
  
  pures
      %empty
  props
      %empty
  
  statistics
      generated state count: 0
      visited state count: 0
      maximum search depth: 0
      duplication count: 0
      operations count:
              
  validation error: duplicate type declaration of loc
  [1]
  $ is ./type4
  original instance
  
  types
      loc
  preds
      
  consts
      A : loc
  facts
      
  laws
      □ (A -* ⊥)
  init
      A
  
  transformed instance
  
  types
      loc
  preds
      
  consts
      A : loc
  facts
      
  laws
      □ (A -* ⊥)
  init
      A
  
  global state
  
  pures
      %empty
  props
      %empty
  
  statistics
      generated state count: 0
      visited state count: 0
      maximum search depth: 0
      duplication count: 0
      operations count:
              
  validation error: A should have type iProp, but has type loc
  [1]
