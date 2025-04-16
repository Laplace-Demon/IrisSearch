  $ is ./predicate2
  original instance
  
  types
      loc
      value
  preds
      pointsto : loc * value -> iProp
      not_pointsto : loc * value -> iProp
  consts
      loc1 : loc
      loc2 : loc
      value1 : value
      value2 : value
  facts
      
  laws
      □ (pointsto loc1 value1 -* pointsto loc1 value2)
      □ (pointsto loc1 value2 -* pointsto loc2 value1)
      □ (pointsto loc2 value1 -* pointsto loc2 value2)
      □ (pointsto loc2 value2 -* ⊥)
  init
      pointsto loc1 value1
  
  transformed instance
  
  types
      loc
      value
  preds
      pointsto : loc * value -> iProp
      not_pointsto : loc * value -> iProp
  consts
      loc1 : loc
      loc2 : loc
      value1 : value
      value2 : value
  facts
      
  laws
      □ (pointsto loc1 value1 -* pointsto loc1 value2)
      □ (pointsto loc1 value2 -* pointsto loc2 value1)
      □ (pointsto loc2 value1 -* pointsto loc2 value2)
      □ (pointsto loc2 value2 -* ⊥)
  init
      pointsto loc1 value1
  
  global state
  
  pures
      %empty
  props
      %empty
  
  initial state
  
  pures
      %empty
  props
      pointsto loc1 value1
  
  path
  
  pures
      %empty
  props
      pointsto loc1 value1
  
  ↓
  
  pures
      %empty
  props
      pointsto loc1 value2
  
  ↓
  
  pures
      %empty
  props
      pointsto loc2 value1
  
  ↓
  
  pures
      %empty
  props
      pointsto loc2 value2
  
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
