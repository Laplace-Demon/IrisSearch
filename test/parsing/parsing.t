  $ is ./section1 --until-parsing
  
  Parsing succeeds.
  
  $ is ./section2 --until-parsing
  
  Parsing succeeds.
  
  $ is ./types1 --until-parsing
  
  Parsing succeeds.
  
  $ is ./types2 --until-parsing
  validation error: duplicate type declaration of iProp
  [1]
  $ is ./preds1 --until-parsing
  
  Parsing succeeds.
  
  $ is ./preds2 --until-parsing
  
  Parsing succeeds.
  
  $ is ./preds3 --until-parsing
  ./preds3:2:20: parsing error
  [1]
  $ is ./preds4 --until-parsing
  
  Parsing succeeds.
  
  $ is ./consts1 --until-parsing
  
  Parsing succeeds.
  
  $ is ./consts2 --until-parsing
  ./consts2:2:10: parsing error
  [1]
  $ is ./facts1 --until-parsing
  
  Parsing succeeds.
  
  $ is ./facts2 --until-parsing
  
  Parsing succeeds.
  
  $ is ./facts3 --until-parsing --show-instance
  original instance
  
  facts
      Persistent A ∧ Persistent B ∨ Persistent C ∧ Persistent D ∨ Persistent E → ¬ Persistent A
  init
      %empty
  
  
  Parsing succeeds.
  
  $ is ./facts4 --until-parsing
  ./facts4:3:15: parsing error
  [1]
  $ is ./laws1 --until-parsing --show-instance
  original instance
  
  laws
      □ (A * B * C * D -* □ ⌜ Persistent E ∧ iris search ⌝),
      □ (A * B * C * D -* □ ⌜ Persistent E ∧ iris search ⌝)
  init
      %empty
  
  
  Parsing succeeds.
  
  $ is ./laws2 --until-parsing --show-instance
  original instance
  
  laws
      □ A A
  init
      %empty
  
  
  Parsing succeeds.
  
  $ is ./laws3 --until-parsing --show-instance
  original instance
  
  laws
      □ (A -* (A -* (A -* A)))
  init
      %empty
  
  
  Parsing succeeds.
  
