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
  
  $ is ./quant1 --until-parsing --show-instance
  original instance
  
  types
      loc
  laws
      □ forall l1 l2 : loc, ⊥
  init
      %empty
  
  
  Parsing succeeds.
  
  $ is ./quant2 --until-parsing --show-instance
  original instance
  
  types
      loc
      val
      dfrac
  preds
      pointsto : loc * dfrac * val -> iProp
  consts
      Discarded : dfrac
  laws
      □ forall (l : loc) (dq1 dq2 : dfrac) (v1 v2 : val), (pointsto l dq1 v1 -* (pointsto l dq2 v2 -* ⌜ v1 = v2 ⌝)),
      □ forall (l : loc) (dq : dfrac) (v : val), (pointsto l dq v -* pointsto l Discarded v)
  init
      %empty
  
  
  Parsing succeeds.
  
  $ is ./quant3 --until-parsing --show-instance
  original instance
  
  types
      loc
      val
      dfrac
  preds
      pointsto : loc * dfrac * val -> iProp
  consts
      Discarded : dfrac
  laws
      □ forall l : loc, forall dq1 : dfrac, forall dq2 : dfrac, forall v1 : val, forall v2 : val, (pointsto l dq1 v1 -* (pointsto l dq2 v2 -* ⌜ v1 = v2 ⌝))
  init
      %empty
  
  
  Parsing succeeds.
  
