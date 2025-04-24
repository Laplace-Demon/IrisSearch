  $ is ./uncurry --until-transformation --show-transformed-instance
  instance after uncurry_transformation
  
  types
      t
  consts
      a b : t
      A : iProp
  facts
      a = b ∧ a = b ∧ a = b → a = b,
      a = b ∧ a = b ∧ a = b → a = b ∧ a = b,
      a = b ∨ a = b ∧ a = b → a = b
  laws
      (A * A * A * A -* A),
      (A * (A * A * A -* A) -* ⊥),
      (A * ⌜ a = b ∧ a = b → a = b ⌝ -* ⊥)
  init
      %empty
  
  instance after merge_quantifier_transformation
  
  types
      t
  consts
      a b : t
      A : iProp
  facts
      a = b ∧ a = b ∧ a = b → a = b,
      a = b ∧ a = b ∧ a = b → a = b ∧ a = b,
      a = b ∨ a = b ∧ a = b → a = b
  laws
      (A * A * A * A -* A),
      (A * (A * A * A -* A) -* ⊥),
      (A * ⌜ a = b ∧ a = b → a = b ⌝ -* ⊥)
  init
      %empty
  
  instance after eliminate_persistent_transformation
  
  types
      t
  consts
      a b : t
      A : iProp
  facts
      a = b ∧ a = b ∧ a = b → a = b,
      a = b ∧ a = b ∧ a = b → a = b ∧ a = b,
      a = b ∨ a = b ∧ a = b → a = b
  laws
      (A * A * A * A -* A),
      (A * (A * A * A -* A) -* ⊥),
      (A * ⌜ a = b ∧ a = b → a = b ⌝ -* ⊥)
  init
      %empty
  
  
  Transformation succeeds.
  
  $ is ./merge_quant --until-transformation --show-transformed-instance
  instance after uncurry_transformation
  
  types
      loc
      val
      dfrac
  preds
      pointsto : loc * dfrac * val -> iProp
  consts
      Discarded : dfrac
  laws
      forall l : loc, forall dq1 : dfrac, forall dq2 : dfrac, forall v1 : val, forall v2 : val, (pointsto l dq1 v1 -* (pointsto l dq2 v2 -* ⌜ v1 = v2 ⌝))
  init
      %empty
  
  instance after merge_quantifier_transformation
  
  types
      loc
      val
      dfrac
  preds
      pointsto : loc * dfrac * val -> iProp
  consts
      Discarded : dfrac
  laws
      forall (l : loc) (dq1 dq2 : dfrac) (v1 v2 : val), (pointsto l dq1 v1 -* (pointsto l dq2 v2 -* ⌜ v1 = v2 ⌝))
  init
      %empty
  
  instance after eliminate_persistent_transformation
  
  types
      loc
      val
      dfrac
  preds
      pointsto : loc * dfrac * val -> iProp
  consts
      Discarded : dfrac
  laws
      forall (l : loc) (dq1 dq2 : dfrac) (v1 v2 : val), (pointsto l dq1 v1 -* (pointsto l dq2 v2 -* ⌜ v1 = v2 ⌝))
  init
      %empty
  
  
  Transformation succeeds.
  
  $ is ./elim_pers --until-transformation --show-transformed-instance
  instance after uncurry_transformation
  
  consts
      A B C : iProp
  facts
      Persistent A,
      Persistent B,
      Persistent C
  laws
      (A -* B),
      (C * (A * A -* B * B) -* □ □ □ C),
      ((A -* B) * (A -* B) * A -* B)
  init
      %empty
  
  instance after merge_quantifier_transformation
  
  consts
      A B C : iProp
  facts
      Persistent A,
      Persistent B,
      Persistent C
  laws
      (A -* B),
      (C * (A * A -* B * B) -* □ □ □ C),
      ((A -* B) * (A -* B) * A -* B)
  init
      %empty
  
  instance after eliminate_persistent_transformation
  
  consts
      A B C : iProp
  laws
      (A -* □ B),
      (C * (□ A * □ A -* B * B) -* □ □ □ □ C),
      ((□ A -* B) * (□ A -* B) * A -* □ B)
  init
      %empty
  
  
  Transformation succeeds.
  
