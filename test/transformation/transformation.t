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
      forall l : loc, forall dq1 : dfrac, forall dq2 : dfrac, forall v1 : val, forall v2 : val, (pointsto l dq1 v1 * pointsto l dq2 v2 -* ⌜ v1 = v2 ⌝)
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
      forall (l : loc) (dq1 dq2 : dfrac) (v1 v2 : val), (pointsto l dq1 v1 * pointsto l dq2 v2 -* ⌜ v1 = v2 ⌝)
  init
      %empty
  
  
  Transformation succeeds.
  
