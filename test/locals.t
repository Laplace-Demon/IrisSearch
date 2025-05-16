  $ is ./locals --show-path
  path
  
  locals
      %empty
  atoms
      □ (P1 a b)
  pures
      %empty
  
  ↓
  
  locals
      aa_0 : A
      bb_0 : B
  atoms
      □ (P1 a b)
      □ (P2 aa_0 bb_0)
  pures
      %empty
  
  ↓
  
  locals
      aa_5 : A
      bb_5 : B
      aa_0 : A
      bb_0 : B
  atoms
      □ (P1 a b)
      □ (P2 aa_0 bb_0)
      □ (P3 aa_5 bb_5)
  pures
      %empty
  
  ↓
  
  locals
      aa_10 : A
      bb_10 : B
      aa_5 : A
      bb_5 : B
      aa_0 : A
      bb_0 : B
  atoms
      □ (P1 a b)
      □ (P2 aa_0 bb_0)
      □ (P3 aa_5 bb_5)
      □ (P4 aa_10 bb_10)
  pures
      %empty
  
  ↓
  
  locals
      aa_19 : A
      bb_19 : B
      aa_10 : A
      bb_10 : B
      aa_5 : A
      bb_5 : B
      aa_0 : A
      bb_0 : B
  atoms
      □ (P1 a b)
      □ (P2 aa_0 bb_0)
      □ (P3 aa_5 bb_5)
      □ (P4 aa_10 bb_10)
      □ (P5 aa_19 bb_19)
  pures
      %empty
  
  Applying law
      forall (a : A) (b : B), (P5 a b -* ⊥)
  yields False.
  
  find solution
  
