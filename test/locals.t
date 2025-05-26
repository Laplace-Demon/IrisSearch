  $ is ./locals --show-path
  path
  
  locals
      %empty
  atoms
      □ (P1 a b)
  pures
      %empty
  
    ↓ applying law forall (a : A) (b : B), (P1 a b -* exists (aa : A) (bb : B), P2 aa bb)
  
  locals
      aa_0 : A
      bb_0 : B
  atoms
      □ (P1 a b)
      □ (P2 aa_0 bb_0)
  pures
      %empty
  
    ↓ applying law forall (a : A) (b : B), (P2 a b -* exists (aa : A) (bb : B), P3 aa bb)
  
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
  
    ↓ applying law forall (a : A) (b : B), (P3 a b -* exists (aa : A) (bb : B), P4 aa bb)
  
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
  
    ↓ applying law forall (a : A) (b : B), (P4 a b -* exists (aa : A) (bb : B), P5 aa bb)
  
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
  
