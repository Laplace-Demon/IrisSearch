  $ is ./law_trans2 --show-path
  
    path
  
  locals
      %empty
  atoms
      mlist l0 Cons(Loc(l1), Cons(Loc(l2), Nil))
      pointsto l1 Loc(l2)
      pointsto l2 Loc(l3)
  pures
      null = l0
  
  Applying law (forall (v : val) (vs : list), mlist null Cons(v, vs) -* ⊥) yields False.
  
    find refutation
  
