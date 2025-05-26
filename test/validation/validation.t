  $ is ./dup1 --until-validation
  validation error: duplicate type declaration of val
  [1]
  $ is ./dup2 --until-validation
  validation error: duplicate predicate declaration of p1
  [1]
  $ is ./dup3 --until-validation
  validation error: duplicate constant declaration of f
  [1]
  $ is ./miss1 --until-validation
  validation error: missing constant declaration of something
  [1]
  $ is ./miss2 --until-validation
  validation error: missing constant declaration of v
  [1]
  $ is ./miss3 --until-validation
  validation error: missing predicate declaration of p3
  [1]
  $ is ./miss4 --until-validation
  validation error: missing predicate declaration of p3
  [1]
  $ is ./miss5 --until-validation
  validation error: missing type declaration of loc
  [1]
  $ is ./miss6 --until-validation
  validation error: missing type declaration of loc
  [1]
  $ is ./illformed_pred1 --until-validation
  validation error: illegal predicate declaration of add
  [1]
  $ is ./illformed_pred2 --until-validation
  validation error: illegal predicate declaration of star
  [1]
  $ is ./type1 --until-validation
  validation error: pr should have type iProp, but has type Prop
  [1]
  $ is ./type2 --until-validation
  validation error: v should have type loc, but has type val
  [1]
  $ is ./type3 --until-validation
  validation error: l should have type val, but has type loc
  [1]
  $ is ./type4 --until-validation
  validation error: pointsto should have type predicate, but has type loc * val -> iProp
  [1]
  $ is ./type5 --until-validation
  validation error: add should have type predicate, but has type val * val * val -> Prop
  [1]
  $ is ./type6 --until-validation
  validation error: v1 should have type loc, but has type val
  [1]
  $ is ./multiple_const --until-validation
  
  Validation succeeds.
  
  $ is ./quant1 --until-validation
  
  Validation succeeds.
  
  $ is ./quant2 --until-transformation --show-transformed-instance
  validation error: l should have type loc, but has type val
  [1]
  $ is ./quant3 --until-validation
  validation error: P should have type constant, but has type iProp
  [1]
  $ is ./form1
  validation error: illegal law declaration, premise "(C -* C)" of law should be simple iprop
  [1]
  $ is ./form2
  validation error: illegal law declaration, conclusion "forall var : t, A" of law should be an existentially quantified simple iprop
  [1]
  $ is ./form3
  validation error: illegal law declaration, law "⊥" should be a universally quantified wand
  [1]
  $ is ./form4
  validation error: illegal law declaration, universally quantified variable "c" should appear in the premise "P2 a b" of law
  [1]
  $ is ./form5
  validation error: illegal law declaration, existentially quantified variable "c" should appear in the conclusion "P2 a b" of law
  [1]
  $ is ./form6
  validation error: illegal init declaration, iprop "(⊥ -* ⊥)" in init declaration should be simple
  [1]
  $ is ./named_law1 --until-validation --show-instance
  original instance
  
  consts
      A B C : iProp
  laws
      first_law : (A * B -* C),
      second_law : (B * C -* A),
      third_law : (C * A -* B)
  init
      %empty
  
  
  Validation succeeds.
  
  $ is ./named_law2 --until-validation
  validation error: duplicate law declaration of second_law
  [1]
