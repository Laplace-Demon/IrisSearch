  $ is ./dup1 --until-validation
  validation error: duplicate type declaration of val
  [1]
  $ is ./dup2 --until-validation
  validation error: duplicate predicate declaration of p1
  [1]
  $ is ./dup3 --until-validation
  validation error: duplicate const declaration of f
  [1]
  $ is ./miss1 --until-validation
  validation error: missing const declaration of something
  [1]
  $ is ./miss2 --until-validation
  validation error: missing const declaration of v
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
  validation error: pointsto should have type ... -> Prop, but has type loc * val -> iProp
  [1]
  $ is ./type5 --until-validation
  validation error: add should have type ... -> iProp, but has type val * val * val -> Prop
  [1]
  $ is ./type6 --until-validation
  validation error: v1 should have type loc, but has type val
  [1]
