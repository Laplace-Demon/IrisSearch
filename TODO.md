theory:
  nombre entier, natural number, (positive) rational number

( implement natural numbers and real numbers, pretend them to be various subtypes by adding constraints to SMT solver )

plus, minus, negation
greater, smaller


simplification and unification

the current instantiation scheme becomes incomplete when we add interpreted terms

if we abstract every compound terms such as forall x, f(T(x)) -* Concl(x) -> forall u, x, f(u) * (u = T(x)) -* Concl(x)

when trying to apply the law against the premise f(T'(st)), we unify the abstraction variable u with T'(st) and obtain: 
  forall x, (T'(st) = T(x)) -* Concl(x)

this becomes a goal-directed law where we instantiate x on Concl(x)

this is costly because we have to take this into account for each law application.

we should probably use a term-indexed table and check satisfiability when adding this


- different from a law, in that it's non-persistent, and should only be used in a goal-directed way
- should not be an actual part of a state, because it's difficult to define subsumption relation then
- but we do need to fix the iProp part of, because it's supposed to be "comsumed" in the future, this does not influence the completeness of algorithm


this solves another problem where we have:
  P n * (n < m) -* P m

unifying n, we get forall m, (n < m) -* P m

it's again a goal-directed rule which is instantiated lazily. If we need P(T), we verify n < P(T) by using a SMT solver.

This delayed instantiation should be combined with lazy case splitting, because that requires partial substraction with instantiation


lazy case splitting will increase performance and generate shorter proofs, suppose the ordering of successors is correct

lazy case splitting may not a full split, a law will generate (state list) * (state list), a list of law will also generate (state list) * (state list),
the former being the concatenation, the latter being combined in a special way.


if A < B,
  if A.br = B.br, continue
  else if B is solved, try to reuse proof of B
    this requires to trace origin of derived atoms, in order to perform a backward deduction to compute the "smallest refutation core"
    this should be done in a call-by-need manner, a little bit like union-find path-compression
  else do nothing

1. ordering on disjunctive list
2. subset relation with instantiation
3. subsumption
4. record branch on state
5. record solved path on state
6. record derivation on props and atoms


---

  Lemma mono_nat_own_alloc_strong P n :
    pred_infinite P →
    ⊢ |==> ∃ γ, ⌜P γ⌝ ∗ mono_nat_auth_own γ 1 n ∗ mono_nat_lb_own γ n.

decouper Multiset and Multiset2
