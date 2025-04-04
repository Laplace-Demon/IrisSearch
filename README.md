### Problem statement

Given a set of separation logic formulae, decide whether their separating conjunction entails false.

### Syntax of input

```
instance ::= declaration '%' list(iprop)

declaration ::= atom_declaration

atom_declaration ::= 'Atom:' separated_list(',', id)

simple_iprop ::= 'False'
  | atom
  | simple_iprop '*' simple_iprop

iprop ::= simple_iprop
  | '□' '(' simple_iprop '-*' simple_iprop ')'

```

A problem instance begins with the declaration section, which currently contains a list of atom declarations. A percent symbol `%` is used to end the declaration section. After that, there should be a list of first-order formulae. Here, first-order means magic wands are not nested. For the time being, we only allow persistent rules.
