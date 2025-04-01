### Problem statement

Given a set of separation logic formulae, decide whether their separating conjunction entails false.

### Syntax of input

```
instance ::= declaration '%' list(iprop)

declaration ::= atom_declaration

atom_declaration ::= 'Atom:' separated_list(',', id)

iprop ::= iprop_no_wand
  | iprop_no_wand '-*' iprop_no_wand

iprop_no_wand ::= 'False'
  | id
  | iprop_no_wand * iprop_no_wand
```

A problem instance begins with the declaration section, which currently contains a list of atom declarations. A percent symbol `%` is used to end the declaration section. After that, there should be a list of first-order formulae. Here, first-order means magic wands are not nested.
