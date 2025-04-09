### Problem statement

Given a set of separation logic formulae, decide whether their separating conjunction entails false.

### Syntax of input

```
instance ::= decl_consts decl_laws decl_init

decl_consts ::= 'consts' list(decl_type)

decl_type ::= string ':' itype

itype ::= 'Prop'
  | 'iProp'

decl_laws ::= 'laws' list(decl_law)

decl_law ::= iprop
  | 'Persistent' iprop

decl_init ::= 'init' list(iprop)

iprop ::= simple_iprop
  | simple_iprop '-*' simple_iprop

simple_iprop ::= 'False'
  | string
  | simple_iprop '*' simple_iprop
```

Law represent either a persistent iprop or a prop.
