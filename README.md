### Problem statement

Given a set of separation logic formulae, decide whether their separating conjunction entails false.

### Syntax of input

```
instance ::= option(decl_types) option(decl_preds) decl_consts decl_laws decl_init

decl_types ::= 'types' list(decl_type)

decl_type ::= ident

decl_preds ::= 'preds' list(decl_pred)

decl_pred ::= ident ':' separated_nonempty_list('*', ident) '->' [ 'Prop' | 'iProp' ]

decl_consts ::= 'consts' list(decl_const)

decl_const ::= ident ':' itype

itype ::= 'Prop'
  | 'iProp'
  | ident

decl_laws ::= 'laws' list(decl_law)

decl_law ::= 'Exclusive' ident
  | iprop
  | prop

decl_init ::= 'init' list(iprop)

iprop ::= simple_iprop
  | simple_iprop '-*' simple_iprop

simple_iprop ::= 'False'
  | simple_iprop '*' simple_iprop
  | '□' simple_iprop
  | '⌜' prop '⌝'
  | ident list(term)

prop ::= simple_prop
  | simple_prop '->' simple_prop

simple_prop ::= 'Persistent' ident
  | '¬' simple_prop
  | simple_prop '∧' simple_prop
  | simple_prop '∨' simple_prop
  | ident list(term)

term ::= ident
```

The above grammar rule differs from that used in the parser, mainly in that:
  - the parser permits parentheses
  - the parser does not distinguish between `simple_iprop` and `iprop`, `simple_prop` and `prop`
