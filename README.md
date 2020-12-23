# proof-generator
Proof generator using Haskell

## Propositional logic (0th order)
* *Variables*:  Should start with an upper case-letter (A, B, VarIaBle...)

### Operators

1. *Negation* (NOT): !
2. *Conjuction* (AND): ^
3. *Disjunction* (OR): v (lower-case letter 'v')
4. *Conditional* (IMPL, IF): -> 
5. *Equality*: = , *Inequalty* (XOR): !=
(Hint: the Operators are evaluated in that order. !A ^ B v C -> D = E != F is the same as ((!A) ^ (B v (C -> (D = E != F)))) 
