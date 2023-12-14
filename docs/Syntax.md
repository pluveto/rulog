# Syntax

The grammar is defined in [BNF](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form) and is as follows:

```bnf
<program> ::= <clause-list>
  <clause-list> ::= <clause> '.' <clause-list> | ε
  <clause> ::= <directive> | <query> | <predicate> | <predicate> ':-' <predicate-list>
  
  <directive> ::= ':-' <predicate> | ':-' <operator-definition>
    <operator-definition> ::= 'op' '(' <priority> ',' <operator-type> ',' <atom> ')'
    <priority> ::= <integer>
    <operator-type> ::= 'xfx' | 'xfy' | 'yfx' | 'fx' | 'fy' | 'xf' | 'yf'
  
  <query> ::= '?-' <predicate-list> '.'

  <predicate> ::= <atom> | <compound-term>
    <atom> ::= <ATOM> | <operator-atom>
    <operator-atom> ::= <defined-operator>

    <compound-term> ::= <atom> '(' <term-list> ')' | <term> <infix-op> <term> | <prefix-op> <term>
      <term-list> ::= <term> ',' <term-list> | <term>
      <term> ::= <atom> | <variable> | <number> | <string> | <list> | <compound-term>
      <list> ::= '[' <term-list> ']' | '[' <term-list> '|' <term> ']' | '[]'

  <predicate-list> ::= <predicate> ',' <predicate-list> | <predicate>

<number> ::= <integer> | <float>
  <integer> ::= <INTEGER>
  <float> ::= <FLOAT>
<defined-operator> ::= <any sequence of operator characters defined by operator-definition>
<variable> ::= <VARIABLE>
<string> ::= <STRING>
<infix-op> ::= <defined-operator>
<prefix-op> ::= <defined-operator>
```
