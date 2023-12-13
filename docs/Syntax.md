# Syntax

The grammar is defined in [BNF](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form) and is as follows:

```bnf
<program> ::= <clause-list>

<clause-list> ::= <clause> '.' <clause-list> | ε

<clause> ::= <predicate> | <predicate> ':-' <predicate-list>

<predicate-list> ::= <predicate> ',' <predicate-list> | <predicate>

<predicate> ::= <atom> | <atom> '(' <term-list> ')'

<term-list> ::= <term> ',' <term-list> | <term>

<term> ::= <atom> | <variable> | <integer> | <float> | <string> | <list> | <structured-term>

<list> ::= '[' <term-list> ']' | '[' <term-list> '|' <term> ']' | '[]'

<structured-term> ::= <atom> '(' <term-list> ')'

<atom> ::= <ATOM>

<variable> ::= <VARIABLE>

<integer> ::= <INTEGER>

<float> ::= <FLOAT>

<string> ::= <STRING>
```
