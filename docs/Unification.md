# Unification

Unification is the process of finding a substitution that makes two terms equal.
In the context of logic programming, this means finding a substitution that
makes two terms unify. The `unify` function in this crate implements the
unification algorithm for terms in the language of Prolog.

Here we go through a few examples to illustrate how the `unify` function would
work with different inputs. For the sake of these examples, let's assume we have
a simplified version of the `Environment` that can bind variables to values and
look them up.

## Example 1: Unifying two atoms

```rust
let term1 = Term::Atom("hello");
let term2 = Term::Atom("hello");
let mut env = Environment::new();

let result = unify(&term1, &term2, &mut env);
```

In this case, `result` would be `true` because both terms are atoms with the
same value.

## Example 2: Unifying a variable with an integer

```rust
let term1 = Term::Variable("X");
let term2 = Term::Integer(42);
let mut env = Environment::new();

let result = unify(&term1, &term2, &mut env);
```

Here, `result` would be `true`, and the environment would be updated to include
a binding from the variable `"X"` to the integer `42`.

## Example 3: Unifying two lists with equal elements

```rust
let term1 = Term::List(vec![Term::Integer(1), Term::Integer(2)], None);
let term2 = Term::List(vec![Term::Integer(1), Term::Integer(2)], None);
let mut env = Environment::new();

let result = unify(&term1, &term2, &mut env);
```

`result` would be `true` because the two lists are the same length and their
corresponding elements unify successfully.

## Example 4: Unifying a variable with a list

```rust
let term1 = Term::Variable("Y");
let term2 = Term::List(vec![Term::Atom("apple"), Term::Atom("banana")], None);
let mut env = Environment::new();

let result = unify(&term1, &term2, &mut env);
```

`result` would be `true`, with the environment updated to bind variable `"Y"` to
the list `["apple", "banana"]`.

## Example 5: Unifying two structures with different names

```rust
let term1 = Term::Structure("person", vec![Term::String("Alice")]);
let term2 = Term::Structure("customer", vec![Term::String("Alice")]);
let mut env = Environment::new();

let result = unify(&term1, &term2, &mut env);
```

In this case, `result` would be `false` because the names of the structures do
not match, even though the elements within them do.

## Example 6: Unifying two lists with different lengths

```rust
let term1 = Term::List(vec![Term::Integer(1)], None);
let term2 = Term::List(vec![Term::Integer(1), Term::Integer(2)], None);
let mut env = Environment::new();

let result = unify(&term1, &term2, &mut env);
```

`result` would be `false` because the lists are of different lengths, so they
cannot be unified.

## Example 7: Unifying two complex structures

```rust
let term1 = Term::Structure("pair", vec![Term::Variable("A"), Term::Integer(2)]);
let term2 = Term::Structure("pair", vec![Term::Integer(1), Term::Variable("B")]);
let mut env = Environment::new();

let result = unify(&term1, &term2, &mut env);
```

`result` would be `true`, and the environment would be updated to include
bindings for `"A"` to `1` and `"B"` to `2`, because the structures have the same
name and the corresponding terms within them can be unified.
