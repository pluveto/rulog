value(X) :- X is 1 + 2 * 3.
negate(Y) :- Y is -(5) + 10.
mix(Z) :- A is 3, Z is A + 0.5.

?- value(X).
?- negate(Y).
?- mix(Z).
?- 1 < 2.
?- 5 > 6.
?- 2 =:= 1 + 1.
?- 2 =\= 2.
?- 3 >= 2.
?- 2 =< 3.
?- var(X).
?- nonvar(hello).
?- atom(hello).
?- integer(42).
?- float(3.14).
?- compound(foo(1)).
