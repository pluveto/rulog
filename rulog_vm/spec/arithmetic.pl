value(X) :- X is 1 + 2 * 3.
negate(Y) :- Y is -(5) + 10.
mix(Z) :- A is 3, Z is A + 0.5.

?- value(X).
?- negate(Y).
?- mix(Z).
