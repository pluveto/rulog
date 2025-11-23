f(a).
f(b).

g(X) :- f(X), !.
plain(X) :- f(X).

left(a).
right(b).

choose(X) :- (left(X) ; right(X)).

cond.
then_value(1).
else_value(2).

decision(X) :- (cond -> then_value(X) ; else_value(X)).

?- \+ left(c).
?- g(X).
?- plain(X).
?- choose(Y).
?- decision(Z).
