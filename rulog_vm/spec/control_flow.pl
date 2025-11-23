f(a).
f(b).

g(X) :- f(X), !.
plain(X) :- f(X).

?- g(X).
?- plain(X).
