value(X) :- X = 1 + 2 * 3.
tuple(X) :- X = (a, b).
list_with_tail(L) :- L = [1, 2 | T], T = [3].

?- value(X).
?- tuple(Y).
?- list_with_tail(L).
