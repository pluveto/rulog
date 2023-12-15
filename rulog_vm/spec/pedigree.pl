parent(tom, liz).
parent(bob, ann).
parent(liz, sophie).
parent(ann, sophie).

sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.

?- sibling(sophie, Sibling).
