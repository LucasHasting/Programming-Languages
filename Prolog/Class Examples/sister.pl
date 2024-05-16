male(albert).
male(edward).

female(alice).
female(victoria).

parents(edward,victoria,albert).
parents(alice,victoria,albert).

sister_of(X,Y) :- female(X), parents(X,Mother,Father), parents(Y,Mother,Father), X\=Y.