male(roland).
male(mason).
male(lucas).
male(jerry).
male(stanly).
male(pete1).
male(pete2).
male(jonah).
male(jay).
male(bill).
male(mark).
male(justin).
female(carly).
female(lee).
female(joy).
female(pam).
female(nancy).
female(emily).
female(lola).
female(nan).
female(lola2).

parent(carly,roland).
parent(justin,roland).
parent(carly,mason).
parent(justin,mason).
parent(carly,lola2).
parent(justin,lola2).
married(carly,justin).

parent(lee,lucas).
parent(stanly,lucas).
parent(lee,carly).
parent(stanly,carly).
parent(lee,jerry).
parent(stanly,jerry).
married(lee,stanly).

parent(pete1,joy).
parent(nancy,joy).
parent(pete1,lee).
parent(nancy,lee).
married(nancy,pete1).

parent(pete2,nancy).
parent(lola,nancy).
married(lola,pete2).

parent(jay,pete1).
parent(nan,pete1).
married(jay,nan).

parent(emily,stanly).
parent(johah,stanly).
parent(emily,pam).
parent(johah,pam).
married(emily,jonah).

married(mark,joy).
married(pam,bill).

mother(X,Y) :- parent(X,Y),female(X).
father(X,Y) :- parent(X,Y),male(X).
daughter(X,Y) :- parent(Y,X),female(X).
son(X,Y) :- parent(Y,X),male(X).

sibling(X,Y) :- parent(Z,X),parent(Z,Y).
brother(X,Y) :- sibling(X,Y),male(X).
sister(X,Y) :-  sibling(X,Y),female(X).
grandchild(X,Y) :- parent(Z,X),parent(Y,Z).
grandparent(X,Y) :- parent(X,Z),parent(Z,Y).
aunt(X,Y) :- parent(Z,Y),sister(X,Z).
uncle(X,Y) :- parent(Z,Y),brother(X,Z).
niece(X,Y) :- sibling(Z,Y),daughter(X,Z).
nephew(X,Y) :- sibling(Z,Y),son(X,Z).
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(Z,Y),ancestor(X,Z).

grandson(X,Y) :- grandchild(X,Y),male(X).
grandaughter(X,Y) :- grandchild(X,Y),female(X).
grandfather(X,Y) :- grandparent(X,Y),male(X).
grandmother(X,Y) :- grandparent(X,Y),female(X).




















