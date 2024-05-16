compute(X,Y,P) :- X>Y, P is 2*X.
compute(X,Y,P) :- X=<Y, P is Y/3.