yard(y1,140,100).
yard(y2,200,300).
yard(y3,150,100).
type(t1,5).
type(t2,6.75).
type(t3,8).
perim(Y,P) :- yard(Y,D1,D2), P is 2*D1+2*D2.
price(Y,T,P) :- perim(Y,P1), type(T,T1), P is P1*T1.