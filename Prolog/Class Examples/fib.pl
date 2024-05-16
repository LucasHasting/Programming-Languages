fib(1,1).
fib(2,1).
fib(X,ANS) :- X>2, Y is X-1, Z is X-2, fib(Y,F1), fib(Z,F2), ANS is F1+F2. 
