salary(bob,100000).
salary(jane,40000).
salary(dave,60000).

credit(bob,685).
credit(jane,710).
credit(dave, 803).

maxApprove(Person,Amount) :- salary(Person,Wages),credit(Person,Rating),>=(Rating,600),Amount is Wages.
maxApprove(Person,Amount) :- salary(Person,Wages),credit(Person,Rating),>=(Rating,700),Amount is 2*Wages.
maxApprove(Person,Amount) :- salary(Person,Wages),credit(Person,Rating),>=(Rating,800),Amount is 3*Wages.


approve(Person,Amount) :- salary(Person,Wages),credit(Person,Rating),>=(Rating,600),=<(Amount,Wages).
approve(Person,Amount) :- salary(Person,Wages),credit(Person,Rating),>=(Rating,700),=<(Amount,2*Wages).
approve(Person,Amount) :- salary(Person,Wages),credit(Person,Rating),>=(Rating,800),=<(Amount,3*Wages).