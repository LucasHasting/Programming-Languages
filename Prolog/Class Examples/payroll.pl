pay(Person,Dollars) :- hours(Person,H), >(40,H), rate(Person,R), Dollars is H*R.
pay(Person,Dollars) :- hours(Person,H), =<(40,H), rate(Person,R), Dollars is 40*R+1.5*(H-40)*R.
hours(smith,30).
hours(jones,50).
rate(smith,8).
rate(jones,6.75).
