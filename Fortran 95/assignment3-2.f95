!Name: Lucas Hasting
!Description: Calculate the total cost when purchasing a bulk order of books
!Course: CS 410W: Programming Languages
!Date: 1/24/2024 

PROGRAM Book
    !declare variables
    REAL :: bookCost, bookAmount, shipping, tax
    
    !get input and output
    WRITE (*,*) "What is the book cost: "
    READ (*, *) bookCost
    WRITE (*,*) "How many books are you ordering: "
    READ (*,*) bookAmount

    !calculate and display bulk cost
    bookCost = bookAmount * bookCost
    WRITE (*,*) "The total bulk cost is: ", bookCost
    
    !calculate and display tax
    tax = bookCost * 0.092
    WRITE (*,*) "The total sales tax is: ", tax
    
    !calculate and display shipping
    shipping = bookAmount * 2
    WRITE (*,*) "The total shipping cost is: ", shipping
    
    !calculate and display book cost
    bookCost = bookCost + tax + shipping
    WRITE (*,*) "The total cost is: ", bookCost
    
END PROGRAM Book

