!Name: Lucas Hasting
!Description: Calculate the total cost when purchasing coal, carrots, and top hats
!Course: CS 410W: Programming Languages
!Date: 1/24/2024 

PROGRAM Sally
    !declare variables
    REAL :: coal, carrot, topHats, totalCost
    
    goto begining
    !get input and display output
    WRITE (*,*) "How many lumps of coal: "
    READ (*,*) coal
    WRITE (*,*) "How many carrots: "
    READ (*,*) carrot
    WRITE (*,*) "How many top hats: "
    READ (*,*) topHats 
    
    !compute total cost
    totalCost = (0.25 * coal) + (0.75 * carrot) + (4.50 * topHats)
    
    !display total cost
    WRITE (*,*) "The total cost is: ", totalCost
    
    
END PROGRAM Sally




