! Compute the area of a triangle given 3 sides
! Forumla:
! S = (A+B+C)/2
! Heron's Formula A = (S*(S-a)*(S-b)*(S-c))^1/2

PROGRAM triangle
    REAL :: A, B, C, S, Area, AreaSqr
    WRITE (*,*) "This program computes the area of a triangle"
    WRITE (*,*) "Type in the lengths of the 3 sides: "
    READ (*,*) A, B, C 
    WRITE (*,*) "You have entered the following lengths: "
    WRITE (*,*) A, B, C 
    S = 0.5 * (A+B+C)
    AreaSqr = S*(S-A)*(S-B)*(S-C)

    END IF (AreaSqr < 0.0) THEN 
        WRITE(*, *) "Error: Invalid Triangle"
        END PROGRAM Triangle
    
    Area = SQRT(AreaSqr)
    WRITE (*,*) "The area is: ", Area
    
END PROGRAM triangle