PROGRAM PROCEDURE
IMPLICIT NONE
REAL :: A, B, C, RESULT

WRITE (*, "(a)", advance = "no") "Enter a: "
READ (*, *) A

WRITE (*, "(a)", advance = "no") "Enter b: "
READ (*, *) B

RESULT = calc_c(A, B)
WRITE (*, *) "c = ", RESULT
END PROGRAM PROCEDURE

FUNCTION calc_c(A, B)
IMPLICIT NONE
REAL :: calc_c
REAL, INTENT(in) :: A, B
calc_c = SQRT(A**2 + B**2)
END FUNCTION calc_c
