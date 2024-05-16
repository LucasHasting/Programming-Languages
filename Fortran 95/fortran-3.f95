PROGRAM PROCEDURE
IMPLICIT NONE
REAL :: A, B, C

WRITE (*, "(a)", advance = "no") "Enter a: "
READ (*, *) A

WRITE (*, "(a)", advance = "no") "Enter b: "
READ (*, *) B

CALL calc_c(A, B, C)
WRITE (*, *) "c = ", C
END PROGRAM PROCEDURE

SUBROUTINE calc_c(A, B, C)
IMPLICIT NONE
REAL, INTENT(IN) :: A
REAL, INTENT(IN) :: B
REAL, INTENT(OUT) :: C
C = SQRT(A**2 + B**2)
END SUBROUTINE