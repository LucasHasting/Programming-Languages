! Name: Lucas Hasting
! Description: program processes the exam scores for a course
! Course: CS 410W: Programming Languages
! Date: 1/29/2024 

! References:  https://pages.mtu.edu/~shene/COURSES/cs201/NOTES/chap05/format.html
!              https://fortran-lang.org/learn/quickstart/arrays_strings/
!              https://gcc.gnu.org/onlinedocs/gfortran/TRIM.html

PROGRAM class
    ! disables implicit typing
    IMPLICIT NONE
    
    ! declare variables
    REAL :: student_exam, average, high_score, low_score
    REAL, DIMENSION(60) :: exam_scores
    INTEGER :: student_amount, i
    CHARACTER(len=30), DIMENSION(60) :: names
    
    ! set default values
    average = 0
    high_score = -1
    low_score = 101
    
    ! validate student amount input
    DO WHILE (student_amount < 1 .OR. student_amount > 60)  
        ! get student amount input
        CALL get_student(student_amount)
    END DO
    
    ! new line
    WRITE(*, *) ""
    
    ! for every student
    DO i = 1, student_amount
        ! reset student_exam variable
        student_exam = -1
        
        ! enter the student's name and save it into an array
        WRITE (*, "(aI2a)", advance = "no") "Enter Student #", i, " name: "
        READ (*, *) names(i)
        
        ! validate the exam score
        DO WHILE (student_exam < 0 .OR. student_exam > 100) 
            ! get the exam score
            CALL valid_exam(student_exam, i)
            
            ! save the exam score in an array
            exam_scores(i) = student_exam
        END DO
        
        ! check for highest score
        IF (student_exam > high_score) THEN
            high_score = student_exam
        END IF
        
        ! check for lowest score
        IF (student_exam < low_score) THEN
            low_score = student_exam
        END IF
        
        ! add to the sum for the average
        average = average + student_exam
        
    END DO
    
    ! calculate average
    average = average / student_amount
    
    ! display results
    WRITE(*, *) ""
    WRITE(*, "(a)", advance = "no") "average: "
    WRITE(*, *) average
    WRITE(*, "(a)", advance = "no") "High Score: "
    WRITE(*, *) high_score
    WRITE(*, "(a)", advance = "no") "Low Score: "
    WRITE(*, *) low_score
    WRITE(*, *) ""

    WRITE(*, "(a)") "Students with an average or above score: "
    
    ! display students with an average or above score
    DO i = 1, student_amount
        IF (exam_scores(i) >= average) THEN
            WRITE(*, "(a)", advance="no") TRIM(names(i))
            WRITE(*, "(a)", advance="no") " with a score of:"
            WRITE(*, *) exam_scores(i)
        END IF
    END DO
    
END PROGRAM class

! subroutine name: get_student
! subroutine description: gets input for the student amount
SUBROUTINE get_student(student_amount)
    ! disables implicit typing
    IMPLICIT NONE
    
    ! declare variables and their intent
    INTEGER, INTENT(OUT) :: student_amount
    
    ! get input
    WRITE (*, "(a)", advance='no') "Enter the number of students: "
    READ (*, *) student_amount
END SUBROUTINE

! subroutine name: valid_exam
! subroutine description: gets input for the exam score
SUBROUTINE valid_exam(exam_score, student)
    ! disables implicit typing
    IMPLICIT NONE
    
    ! declare variables and their intent
    REAL, INTENT(OUT) :: exam_score
    INTEGER, INTENT(IN) :: student

    ! get input
    WRITE (*, "(aI2a)", advance='no') "Enter the exam score for Student #", student, ": "
    READ (*, *) exam_score
END SUBROUTINE

