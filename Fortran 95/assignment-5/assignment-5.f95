! Name: Lucas Hasting
! Description: program processes the exam scores for a course using an input file
! Course: CS 410W: Programming Languages
! Date: 1/31/2024 

! References:  https://pages.mtu.edu/~shene/COURSES/cs201/NOTES/chap05/format.html
!              https://fortran-lang.org/learn/quickstart/arrays_strings/
!              https://gcc.gnu.org/onlinedocs/gfortran/TRIM.html
!              https://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vnbc/index.html

PROGRAM class
    ! disables implicit typing
    IMPLICIT NONE
    
    ! declare variables
    REAL :: student_exam, average, high_score, low_score
    REAL, DIMENSION(60) :: exam_scores
    INTEGER :: student_amount, i, nvals, status
    CHARACTER(LEN=30) :: filename
    CHARACTER(LEN=30), DIMENSION(60) :: names
    
    ! set default values
    average = 0
    high_score = -1
    low_score = 101
    nvals = 0
    i = 1
    
    ! get filename
    WRITE (*, "(a)", advance="no") "Input file name: "
    READ (*, *) filename
    
    ! open file
    OPEN (unit=3, file=filename, status="old", action="read", iostat=status)
    
    ! if the file opens successfully
    openif: IF (status==0) THEN
    
        ! loop through the file
        readloop: DO
        
            ! get name, add 1 to val counter, validate
            read(3, *, iostat=status) names(i)
            nvals = nvals + 1
            IF (status /= 0 .OR. nvals > 120) EXIT
            
            ! get score, add 1 to val counter, validate
            read(3, *, iostat=status) exam_scores(i)
            nvals = nvals + 1
            IF (status /= 0 .OR. nvals > 120 .OR. exam_scores(i) < 0 .OR. exam_scores(i) > 100) EXIT
            
            ! check for highest score
            IF (exam_scores(i) > high_score) THEN
                high_score = exam_scores(i)
            END IF
            
            ! check for lowest score
            IF (exam_scores(i) < low_score) THEN
                low_score = exam_scores(i)
            END IF
            
            ! add to the sum for the average
            average = average + exam_scores(i)
            
            ! increase index for name and exam score
            i = i + 1
            
        END DO readloop
        
        ! if there was an error in the file, tell the user and end the program
        IF (status > 0 .OR. exam_scores(i) < 0 .OR. exam_scores(i) > 100 .OR. nvals > 120) THEN
            WRITE (*, "(aI3)") "An error occurred reading line: ", nvals
            STOP
        END IF
        
    ! if there was an error opening the file, tell the user and end the program
    ELSE openif
        WRITE (*, "(a)") "Error opening file."
        STOP
    END IF openif
    
    ! close the file
    CLOSE(unit=3)
    
    ! calculate average
    average = average / (nvals / 2)
    
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
    DO i = 1, (nvals/2)
        IF (exam_scores(i) >= average) THEN
            WRITE(*, "(a)", advance="no") TRIM(names(i))
            WRITE(*, "(a)", advance="no") " with a score of:"
            WRITE(*, *) exam_scores(i)
        END IF
    END DO
    
END PROGRAM class