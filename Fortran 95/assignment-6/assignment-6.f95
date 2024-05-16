! Name: Lucas Hasting
! Description: program processes student records from a file and presents them to the user
! Course: CS 410W: Programming Languages
! Date: 2/5/2024 

! References:  https://pages.mtu.edu/~shene/COURSES/cs201/NOTES/chap05/format.html
!              https://web.math.utk.edu/~vasili/refs/Fortran/AiS-f77/fortran7.10.html
!              https://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vnbc/index.html
!              https://pages.mtu.edu/~shene/COURSES/cs201/NOTES/chap01/continue.html
!              https://wisdomthroughknowledge.blogspot.com/2016/04/fortran-77-f77-convert-integer-to-string.html

PROGRAM ACM
    ! declare derived type ACM_Member
    TYPE :: ACM_Member
        CHARACTER (len=10) :: firstName
        CHARACTER (len=10) :: lastName
        CHARACTER (len=20) :: email
        CHARACTER (len=20) :: phone
        INTEGER :: age
        REAL :: duesOwed
    END TYPE ACM_Member
    
    ! declare variables
    CHARACTER(len=30) :: filename
    INTEGER :: maxLine, i, status, checkForHeader
    TYPE (ACM_Member) :: student(100)
    REAL :: dues
    
    ! initilize variables
    maxLine = 100 * 6
    i = 1
    checkForHeader = 0
    
    ! get filename
    WRITE (*, "(a)", advance="no") "Input file name: "
    READ (*, *) filename
    
    ! open file
    OPEN (unit=3, file=filename, status="old", action="read", iostat=status)
    
    ! if the file opens successfully
    openif: IF (status==0) THEN
    
        ! loop through the file
        readloop: DO
            
            ! get first name, add 1 to val counter, validate
            read(3, *, iostat=status) student(i)%firstName
            nvals = nvals + 1
            IF (status /= 0) EXIT
            
            ! get last name, add 1 to val counter, validate
            read(3, *, iostat=status) student(i)%lastName
            nvals = nvals + 1
            IF (status /= 0) EXIT
            
            ! get email, add 1 to val counter, validate
            read(3, *, iostat=status) student(i)%email
            nvals = nvals + 1
            IF (status /= 0) EXIT
            
            ! get phone number, add 1 to val counter, validate
            read(3, '(a)', iostat=status) student(i)%phone
            nvals = nvals + 1
            IF (status /= 0) EXIT
            
            ! get age, add 1 to val counter, validate
            read(3, *, iostat=status) student(i)%age
            nvals = nvals + 1
            IF (status /= 0) EXIT
            
            ! get dues owed, add 1 to val counter, validate
            read(3, *, iostat=status) student(i)%duesOwed
            nvals = nvals + 1
            IF (status /= 0 .OR. nvals > maxLine) EXIT
            
            ! increase index
            i = i + 1
            
        END DO readloop
        
        ! if there was an error in the file, tell the user and end the program
        IF (status > 0 .OR. nvals > maxLine) THEN
            WRITE (*, "(aI3)") "An error occurred reading line: ", nvals
            STOP
        END IF
        
        ! blank line  
        WRITE(*, *) 
        
        ! display a student record table
        WRITE(*, "(a)") "Student Records: "
        CALL displayHeader()

        ! display every student in the record using displayStudent subroutine
        DO i = 1, (nvals/6)
            CALL displayStudent(student(i)%firstName, student(i)%lastName, student(i)%email, &
                student(i)%phone, student(i)%age, student(i)%duesOwed)
        END DO
        
        ! blank line  
        WRITE(*, *)
        
        ! get input for due owed filter
        WRITE(*, "(a)", advance="no") "Filter by dues owed (Enter minimum amount owed): $"
        READ(*, *) dues
        
        ! blank line
        WRITE(*, *)
        
        ! display information filtered by user
        DO i = 1, (nvals/6)
            
            ! apply filter
            IF (student(i)%duesOwed >= dues) THEN
            
                ! decide if the header needs to be displayed
                IF (checkForHeader == 0) THEN
                    checkForHeader = checkForHeader + 1
                    CALL displayHeader()
                END IF
                
                ! display every student in the filter using displayStudent subroutine
                CALL displayStudent(student(i)%firstName, student(i)%lastName, student(i)%email, &
                student(i)%phone, student(i)%age, student(i)%duesOwed)
            END IF
        END DO
        
        ! check if the header was displayed, if not tell the user no students were found
        IF (checkForHeader == 0) THEN
            WRITE(*, "(a)") "No students found"
            STOP
        END IF
        
    ! if there was an error opening the file, tell the user and end the program
    ELSE openif
        WRITE (*, "(a)") "Error opening file."
        STOP
    END IF openif
    
    ! close the file
    CLOSE(unit=3)
END PROGRAM ACM

! subroutine name: displayStudent
! subroutine description: displays a student record
SUBROUTINE displayStudent(fname, lname, email, phone, age, duesOwed)
    ! declare parameter variables
    CHARACTER (len=10), INTENT(IN) :: fname
    CHARACTER (len=10), INTENT(IN) :: lname
    CHARACTER (len=20), INTENT(IN) :: email
    CHARACTER (len=20), INTENT(IN) :: phone
    INTEGER, INTENT(IN) :: age
    REAL, INTENT(IN) :: duesOwed
    
    ! declare local variables
    CHARACTER (len=3) :: age_text
    CHARACTER (len=10) :: dues_text
    
    ! convert age to string
    WRITE (age_text, "(i3)") age
    age_text = ADJUSTL(age_text)

    ! convert duesOwed to string    
    WRITE (dues_text, "(f10.5)") duesOwed
    dues_text = ADJUSTL(dues_text)
    
    ! display the student record
    WRITE (*, "(a10,2x,a10,1x,a20,1x,a20,1x,a3,2x,a10)") fname, lname, email, &
        phone, age_text, dues_text
END SUBROUTINE displayStudent

! subroutine name: displayHeader
! subroutine description: displays the header for a table displaying student records
SUBROUTINE displayHeader()
    ! declare local variables
    CHARACTER (len=10) :: fname = "First-Name"
    CHARACTER (len=10) :: lname = "Last-Name "
    CHARACTER (len=20) :: email = "Email"
    CHARACTER (len=20) :: phone = "Phone-Number"
    CHARACTER (len=3) :: age = "Age"
    CHARACTER (len=10) :: duesOwed = "Dues-Owed"
    
    ! display header
    WRITE (*, "(a10,2x,a10,1x,a20,1x,a20,1x,a3,2x,a9)") fname, lname, email, phone, age, duesOwed 
END SUBROUTINE displayHeader