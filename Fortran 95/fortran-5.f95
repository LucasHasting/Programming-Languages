PROGRAM derivedType
TYPE :: ACM_Member
    CHARACTER (len=10) :: firstName
    CHARACTER (len=10) :: lastName
    CHARACTER (len=15) :: email
    CHARACTER (len=20) :: phone
    INTEGER :: age
    REAL :: duesOwed
END TYPE ACM_Member

TYPE (ACM_Member) :: student(10)
student(1) = ACM_Member("John", "Doe", "jdoe@mail.com", "222-222-2222", 21, 5.25)

WRITE (*, *) "Free Format: ", student(1)

WRITE (*, *) student(1)%firstName

END PROGRAM derivedType