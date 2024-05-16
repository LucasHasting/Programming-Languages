PROGRAM readFile
    IMPLICIT NONE
    character(len=20) :: filename
    integer :: nvals, status
    real :: value
    
    nvals = 0
    write (*, *) "Input file name: "
    read (*, *) filename
    write (*, *) "The input file name is: ", filename
    
    open (unit=3, file=filename, status="old", action="read", iostat=status)
    openif: if (status==0) then 
        readloop: do
            read(3, *, iostat=status) value
            if (status /= 0) exit
            nvals = nvals + 1
            write (*, *) nvals, value
        end do readloop
        
        readif: if (status > 0) then
            write (*, *) "An error occurred reading line: ", nvals+1
        else
            write (*, *) "End of file reached. Numbers read: ", nvals
        end if readif
        
    else openif
        write (*, *) "Error opening file. IOSTAT= ", status
    end if openif
    
    close(unit=3)

End Program readFile