!
! 
!    This module contains check subroutine which is used in
!    all the fortran h5 test files
!

         SUBROUTINE check(string,error,total_error)
            CHARACTER(LEN=*) :: string
            INTEGER :: error, total_error
            if (error .lt. 0) then
                total_error=total_error+1
                write(*,*) string, " failed"
            endif
            RETURN
         END SUBROUTINE check   

