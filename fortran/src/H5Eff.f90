!
! This file contains FORTRAN90 interfaces for H5E functions
!
      MODULE H5E

        USE H5FORTRAN_TYPES 
        USE H5FORTRAN_FLAGS 
      
      CONTAINS

          SUBROUTINE h5eclear_f(hdferr) 
            IMPLICIT NONE
            INTEGER, INTENT(OUT) :: hdferr  ! Error code
            INTEGER, EXTERNAL :: h5eclear_c

            hdferr = h5eclear_c()
          END SUBROUTINE h5eclear_f

          SUBROUTINE h5eprint_f(hdferr, name)
            CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: name ! File name
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5eprint_c1, h5eprint_c2 
            
            INTEGER :: namelen

            namelen = LEN(NAME)
            if (present(name)) hdferr = h5eprint_c1(name, namelen) 
            hdferr = h5eprint_c2() 
          END SUBROUTINE h5eprint_f

          SUBROUTINE h5eget_major_f(error_no, name, hdferr)
            INTEGER, INTENT(IN) :: error_no !Major error number
            CHARACTER(LEN=*), INTENT(OUT) :: name ! File name
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5eget_major_c

            hdferr = h5eget_major_c(error_no, name) 
          END SUBROUTINE h5eget_major_f

          SUBROUTINE h5eget_minor_f(error_no, name, hdferr)
            INTEGER, INTENT(IN) :: error_no !Major error number
            CHARACTER(LEN=*), INTENT(OUT) :: name ! File name
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5eget_minor_c

            hdferr = h5eget_minor_c(error_no, name) 
          END SUBROUTINE h5eget_minor_f

          SUBROUTINE h5eset_auto_f(printflag, hdferr)
            INTEGER, INTENT(IN) :: printflag  !flag to turn automatic error
                                              !printing on or off
                                              !possible values are:
                                              !printon (1)
                                              !printoff(0)
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER, EXTERNAL :: h5eset_auto_c

            hdferr = h5eset_auto_c(printflag) 
          END SUBROUTINE h5eset_auto_f

      END MODULE H5E
          
