! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by the Board of Trustees of the University of Illinois.         *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the files COPYING and Copyright.html.  COPYING can be found at the root   *
!   of the source code distribution tree; Copyright.html can be found at the  *
!   root level of an installed copy of the electronic HDF5 document set and   *
!   is linked from the top-level documents page.  It can also be found at     *
!   http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
!   access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! H5test_kind_SIZEOF.f90
!
! This fortran program generates H5fortran_detect.f90
! 
!
PROGRAM test_kind
  INTEGER :: i, j, ii, last, kind_numbers(10)
  INTEGER :: jr, jd
  last = -1
  ii = 0
  j = SELECTED_INT_KIND(18)
  DO i = 1,100
     j = SELECTED_INT_KIND(i)
     IF(j .NE. last) THEN
        IF(last .NE. -1) THEN
           ii = ii + 1
           kind_numbers(ii) = last
        ENDIF
        last = j
        IF(j .EQ. -1) EXIT
     ENDIF
  ENDDO
! Generate a program
  WRITE(*,*) "PROGRAM int_kind"
  WRITE(*,*) "WRITE(*,*) "" /*generating header file*/ """
  j = 0
  WRITE(*, "("" CALL i"", i2.2,""()"")") j
  jr = 0
  WRITE(*, "("" CALL r"", i2.2,""()"")") jr
  jd = 0
  WRITE(*, "("" CALL d"", i2.2,""()"")") jd
  DO i = 1, ii
     j = kind_numbers(i)
     WRITE(*, "("" CALL i"", i2.2,""()"")") j
  ENDDO
  WRITE(*,*) "END PROGRAM int_kind"
  j = 0
  WRITE(*, "("" SUBROUTINE i"", i2.2,""()"")") j
  WRITE(*,*)"   IMPLICIT NONE"
  WRITE(*,*)"   INTEGER :: a"
  WRITE(*,*)"   INTEGER :: a_size"
  WRITE(*,*)"   CHARACTER(LEN=2) :: ichr2"
  WRITE(*,*)"   a_size = SIZEOF(a)"
  WRITE(*,*)"   WRITE(ichr2,'(I2)') a_size"
  WRITE(*,*)'   WRITE(*,*) "#define H5_FORTRAN_HAS_NATIVE_"'//"//ADJUSTL(ichr2)"
  WRITE(*,*)"   RETURN"
  WRITE(*,*)"END SUBROUTINE"    
  jr = 0
  WRITE(*, "("" SUBROUTINE r"", i2.2,""()"")") j
  WRITE(*,*)"   IMPLICIT NONE"
  WRITE(*,*)"   REAL :: a"
  WRITE(*,*)"   INTEGER :: a_size"
  WRITE(*,*)"   CHARACTER(LEN=2) :: ichr2"
  WRITE(*,*)"   a_size = SIZEOF(a)"
  WRITE(*,*)"   WRITE(ichr2,'(I2)') a_size"
  WRITE(*,*)'   WRITE(*,*) "#define H5_FORTRAN_HAS_REAL_NATIVE_"'//"//ADJUSTL(ichr2)"
  WRITE(*,*)"   RETURN"
  WRITE(*,*)"END SUBROUTINE"
  jd = 0
  WRITE(*, "("" SUBROUTINE d"", i2.2,""()"")") jd
  WRITE(*,*)"   IMPLICIT NONE"
  WRITE(*,*)"   DOUBLE PRECISION :: a"
  WRITE(*,*)"   INTEGER :: a_size"
  WRITE(*,*)"   CHARACTER(LEN=2) :: ichr2"
  WRITE(*,*)"   a_size = SIZEOF(a)"
  WRITE(*,*)"   WRITE(ichr2,'(I2)') a_size"
  WRITE(*,*)'   WRITE(*,*) "#define H5_FORTRAN_HAS_DOUBLE_NATIVE_"'//"//ADJUSTL(ichr2)"
  WRITE(*,*)"   RETURN"
  WRITE(*,*)"END SUBROUTINE"
  DO i = 1, ii
     j = kind_numbers(i)
     WRITE(*, "("" SUBROUTINE i"", i2.2,""()"")") j
     WRITE(*,*)"   IMPLICIT NONE"
     WRITE(*,*)"   INTEGER(",j,") :: a"
     WRITE(*,*)"   INTEGER :: a_size"
     WRITE(*,*)"   CHARACTER(LEN=2) :: ichr2"
     WRITE(*,*)"   a_size = SIZEOF(a)"
     WRITE(*,*)"   WRITE(ichr2,'(I2)') a_size"
     WRITE(*,*)'   WRITE(*,*) "#define H5_FORTRAN_HAS_INTEGER_"'//"//ADJUSTL(ichr2)"
     WRITE(*,*)"   RETURN"
     WRITE(*,*)"END SUBROUTINE"
  ENDDO
END PROGRAM test_kind
              
            

