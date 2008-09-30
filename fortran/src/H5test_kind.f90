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
! H5test_kind.f90
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
  WRITE(*,*)"   INTEGER :: a = 0"
  WRITE(*,*)"   INTEGER :: a_size"
  WRITE(*,*)"   a_size = BIT_SIZE(a)"
  WRITE(*,*)"   IF (a_size .EQ. 8) THEN"
  WRITE(*,*)"       WRITE(*,*) ""#define H5_FORTRAN_HAS_NATIVE_1"" "
  WRITE(*,*)"   endif"
  WRITE(*,*)"   IF (a_size .EQ. 16) THEN"
  WRITE(*,*)"       WRITE(*,*) ""#define H5_FORTRAN_HAS_NATIVE_2"" "
  WRITE(*,*)"   endif"
  WRITE(*,*)"   IF (a_size .EQ. 32) THEN"
  WRITE(*,*)"       WRITE(*,*) ""#define H5_FORTRAN_HAS_NATIVE_4"" "
  WRITE(*,*)"   ENDIF"
  WRITE(*,*)"   IF (a_size .EQ. 64) THEN"
  WRITE(*,*)"       WRITE(*,*) ""#define H5_FORTRAN_HAS_NATIVE_8"" "
  WRITE(*,*)"   ENDIF"
  WRITE(*,*)"   IF (a_size .EQ. 128) THEN"
  WRITE(*,*)"       WRITE(*,*) ""#define H5_FORTRAN_HAS_NATIVE_16"" "
  WRITE(*,*)"   ENDIF"
  WRITE(*,*)"   RETURN"
  WRITE(*,*)"END SUBROUTINE"    
  jr = 0
  WRITE(*, "("" SUBROUTINE r"", i2.2,""()"")") j
  WRITE(*,*)"   IMPLICIT NONE"
  WRITE(*,*)"   REAL :: b(32)"
  WRITE(*,*)"   INTEGER :: a(1)"
  WRITE(*,*)"   INTEGER :: a_size"
  WRITE(*,*)"   INTEGER :: real_size"
  WRITE(*,*)"   a_size = BIT_SIZE(a(1)) ! Size in bits for integer"
  WRITE(*,*)"   real_size = (SIZE(TRANSFER(b,a))*a_size)/SIZE(b)"
  WRITE(*,*)"   IF (real_size .EQ. 32) THEN"
  WRITE(*,*)"       WRITE(*,*) ""#define H5_FORTRAN_HAS_REAL_NATIVE_4"" "
  WRITE(*,*)"   ENDIF"
  WRITE(*,*)"   IF (real_size .EQ. 64) THEN"
  WRITE(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_REAL_NATIVE_8"" "
  WRITE(*,*)"   endif"
  WRITE(*,*)"   IF (real_size .EQ. 128) THEN"
  WRITE(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_REAL_NATIVE_16"" "
  WRITE(*,*)"   ENDIF"
  WRITE(*,*)"   RETURN"
  WRITE(*,*)"END SUBROUTINE"    
  jd = 0
  WRITE(*, "("" SUBROUTINE d"", i2.2,""()"")") jd
  WRITE(*,*)"   IMPLICIT NONE"
  WRITE(*,*)"   DOUBLE PRECISION :: b=0"
  WRITE(*,*)"   INTEGER :: a(8)=0"
  WRITE(*,*)"   INTEGER :: a_size"
  WRITE(*,*)"   INTEGER :: b_size"
  WRITE(*,*)"   a_size = BIT_SIZE(a(1))"
  WRITE(*,*)"   b_size = SIZE(transfer(b,a))*a_size"
  WRITE(*,*)"   IF (b_size .EQ. 64) THEN"
  WRITE(*,*)"       WRITE(*,*) ""#define H5_FORTRAN_HAS_DOUBLE_NATIVE_8"" "
  WRITE(*,*)"   ENDIF"
  WRITE(*,*)"   IF (b_size .EQ. 128) THEN"
  WRITE(*,*)"       WRITE(*,*) ""#define H5_FORTRAN_HAS_DOUBLE_NATIVE_16"" "
  WRITE(*,*)"   ENDIF"
  WRITE(*,*)"   RETURN"
  WRITE(*,*)"END SUBROUTINE"    
  DO i = 1, ii
     j = kind_numbers(i)
     WRITE(*, "("" SUBROUTINE i"", i2.2,""()"")") j
     WRITE(*,*)"   IMPLICIT NONE"
     WRITE(*,*)"   INTEGER(",j,") :: a = 0"
     WRITE(*,*)"   INTEGER :: a_size"
     WRITE(*,*)"   a_size = BIT_SIZE(a)"
     WRITE(*,*)"   IF (a_size .EQ. 8) THEN"
     WRITE(*,*)"       WRITE(*,*) ""#define H5_FORTRAN_HAS_INTEGER_1"" "
     WRITE(*,*)"   ENDIF"
     WRITE(*,*)"   IF (a_size .EQ. 16) THEN"
     WRITE(*,*)"       WRITE(*,*) ""#define H5_FORTRAN_HAS_INTEGER_2"" "
     WRITE(*,*)"   ENDIF"
     WRITE(*,*)"   IF (a_size .EQ. 32) THEN"
     WRITE(*,*)"       WRITE(*,*) ""#define H5_FORTRAN_HAS_INTEGER_4"" "
     WRITE(*,*)"   ENDIF"
     WRITE(*,*)"   IF (a_size .EQ. 64) THEN"
     WRITE(*,*)"       WRITE(*,*) ""#define H5_FORTRAN_HAS_INTEGER_8"" "
     WRITE(*,*)"   ENDIF"
     WRITE(*,*)"   IF (a_size .EQ. 128) THEN"
     WRITE(*,*)"       WRITE(*,*) ""#define H5_FORTRAN_HAS_INTEGER_16"" "
     WRITE(*,*)"   ENDIF"
     WRITE(*,*)"   RETURN"
     WRITE(*,*)"   END SUBROUTINE"    
  ENDDO
END PROGRAM test_kind

            

