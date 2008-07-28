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
!         write(*,*) j
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
!          write(*,*) kind_numbers(1:ii)
! Generate a program
          WRITE(*,*) "program int_kind"
          WRITE(*,*) "write(*,*) "" /*generating header file*/ """
             j = 0
             WRITE(*, "("" call i"", i2.2,""()"")") j
             jr = 0
             WRITE(*, "("" call r"", i2.2,""()"")") jr
             jd = 0
             WRITE(*, "("" call d"", i2.2,""()"")") jd
          DO i = 1, ii
             j = kind_numbers(i)
             WRITE(*, "("" call i"", i2.2,""()"")") j
          ENDDO
          WRITE(*,*) "end program int_kind"
              j = 0
             WRITE(*, "("" subroutine i"", i2.2,""()"")") j
             WRITE(*,*)"   implicit none"
             WRITE(*,*)"   integer :: a = 0"
             WRITE(*,*)"   integer :: a_size"
             WRITE(*,*)"   a_size = bit_size(a)"
             WRITE(*,*)"   if (a_size .eq. 8) then"
             WRITE(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_NATIVE_1"" "
             WRITE(*,*)"   endif"
             WRITE(*,*)"   if (a_size .eq. 16) then"
             WRITE(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_NATIVE_2"" "
             WRITE(*,*)"   endif"
             WRITE(*,*)"   if (a_size .eq. 32) then"
             WRITE(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_NATIVE_4"" "
             WRITE(*,*)"   endif"
             WRITE(*,*)"   if (a_size .eq. 64) then"
             WRITE(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_NATIVE_8"" "
             WRITE(*,*)"   endif"
             WRITE(*,*)"   if (a_size .eq. 128) then"
             WRITE(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_NATIVE_16"" "
             WRITE(*,*)"   endif"
             WRITE(*,*)"   return"
             WRITE(*,*)"   end subroutine"    
              jr = 0
             WRITE(*, "("" subroutine r"", i2.2,""()"")") j
             WRITE(*,*)"   implicit none"
             WRITE(*,*)"   real :: b(1) = 0"
             WRITE(*,*)"   integer :: a(1) = 0"
             WRITE(*,*)"   integer :: a_size"
             WRITE(*,*)"   integer :: real_size"
             WRITE(*,*)"   integer :: ab_size ! How many integers needed to hold a real"
             WRITE(*,*)"   integer :: ba_size ! How many reals needed to hold an integer"
             WRITE(*,*)"   a_size = bit_size(a(1)) ! Size in bits for integer"
             WRITE(*,*)"   ab_size = size(transfer(b,a))"
             WRITE(*,*)"   ba_size = size(transfer(a,b))"
             WRITE(*,*)"   if (ab_size .eq. ba_size) real_size=a_size"
             WRITE(*,*)"   if (ab_size .gt. ba_size) real_size=a_size*ba_size"
             WRITE(*,*)"   if (ab_size .lt. ba_size) real_size=a_size/ba_size"
             WRITE(*,*)"   if (real_size .eq. 32) then"
             WRITE(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_REAL_NATIVE_4"" "
             WRITE(*,*)"   endif"
             WRITE(*,*)"   if (real_size .eq. 64) then"
             WRITE(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_REAL_NATIVE_8"" "
             WRITE(*,*)"   endif"
             WRITE(*,*)"   if (real_size .eq. 128) then"
             WRITE(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_REAL_NATIVE_16"" "
             WRITE(*,*)"   endif"
             WRITE(*,*)"   return"
             WRITE(*,*)"   end subroutine"    
              jd = 0
             WRITE(*, "("" subroutine d"", i2.2,""()"")") jd
             WRITE(*,*)"   implicit none"
             WRITE(*,*)"   double precision :: b = 0"
             WRITE(*,*)"   integer :: a(8) = 0"
             WRITE(*,*)"   integer :: a_size"
             WRITE(*,*)"   integer :: b_size"
             WRITE(*,*)"   a_size = bit_size(a(1))"
             WRITE(*,*)"   b_size = size(transfer(b,a))*a_size"
             WRITE(*,*)"   if (b_size .eq. 64) then"
             WRITE(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_DOUBLE_NATIVE_8"" "
             WRITE(*,*)"   endif"
             WRITE(*,*)"   if (b_size .eq. 128) then"
             WRITE(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_DOUBLE_NATIVE_16"" "
             WRITE(*,*)"   endif"
             WRITE(*,*)"   return"
             WRITE(*,*)"   end subroutine"    
          DO i = 1, ii
              j = kind_numbers(i)
             WRITE(*, "("" subroutine i"", i2.2,""()"")") j
             WRITE(*,*)"   implicit none"
             WRITE(*,*)"   integer(",j,") :: a = 0"
             WRITE(*,*)"   integer :: a_size"
             WRITE(*,*)"   a_size = bit_size(a)"
             WRITE(*,*)"   if (a_size .eq. 8) then"
             WRITE(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_INTEGER_1"" "
             WRITE(*,*)"   endif"
             WRITE(*,*)"   if (a_size .eq. 16) then"
             WRITE(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_INTEGER_2"" "
             WRITE(*,*)"   endif"
             WRITE(*,*)"   if (a_size .eq. 32) then"
             WRITE(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_INTEGER_4"" "
             WRITE(*,*)"   endif"
             WRITE(*,*)"   if (a_size .eq. 64) then"
             WRITE(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_INTEGER_8"" "
             WRITE(*,*)"   endif"
             WRITE(*,*)"   if (a_size .eq. 128) then"
             WRITE(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_INTEGER_16"" "
             WRITE(*,*)"   endif"
             WRITE(*,*)"   return"
             WRITE(*,*)"   end subroutine"    
          ENDDO
          END PROGRAM
              
            

