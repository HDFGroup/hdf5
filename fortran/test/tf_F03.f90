!****h* root/fortran/test/tf_F03.f90
!
! NAME
!  tf_F03.f90
!
! FUNCTION
!  Contains functions that are part of the F2003 standard, and are not F2008 compliant.
!  Needed by the hdf5 fortran tests.
!
! COPYRIGHT
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   Copyright by the Board of Trustees of the University of Illinois.         *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the files COPYING and Copyright.html.  COPYING can be found at the root   *
!   of the source code distribution tree; Copyright.html can be found at the  *
!   root level of an installed copy of the electronic HDF5 document set and   *
!   is linked from the top-level documents page.  It can also be found at     *
!   http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
!   access to either file, you may request a copy from help@hdfgroup.org.     *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! CONTAINS SUBROUTINES
!   H5_SIZEOF
!
! NOTES
!   The Sun/Oracle compiler has the following restrictions on the SIZEOF intrinsic function:
!
!     "The SIZEOF intrinsic cannot be applied to arrays of an assumed size, characters of a 
!      length that is passed, or subroutine calls or names. SIZEOF returns default INTEGER*4 data. 
!      If compiling for a 64-bit environment, the compiler will issue a warning if the result overflows 
!      the INTEGER*4 data range. To use SIZEOF in a 64-bit environment with arrays larger 
!      than the INTEGER*4 limit (2 Gbytes), the SIZEOF function and 
!      the variables receiving the result must be declared INTEGER*8."
!
!    Thus, we can not overload the H5_SIZEOF function to handle arrays (as used in tH5P_F03.f90), or
!    characters that do not have a set length (as used in tH5P_F03.f90), sigh...
!
!*****
MODULE TH5_MISC_PROVISIONAL

  USE ISO_C_BINDING
  IMPLICIT NONE

  INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(5)  ! This should map to REAL*4 on most modern processors
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(10) ! This should map to REAL*8 on most modern processors

  ! generic compound datatype
  TYPE, BIND(C) :: comp_datatype
    REAL :: a
    INTEGER :: x
    DOUBLE PRECISION :: y
    CHARACTER(LEN=1) :: z
  END TYPE comp_datatype

  PUBLIC :: H5_SIZEOF
  INTERFACE H5_SIZEOF
     MODULE PROCEDURE H5_SIZEOF_CMPD
     MODULE PROCEDURE H5_SIZEOF_I, H5_SIZEOF_CHR   
     MODULE PROCEDURE H5_SIZEOF_SP,H5_SIZEOF_DP
  END INTERFACE

CONTAINS

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: h5_sizeof_cmpd
 !DEC$endif
  INTEGER(C_SIZE_T) FUNCTION H5_SIZEOF_CMPD(a)
    IMPLICIT NONE
    TYPE(comp_datatype), INTENT(in) :: a

    H5_SIZEOF_CMPD = SIZEOF(a)

  END FUNCTION H5_SIZEOF_CMPD

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: h5_sizeof_chr
!DEC$endif
  INTEGER(C_SIZE_T) FUNCTION H5_SIZEOF_CHR(a)
    IMPLICIT NONE
    CHARACTER(LEN=1), INTENT(in):: a

    H5_SIZEOF_CHR = SIZEOF(a)

  END FUNCTION H5_SIZEOF_CHR

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: h5_sizeof_i
!DEC$endif
  INTEGER(C_SIZE_T) FUNCTION H5_SIZEOF_I(a)
    IMPLICIT NONE
    INTEGER, INTENT(in):: a

    H5_SIZEOF_I = SIZEOF(a)

  END FUNCTION H5_SIZEOF_I

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: h5_sizeof_sp
!DEC$endif
  INTEGER(C_SIZE_T) FUNCTION H5_SIZEOF_SP(a)
    IMPLICIT NONE
    REAL(sp), INTENT(in):: a

    H5_SIZEOF_SP = SIZEOF(a)

  END FUNCTION H5_SIZEOF_SP

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: h5_sizeof_dp
!DEC$endif
  INTEGER(C_SIZE_T) FUNCTION H5_SIZEOF_DP(a)
    IMPLICIT NONE
    REAL(dp), INTENT(in):: a

    H5_SIZEOF_DP = SIZEOF(a)

  END FUNCTION H5_SIZEOF_DP

END MODULE TH5_MISC_PROVISIONAL
