!****h* root/fortran/test/tf_F08.f90
!
! NAME
!  tf_F08.f90
!
! FUNCTION
!  Contains functions that are part of the F2008 standard and needed by 
!  the hdf5 fortran tests.
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
!   This file contains "sizeof" functions that are F2008 standard compliant
!   and replace the non-standard 'SIZEOF' functions found in the file tf_F03.
!   Unfortunity we need to wrap the C_SIZEOF/STORAGE_SIZE functions to handle different
!   data types from the various tests.
!
!   F08+TS29113 requires C interoperable variable as argument for C_SIZEOF.
!   
!   This file will be build instead of tf_F03.f90 if the intrinsic fortran 
!   function C_SIZEOF/STORAGE_SIZE is found during configure.
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
     MODULE PROCEDURE H5_SIZEOF_CHR
     MODULE PROCEDURE H5_SIZEOF_I
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

    H5_SIZEOF_CMPD = C_SIZEOF(a)

  END FUNCTION H5_SIZEOF_CMPD

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: h5_sizeof_chr
!DEC$endif
  INTEGER(C_SIZE_T) FUNCTION H5_SIZEOF_CHR(a)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(in) :: a

    H5_SIZEOF_CHR = storage_size(a, c_size_t)/storage_size(c_char_'a',c_size_t)

  END FUNCTION H5_SIZEOF_CHR

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: h5_sizeof_i
!DEC$endif
  INTEGER(C_SIZE_T) FUNCTION H5_SIZEOF_I(a)
    IMPLICIT NONE
    INTEGER, INTENT(in):: a

    H5_SIZEOF_I = storage_size(a, c_size_t)/storage_size(c_char_'a',c_size_t)

  END FUNCTION H5_SIZEOF_I


!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: h5_sizeof_sp
!DEC$endif
  INTEGER(C_SIZE_T) FUNCTION H5_SIZEOF_SP(a)
    IMPLICIT NONE
    REAL(sp), INTENT(in):: a

    H5_SIZEOF_SP = storage_size(a, c_size_t)/storage_size(c_char_'a',c_size_t)

  END FUNCTION H5_SIZEOF_SP

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_TEST_DLL)
!DEC$attributes dllexport :: h5_sizeof_dp
!DEC$endif
  INTEGER(C_SIZE_T) FUNCTION H5_SIZEOF_DP(a)
    IMPLICIT NONE
    REAL(dp), INTENT(in):: a

    H5_SIZEOF_DP = storage_size(a, c_size_t)/storage_size(c_char_'a',c_size_t)

  END FUNCTION H5_SIZEOF_DP

END MODULE TH5_MISC_PROVISIONAL
