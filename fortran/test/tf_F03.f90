!****h* root/fortran/test/tf_F08.f90
!
! NAME
!  tf_F08.f90
!
! FUNCTION
!  Contains Functions that are part of the F2008 standard and needed by 
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
!*****
MODULE TH5_MISC_PROVISIONAL
  IMPLICIT NONE

  INTEGER, PARAMETER :: sp = KIND(0.0)
  INTEGER, PARAMETER :: dp = KIND(0.D0)

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
     MODULE PROCEDURE H5_SIZEOF_I, H5_SIZEOF_IV     
     MODULE PROCEDURE H5_SIZEOF_SP,H5_SIZEOF_DP
  END INTERFACE

CONTAINS

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5_sizeof_cmpd
!DEC$endif
  INTEGER(C_SIZE_T) FUNCTION H5_SIZEOF_CMPD(a)
    IMPLICIT NONE
    TYPE(comp_datatype), INTENT(in) :: a

    H5_SIZEOF_CMPD = SIZEOF(a)

  END FUNCTION H5_SIZEOF_CMPD

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5_sizeof_chr
!DEC$endif
  INTEGER(C_SIZE_T) FUNCTION H5_SIZEOF_CHR(a)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(in):: a

    H5_SIZEOF_CHR = SIZEOF(a)

  END FUNCTION H5_SIZEOF_CHR

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5_sizeof_i
!DEC$endif
  INTEGER(C_SIZE_T) FUNCTION H5_SIZEOF_I(a)
    IMPLICIT NONE
    INTEGER, INTENT(in):: a

    H5_SIZEOF_I = SIZEOF(a)

  END FUNCTION H5_SIZEOF_I

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5_sizeof_iv
!DEC$endif
  INTEGER(C_SIZE_T) FUNCTION H5_SIZEOF_IV(a)
    IMPLICIT NONE
    INTEGER, DIMENSION(:), INTENT(in):: a

    H5_SIZEOF_IV = SIZEOF(a)

  END FUNCTION H5_SIZEOF_IV

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5_sizeof_sp
!DEC$endif
  INTEGER(C_SIZE_T) FUNCTION H5_SIZEOF_SP(a)
    IMPLICIT NONE
    REAL(sp), INTENT(in):: a

    H5_SIZEOF_SP = SIZEOF(a)

  END FUNCTION H5_SIZEOF_SP

!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5_sizeof_dp
!DEC$endif
  INTEGER(C_SIZE_T) FUNCTION H5_SIZEOF_DP(a)
    IMPLICIT NONE
    REAL(dp), INTENT(in):: a

    H5_SIZEOF_DP = SIZEOF(a)

  END FUNCTION H5_SIZEOF_DP

END MODULE TH5_MISC_PROVISIONAL
