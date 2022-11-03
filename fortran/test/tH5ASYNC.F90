!****h* root/fortran/test/tH5ASYNC.F90
!
! NAME
!  tH5ASYNC.f90
!
! FUNCTION
!  Basic testing of Fortran async APIs.
!
! COPYRIGHT
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the COPYING file, which can be found at the root of the source code       *
!   distribution tree, or in https://www.hdfgroup.org/licenses.               *
!   If you do not have access to either file, you may request a copy from     *
!   help@hdfgroup.org.                                                        *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
!*****

MODULE TH5_ASYNC

  USE HDF5 ! This module contains all necessary modules
  USE TH5_MISC
  USE TH5_MISC_GEN

  IMPLICIT NONE

CONTAINS

  SUBROUTINE test_eventset(total_error)

    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: total_error

  END SUBROUTINE test_eventset

  SUBROUTINE test_async(total_error)

    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: total_error

  END SUBROUTINE test_async

END MODULE TH5_ASYNC
