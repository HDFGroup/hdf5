!****h* root/fortran/test/fortranlib_test_1_8.f90
!
! NAME
!  fortranlib_test_1_8.f90
!
! FUNCTION
!  Basic testing of Fortran API's introduced in 1.8 release.
!
! COPYRIGHT
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   Copyright by the Board of Trustees of the University of Illinois.         *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the COPYING file, which can be found at the root of the source code       *
!   distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
!   If you do not have access to either file, you may request a copy from     *
!   help@hdfgroup.org.                                                        *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
!*****

PROGRAM fortranlibtest

  USE HDF5
  USE THDF5_1_8
  USE TH5_MISC
  IMPLICIT NONE
  INTEGER :: total_error = 0
  INTEGER :: error
  INTEGER :: ret_total_error
  INTEGER :: majnum, minnum, relnum
  LOGICAL :: cleanup, status

  CALL h5open_f(error)

  cleanup = .TRUE.
  CALL h5_env_nocleanup_f(status)
  IF(status) cleanup=.FALSE.

  WRITE(*,*) '                       ==========================                            '
  WRITE(*,*) '                              FORTRAN 1.8 tests '
  WRITE(*,*) '                       ==========================                            '
  CALL h5get_libversion_f(majnum, minnum, relnum, total_error)
  IF(total_error .EQ. 0) THEN
     WRITE(*, '(" FORTRANLIB_TEST is linked with HDF5 Library version ")', advance="NO")
     WRITE(*, '(I0)', advance="NO") majnum
     WRITE(*, '(".")', advance="NO")
     WRITE(*, '(I0)', advance="NO") minnum
     WRITE(*, '(" release ")', advance="NO")
     WRITE(*, '(I0)') relnum
  ELSE
     total_error = total_error + 1
  ENDIF
  WRITE(*,*)

  CALL h5eset_auto_f(0, ret_total_error)
  IF(ret_total_error.NE.0) &
       CALL write_test_status(ret_total_error, &
       ' h5eset_auto_f', &
       total_error)

  ret_total_error = 0
  CALL attribute_test_1_8(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing attributes', &
       total_error)

  ret_total_error = 0
  CALL group_test(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing groups', &
       total_error)

  ret_total_error = 0
  CALL test_h5o(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing object interface', &
       total_error)

  ret_total_error = 0
  CALL dtransform(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing data transform', &
       total_error)

  ret_total_error = 0
  CALL test_h5s_encode(ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing dataspace encoding and decoding', &
       total_error)

  ret_total_error = 0
  CALL test_scaleoffset(cleanup, ret_total_error )
  CALL write_test_status(ret_total_error, &
       ' Testing scaleoffset filter', &
       total_error)

  ret_total_error = 0
  CALL test_genprop_basic_class(ret_total_error )
  CALL write_test_status(ret_total_error, &
       ' Testing basic generic property list class creation functionality', &
       total_error)

  WRITE(*,*)

  WRITE(*,*) '                  ============================================  '
  WRITE(*, fmt = '(19x, 27a)', advance='NO') ' FORTRAN tests completed with '
  WRITE(*, fmt = '(i4)', advance='NO') total_error
  WRITE(*, fmt = '(12a)' ) ' error(s) ! '
  WRITE(*,*) '                  ============================================  '

  CALL h5close_f(error)

  ! if errors detected, exit with non-zero code.
  IF (total_error .NE. 0) CALL h5_exit_f (1)

END PROGRAM fortranlibtest
