!****h* root/fortran/test/fortranlib_test.f90
!
! NAME
!  fortranlib_test.f90
!
! FUNCTION
!  Basic testing of Fortran API's functionality.
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

!#include <H5config_f.inc>

PROGRAM fortranlibtest

  USE HDF5
  USE THDF5

  IMPLICIT NONE
  INTEGER :: total_error = 0
  INTEGER :: error
  INTEGER :: majnum, minnum, relnum
  LOGICAL :: szip_flag
  INTEGER :: ret_total_error
  LOGICAL :: cleanup, status

  CALL h5open_f(error)

  cleanup = .TRUE.
  CALL h5_env_nocleanup_f(status)
  IF(status) cleanup=.FALSE.

  WRITE(*,*) '                       ==========================                            '
  WRITE(*,*) '                              FORTRAN tests '
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

!      CALL h5check_version_f(1,4,4,total_error)
!      '========================================='
!      'Testing FILE Interface                   '
!      '========================================='

  ret_total_error = 0
  CALL mountingtest(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Mounting test', total_error)

  ret_total_error = 0
  CALL reopentest(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Reopen test', total_error)

  ret_total_error = 0
  CALL file_close(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' File open/close test', total_error)

  ret_total_error = 0
  CALL file_space("file_space",cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' File free space test', total_error)

!     
!      '========================================='
!      'Testing DATASET Interface                '
!      '========================================='

  ret_total_error = 0
  CALL datasettest(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Dataset test', total_error)

  ret_total_error = 0
  CALL extenddsettest(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Extendible dataset test', total_error)

  CALL test_userblock_offset(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Dataset offset with user block', total_error)

!     
!      '========================================='
!      'Testing DATASPACE Interface             '
!      '========================================='

  ret_total_error = 0
  CALL dataspace_basic_test(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Basic dataspace test', total_error)

!     
!      '========================================='
!      'Testing REFERENCE Interface              '
!      '========================================='

  ret_total_error = 0
  CALL refobjtest(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Reference to object test', total_error)

  ret_total_error = 0
  CALL refregtest(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Reference to dataset region test', total_error)

!     
!      '========================================='
!      'Testing selection functionalities        '
!      '========================================='

  ret_total_error = 0
  CALL test_basic_select(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Basic selection test', total_error)


  ret_total_error = 0
  CALL test_select_hyperslab( cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Hyperslab selection test', total_error)

  ret_total_error = 0
  CALL test_select_element(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Element selection test', total_error)

  ret_total_error = 0
  CALL test_select_point(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Element selection functions test ', total_error)

  ret_total_error = 0
  CALL test_select_combine(ret_total_error)
  CALL write_test_status(ret_total_error, ' Selection combinations test ', total_error)

  ret_total_error = 0
  CALL test_select_bounds(ret_total_error)
  CALL write_test_status(ret_total_error, ' Selection bounds test ', total_error)
  
!     
!      '========================================='
!      'Testing DATATYPE interface               '
!      '========================================='
  ret_total_error = 0
  CALL basic_data_type_test(ret_total_error)
  CALL write_test_status(ret_total_error, ' Basic datatype test', total_error)

  ret_total_error = 0
  CALL compoundtest(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Compound datatype test', total_error)

  ret_total_error = 0
  CALL enumtest(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Enum datatype test', total_error)

  ret_total_error = 0
  CALL test_derived_flt(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Derived float datatype test', total_error)

!     
!      '========================================='
!      'Testing PROPERTY interface               '
!      '========================================='

  ret_total_error = 0
  CALL external_test(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' External dataset test', total_error)

  ret_total_error = 0
  CALL multi_file_test(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Multi file driver test', total_error)

  ret_total_error = 0
  CALL test_chunk_cache (cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Dataset chunk cache configuration', total_error)

!     
!      '========================================='
!      'Testing ATTRIBUTE interface              '
!      '========================================='

  ret_total_error = 0
  CALL attribute_test(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Attribute test', total_error)

!     
!      '========================================='
!      'Testing IDENTIFIER interface             '
!      '========================================='

  ret_total_error = 0
  CALL identifier_test(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Identifier test', total_error)

  ret_total_error = 0
  CALL filters_test(ret_total_error)
  CALL write_test_status(ret_total_error, ' Filters test', total_error)

  ret_total_error = 0
  CALL szip_test(szip_flag, cleanup, ret_total_error)

  IF (.NOT. szip_flag) THEN ! test not available
     CALL write_test_status(-1, ' SZIP filter test', total_error)
  ELSE
     CALL write_test_status(ret_total_error, ' SZIP filter test', total_error)
  ENDIF

!     
!      '========================================='
!      'Testing GROUP interface                  '
!      '========================================='

  ret_total_error = 0
  CALL group_test(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Group test', total_error)

  ret_total_error = 0
  CALL error_report_test(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Error test', total_error)

  ret_total_error = 0
  CALL vl_test_integer(cleanup, ret_total_error)
  CALL vl_test_real(cleanup, ret_total_error)
  CALL vl_test_string(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' VL test', total_error)

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
