!****h* root/fortran/test/fortranlib_test_F03.f90
!
! NAME
!  fortranlib_test_F03.f90
!
! FUNCTION
!  Basic testing of Fortran API's requiring Fortran 2003
!  compliance.
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

PROGRAM fortranlibtest_F03
  
  USE HDF5
  USE THDF5_F03

  IMPLICIT NONE
  INTEGER :: total_error = 0
  INTEGER :: error
  INTEGER :: majnum, minnum, relnum
  INTEGER :: ret_total_error
  LOGICAL :: cleanup, status

  CALL h5open_f(error)

  cleanup = .TRUE.
  CALL h5_env_nocleanup_f(status)
  IF(status) cleanup=.FALSE.

  WRITE(*,'(24X,A)') '=============================='
  WRITE(*,'(24X,A)') '      FORTRAN 2003 tests      '
  WRITE(*,'(24X,A)') '=============================='
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

  ret_total_error = 0
!  PROBLEMS with C
!  CALL test_error(ret_total_error)
!  CALL write_test_status(ret_total_error, ' Test error API based on data I/O', total_error)

  WRITE(*,*)

  ret_total_error = 0
  CALL test_array_compound_atomic(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing 1-D Array of Compound Datatypes Functionality', total_error)

  ret_total_error = 0
  CALL test_array_compound_array(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing 1-D Array of Compound Array Datatypes Functionality', total_error)

  ret_total_error = 0
  CALL t_array(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing 3-D array by dataset, using C_LOC', total_error)

  ret_total_error = 0
  CALL t_enum(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing writing/reading enum dataset, using C_LOC', total_error)  

  ret_total_error = 0
  CALL t_enum_conv(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing enumeration conversions', total_error)  

  ret_total_error = 0
  CALL t_bit(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing writing/reading bitfield dataset, using C_LOC', total_error)

  ret_total_error = 0
  CALL t_opaque(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing writing/reading opaque datatypes, using C_LOC', total_error) 

  ret_total_error = 0
  CALL t_objref(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing writing/reading object references, using C_LOC', total_error)

  ret_total_error = 0
  CALL t_regref(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing writing/reading region references, using C_LOC', total_error)  

  ret_total_error = 0
  CALL t_vlen(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing writing/reading variable-length datatypes, using C_LOC', total_error)

  ret_total_error = 0
  CALL t_vlstring(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing writing/reading variable-string datatypes, using C_LOC', total_error)

  ret_total_error = 0
  CALL t_vlstring_readwrite(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing variable-string write/read, using h5dwrite_f/h5dread_f', total_error)

  ret_total_error = 0
  CALL t_string(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing writing/reading string datatypes, using C_LOC', total_error)

  ret_total_error = 0
  CALL vl_test_special_char(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing string datatypes containing control characters', total_error)

  ret_total_error = 0
  CALL test_create(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing filling functions', total_error)

  ret_total_error = 0
  CALL test_h5kind_to_type(total_error)
  CALL write_test_status(ret_total_error, ' Test function h5kind_to_type', total_error)

  ret_total_error = 0
  CALL test_array_bkg(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing Partial I/O of Array Fields in Compound Datatype Functionality', total_error)

  ret_total_error = 0
  CALL test_genprop_class_callback(ret_total_error)
  CALL write_test_status(ret_total_error, ' Test basic generic property list callback functionality', total_error)

  ret_total_error = 0
  CALL test_iter_group(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing group iteration functionality', total_error)

  ret_total_error = 0
  CALL test_nbit(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing nbit filter', total_error)

  ret_total_error = 0
  CALL external_test_offset(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing external dataset with offset', total_error)

  ret_total_error = 0
  CALL test_h5p_file_image(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing h5pset/get file image', total_error)
  
!     write(*,*)
!     write(*,*) '========================================='
!     write(*,*) 'Testing GROUP interface             '
!     write(*,*) '========================================='

  ret_total_error = 0
  CALL test_h5o_refcount(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing object functions ', total_error)

  ret_total_error = 0
  CALL obj_visit(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing object visiting functions ', total_error)

  ret_total_error = 0
  CALL obj_info(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing object info functions ', total_error)

  ret_total_error = 0
  CALL test_get_file_image(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing get file image ', total_error)

!     write(*,*)
!     write(*,*) '========================================='
!     write(*,*) 'Testing VDS                              '
!     write(*,*) '========================================='

  ret_total_error = 0
  CALL test_vds(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing vds ', total_error)

  WRITE(*,*)

  WRITE(*,*) '                  ============================================  '
  WRITE(*, fmt = '(19x, 27a)', advance='NO') ' FORTRAN tests completed with '
  WRITE(*, fmt = '(i4)', advance='NO') total_error
  WRITE(*, fmt = '(12a)' ) ' error(s) ! '
  WRITE(*,*) '                  ============================================  '
  
  CALL h5close_f(error)

  ! if errors detected, exit with non-zero code.
  IF (total_error .NE. 0) CALL h5_exit_f(1)

END PROGRAM fortranlibtest_F03


