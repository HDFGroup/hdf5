!****h* root/fortran/test/vol_connector.F90
!
! NAME
!  vol_connector.F90
!
! FUNCTION
!
!  Tests basic Fortran VOL plugin operations (registration, etc.).
!  Uses the null VOL connector (built with the testing code)
!  which is loaded as a dynamic plugin.
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

MODULE VOL_TMOD

  USE HDF5
  USE TH5_MISC
  USE TH5_MISC_GEN
  IMPLICIT NONE

  INTEGER, PARAMETER :: NATIVE_VOL_CONNECTOR_VALUE = 0
  CHARACTER(LEN=180) :: NATIVE_VOL_CONNECTOR_NAME

CONTAINS

  !-------------------------------------------------------------------------
  ! Function:    test_registration_by_name()
  !
  ! Purpose:     Tests if we can load, register, and close a VOL
  !              connector by name.
  !
  !-------------------------------------------------------------------------
  !

  SUBROUTINE test_registration_by_name(total_error)

    IMPLICIT NONE

    INTEGER, INTENT(INOUT) :: total_error
    INTEGER :: error = 0

    LOGICAL :: is_registered = .FALSE.
    INTEGER(hid_t) :: vol_id = 0, vol_id_out = 1
    CHARACTER(LEN=64) :: name
    CHARACTER(LEN=1) :: name_null
    INTEGER(SIZE_T) :: name_len
    INTEGER(hid_t) :: file_id

    ! The null VOL connector should not be registered at the start of the test
    CALL H5VLis_connector_registered_by_name_f( "FAKE_VOL_CONNECTOR_NAME", is_registered, error)
    CALL check("H5VLis_connector_registered_by_name_f",error,total_error)
    CALL VERIFY("H5VLis_connector_registered_by_name_f", is_registered, .FALSE., total_error)

    ! Register the connector by name
    CALL H5VLregister_connector_by_name_f(NATIVE_VOL_CONNECTOR_NAME, vol_id, error)
    CALL check("H5VLregister_connector_by_name_f",error,total_error)

    ! The connector should be registered now
    CALL H5VLis_connector_registered_by_name_f(NATIVE_VOL_CONNECTOR_NAME, is_registered, error)
    CALL check("H5VLis_connector_registered_by_name_f",error,total_error)
    CALL VERIFY("H5VLis_connector_registered_by_name_f", is_registered, .TRUE., total_error)

    CALL H5VLget_connector_id_by_name_f(NATIVE_VOL_CONNECTOR_NAME, vol_id_out, error)
    CALL check("H5VLget_connector_id_by_name_f",error,total_error)

    CALL H5Fcreate_f("voltest.h5",H5F_ACC_TRUNC_F, file_id, error)
    CALL check("H5F_create_f",error,total_error)

    CALL H5VLget_connector_name_f(file_id, name, error, name_len)
    CALL check("H5VLget_connector_name_f",error,total_error)
    CALL VERIFY("H5VLget_connector_name_f", INT(name_len), LEN_TRIM(NATIVE_VOL_CONNECTOR_NAME), total_error)

    CALL H5VLget_connector_name_f(file_id, name, error)
    CALL check("H5VLget_connector_name_f",error,total_error)
    CALL VERIFY("H5VLget_connector_name_f", TRIM(name), NATIVE_VOL_CONNECTOR_NAME, total_error)

    CALL H5VLget_connector_name_f(file_id, name_null, error, name_len)
    CALL check("H5VLget_connector_name_f",error,total_error)
    CALL VERIFY("H5VLget_connector_name_f", INT(name_len), LEN_TRIM(NATIVE_VOL_CONNECTOR_NAME), total_error)

    CALL H5VLget_connector_name_f(file_id, name_null, error)
    CALL check("H5VLget_connector_name_f",error,total_error)
    CALL VERIFY("H5VLget_connector_name_f", name_null, NATIVE_VOL_CONNECTOR_NAME(1:1), total_error)

    CALL H5Fclose_f(file_id, error)
    CALL check("H5Fclose_f",error,total_error)

    CALL H5VLclose_f(vol_id_out, error)
    CALL check("H5VLclose_f",error, total_error)

  END SUBROUTINE test_registration_by_name

  !-------------------------------------------------------------------------
  ! Function:    test_registration_by_value()
  !
  ! Purpose:     Tests if we can load, register, and close a VOL
  !              connector by value.
  !
  !-------------------------------------------------------------------------

  SUBROUTINE test_registration_by_value(total_error)

    IMPLICIT NONE

    INTEGER, INTENT(INOUT) :: total_error
    INTEGER :: error = 0

    LOGICAL :: is_registered = .FALSE.
    INTEGER(hid_t) :: vol_id = 0


    ! The null VOL connector should not be registered at the start of the test
    CALL H5VLis_connector_registered_by_name_f( "FAKE_VOL_CONNECTOR_NAME", is_registered, error)
    CALL check("H5VLis_connector_registered_by_name_f",error,total_error)
    CALL VERIFY("H5VLis_connector_registered_by_name_f", is_registered, .FALSE., total_error)

    ! Register the connector by value
    CALL H5VLregister_connector_by_value_f(NATIVE_VOL_CONNECTOR_VALUE, vol_id, error)
    CALL check("H5VLregister_connector_by_value_f", error, total_error)

    ! The connector should be registered now
    CALL H5VLis_connector_registered_by_name_f(NATIVE_VOL_CONNECTOR_NAME, is_registered, error)
    CALL check("H5VLis_connector_registered_by_name_f",error,total_error)
    CALL VERIFY("H5VLis_connector_registered_by_name_f", is_registered, .TRUE., total_error)

  END SUBROUTINE test_registration_by_value


  !-------------------------------------------------------------------------
  ! Function:    test_registration_by_name()
  !
  ! Purpose:     Tests if we can load, register, and close a VOL
  !              connector by name.
  !
  !-------------------------------------------------------------------------
  !

  SUBROUTINE test_registration_by_fapl(total_error)

    IMPLICIT NONE

    INTEGER, INTENT(INOUT) :: total_error
    INTEGER :: error = 0

    LOGICAL :: is_registered = .FALSE.
    INTEGER(hid_t) :: vol_id = 0, vol_id_out = 1
    INTEGER(hid_t) :: file_id
    INTEGER(hid_t) :: fapl_id
    TYPE(C_PTR) :: f_ptr

    CALL H5VLis_connector_registered_by_name_f( "FAKE_VOL_CONNECTOR_NAME", is_registered, error)

    CALL check("H5VLis_connector_registered_by_name_f",error,total_error)
    CALL VERIFY("H5VLis_connector_registered_by_name_f", is_registered, .FALSE., total_error)

    ! The null VOL connector should not be registered at the start of the test
    CALL H5VLis_connector_registered_by_name_f( "FAKE_VOL_CONNECTOR_NAME", is_registered, error)
    CALL check("H5VLis_connector_registered_by_name_f",error,total_error)
    CALL VERIFY("H5VLis_connector_registered_by_name_f", is_registered, .FALSE., total_error)

    CALL H5VLregister_connector_by_name_f(NATIVE_VOL_CONNECTOR_NAME, vol_id, error)
    CALL check("H5VLregister_connector_by_name_f",error,total_error)

    ! The connector should be registered now
    CALL H5VLis_connector_registered_by_name_f(NATIVE_VOL_CONNECTOR_NAME, is_registered, error)
    CALL check("H5VLis_connector_registered_by_name_f",error,total_error)
    CALL VERIFY("H5VLis_connector_registered_by_name_f", is_registered, .TRUE., total_error)

    ! Register the connector
    CALL H5Pcreate_f(H5P_FILE_ACCESS_F, fapl_id, error)
    CALL check("H5Pcreate_f",error,total_error)

    IF(TRIM(NATIVE_VOL_CONNECTOR_NAME) .EQ. "native")THEN
       CALL H5Pset_vol_f(fapl_id, vol_id, error)
       CALL check("H5Pset_vol_f",error,total_error)

       CALL H5Pget_vol_id_f(fapl_id, vol_id_out, error)
       CALL check("H5Pget_vol_id_f",error,total_error)
       CALL VERIFY("H5Pget_vol_id_f", vol_id_out, vol_id, total_error)

       f_ptr = C_NULL_PTR
       CALL H5Pset_vol_f(fapl_id, vol_id, error, f_ptr)
       CALL check("H5Pset_vol_f",error,total_error)

       CALL H5Pget_vol_id_f(fapl_id, vol_id_out, error)
       CALL check("H5Pget_vol_id_f",error,total_error)
       CALL VERIFY("H5Pget_vol_id_f", vol_id_out, vol_id, total_error)
    ENDIF

    CALL H5VLget_connector_id_by_name_f(NATIVE_VOL_CONNECTOR_NAME, vol_id_out, error)
    CALL check("H5VLget_connector_id_by_name_f",error,total_error)
    CALL VERIFY("H5VLget_connector_id_by_name_f", vol_id_out, vol_id, total_error)
    CALL H5Fcreate_f("voltest.h5",H5F_ACC_TRUNC_F, file_id, error, H5P_DEFAULT_F, fapl_id)

    CALL check("H5F_create_f",error,total_error)

    CALL H5VLclose_f(vol_id_out, error)
    CALL check("H5VLclose_f",error, total_error)

    CALL H5VLclose_f(vol_id, error)
    CALL check("H5VLclose_f",error, total_error)

    CALL H5Fclose_f(file_id, error)
    CALL check("H5Fclose_f",error,total_error)

    CALL H5Pclose_f(fapl_id, error)
    CALL check("H5Pclose_f",error,total_error)

  END SUBROUTINE test_registration_by_fapl


END MODULE VOL_TMOD


PROGRAM vol_connector

  USE VOL_TMOD

  IMPLICIT NONE
  INTEGER :: total_error = 0
  INTEGER :: error
  INTEGER :: ret_total_error
  LOGICAL :: cleanup, status
  CHARACTER(LEN=32) :: VOL_CONNECTOR_ENV
  INTEGER :: LEN = 0
  INTEGER :: CONN_NAME_LEN

  CALL h5open_f(error)
  cleanup = .TRUE.
  CALL h5_env_nocleanup_f(status)
  IF(status) cleanup=.FALSE.

  WRITE(*,'(18X,A)') '=============================='
  WRITE(*,'(24X,A)') 'FORTRAN VOL tests'
  WRITE(*,'(18X,A)') '=============================='

  WRITE(*,'(A)') "Testing VOL connector plugin functionality."

  ! Check to see if the VOL connector was set with an env variable
  CALL GET_ENVIRONMENT_VARIABLE("HDF5_VOL_CONNECTOR", VOL_CONNECTOR_ENV, LEN)
  CONN_NAME_LEN = INDEX(VOL_CONNECTOR_ENV, ' ')
  IF(LEN.NE.0)THEN
     NATIVE_VOL_CONNECTOR_NAME = TRIM(VOL_CONNECTOR_ENV(1:CONN_NAME_LEN))
  ELSE
     NATIVE_VOL_CONNECTOR_NAME = "native"
  ENDIF

  ret_total_error = 0
  CALL test_registration_by_name(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing VOL registration by name', total_error)

  ret_total_error = 0
  CALL test_registration_by_value(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing VOL registration by value', total_error)

  ret_total_error = 0
  CALL test_registration_by_fapl(ret_total_error)
  CALL write_test_status(ret_total_error, ' Testing VOL registration by fapl', total_error)

  WRITE(*, fmt = '(/18X,A)') '============================================'
  WRITE(*, fmt = '(19X, A)', advance='NO') ' FORTRAN VOL tests completed with '
  WRITE(*, fmt = '(I4)', advance='NO') total_error
  WRITE(*, fmt = '(A)' ) ' error(s) ! '
  WRITE(*,'(18X,A)') '============================================'

  CALL h5close_f(error)

  ! if errors detected, exit with non-zero code.
  IF (total_error .NE. 0) CALL h5_exit_f(1)

END PROGRAM vol_connector
