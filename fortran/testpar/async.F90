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
  USE ISO_C_BINDING

  IMPLICIT NONE

CONTAINS

  SUBROUTINE test_eventset(total_error)

    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: total_error
#if 0
    INTEGER(HID_T)  :: es_id
    INTEGER         :: error
    INTEGER(SIZE_T) :: count
    INTEGER(SIZE_T) :: num_errs
    LOGICAL         :: err_occurred
    INT(C_INT64_t)  :: num_ops

    CALL H5ESwait_f(H5ES_NONE_F, error)
    CALL check("H5ESwait_f", error, total_error)

    ! Create an event set
    CALL H5EScreate_f(es_id, error)
    CALL check("H5EScreate_f", error, total_error)

    ! Query the # of events in empty event set
    count = 0;
    CALL H5ESget_count_f(es_id, count, error)
    CALL check("H5ESget_count_f", error, total_error)
    CALL VERIFY("H5ESget_count_f", count, INT(0,size_t), total_error)

    ! Check for errors
    CALL H5ESget_err_status_f(es_id, err_occurred, error)
    CALL check("H5ESget_err_status_f", error, total_error)
    CALL VERIFY("H5ESget_err_status_f", error_occurred, .FALSE., total_error)

    ! Check errors count
    CALL H5ESget_err_count_f(es_id, num_errs, hdferr)
    CALL check("H5ESget_err_count_f", error, total_error)
    CALL VERIFY("H5ESget_err_count_f", num_errs, INT(0,size_t), total_error)

    ! Check errors count
    CALL H5ESget_op_counter_f(es_id, num_ops, error)
    CALL check("H5ESget_op_counter_f", error, total_error)
    CALL VERIFY("H5ESget_op_counter_f", num_ops, INT(0,C_INT64_t), total_error)
#endif
  END SUBROUTINE test_eventset

  SUBROUTINE test_async(total_error)

    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: total_error

    INTEGER(HID_T) :: vol_id
    INTEGER :: hdferr
    LOGICAL :: registered

    ! check if ASYNC VOL is available)
    CALL H5VLis_connector_registered_by_name_f("async", registered,  hdferr)
    CALL check("H5VLis_connector_registered_by_name_f", hdferr, total_error)

    IF(.NOT.registered)THEN

       ! check if the DAOS VOL is available
       CALL H5VLis_connector_registered_by_name_f("daos", registered,  hdferr)
       CALL check("H5VLis_connector_registered_by_name_f", hdferr, total_error)

       IF(.NOT.registered)THEN
          ! No async compatible VOL found, skippin test
          total_error = -1
          RETURN
       ENDIF
    ENDIF

 !   CALL H5VLclose_f(vol_id, hdferr)
 !   CALL check("H5VLclose_f", hdferr, total_error)

  END SUBROUTINE test_async

END MODULE TH5_ASYNC
