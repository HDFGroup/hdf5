!> @defgroup FH5ES Fortran Event Set (H5ES) Interface
!!
!! @see H5ES, C-API
!!
!! @see @ref H5ES_UG, User Guide
!!
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

MODULE H5ES

  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR, C_CHAR, C_INT64_T, C_BOOL
  USE H5GLOBAL
  IMPLICIT NONE

#if 0
!> @brief H5ES_err_info_t derived type
  TYPE :: H5ES_err_info_t

    CHARACTER(KIND=C_CHAR, LEN=:), POINTER :: api_name !< Name of HDF5 API routine called
    CHARACTER(KIND=C_CHAR, LEN=:), POINTER :: api_args !< "Argument string" for arguments to HDF5 API routine called

    ! Application info
    CHARACTER(KIND=C_CHAR, LEN=:), POINTER :: app_file_name !< Name of source file where the HDF5 API routine was called
    CHARACTER(KIND=C_CHAR, LEN=:), POINTER :: app_func_name !< Name of function where the HDF5 API routine was called
    INTEGER(C_INT)   :: app_line_num                        !< Line # of source file where the HDF5 API routine was called

    ! Operation info
    INTEGER(C_INT64_T) :: op_ins_count  !< Counter of operation's insertion into event set
    INTEGER(C_INT64_T) :: op_ins_ts     !< Timestamp for when the operation was inserted into the event set
    INTEGER(C_INT64_T) :: op_exec_ts    !< Timestamp for when the operation began execution
    INTEGER(C_INT64_T) :: op_exec_time  !< Execution time for operation (in ns)

  END TYPE H5ES_err_info_t
#endif

CONTAINS

  SUBROUTINE H5EScreate_f(es_id, hdferr)
    IMPLICIT NONE

    INTEGER(HID_T) :: es_id
    INTEGER        :: hdferr

    INTERFACE
       INTEGER(HID_T) FUNCTION H5EScreate() BIND(C,NAME='H5EScreate')
         IMPORT :: HID_T
       END FUNCTION H5EScreate
    END INTERFACE

    es_id = H5EScreate()

    hdferr = 0
    IF(es_id.LT.0) hdferr = -1

  END SUBROUTINE H5EScreate_f

  SUBROUTINE H5ESinsert_request_f(es_id, connector_id, request, hdferr)
    IMPLICIT NONE

    INTEGER(HID_T) :: es_id
    INTEGER(HID_T) :: connector_id
    TYPE(C_PTR)    :: request
    INTEGER        :: hdferr

    INTERFACE
       INTEGER FUNCTION H5ESinsert_request(es_id, connector_id, request) BIND(C,NAME='H5ESinsert_request')
         IMPORT :: HID_T, C_PTR
         INTEGER(HID_T), VALUE :: es_id
         INTEGER(HID_T), VALUE :: connector_id
         TYPE(C_PTR)   , VALUE :: request
       END FUNCTION H5ESinsert_request
    END INTERFACE

    hdferr = H5ESinsert_request(es_id, connector_id, request)

  END SUBROUTINE H5ESinsert_request_f

  SUBROUTINE H5ESget_count_f(es_id, count, hdferr)
    IMPLICIT NONE

    INTEGER(hid_t)  :: es_id
    INTEGER(size_t) :: count
    INTEGER         :: hdferr

    INTERFACE
       INTEGER FUNCTION H5ESget_count(es_id, count) BIND(C,NAME='H5ESget_count')
         IMPORT :: HID_T, SIZE_T
         INTEGER(HID_T), VALUE  :: es_id
         INTEGER(SIZE_T), VALUE :: count
       END FUNCTION H5ESget_count
    END INTERFACE

    hdferr = H5ESget_count(es_id, count)

  END SUBROUTINE H5ESget_count_f

  SUBROUTINE H5ESget_op_counter_f(es_id, op_counter, hdferr)

    IMPLICIT NONE

    INTEGER(HID_T)     :: es_id
    INTEGER(C_INT64_T) :: op_counter
    INTEGER            :: hdferr

    INTERFACE
       INTEGER FUNCTION H5ESget_op_counter(es_id, op_counter) BIND(C,NAME='H5ESget_op_counter')
         IMPORT :: HID_T, C_INT64_T
         INTEGER(HID_T),     VALUE :: es_id
         INTEGER(C_INT64_T), VALUE :: op_counter
       END FUNCTION H5ESget_op_counter
    END INTERFACE

    hdferr = H5ESget_op_counter(es_id, op_counter)

  END SUBROUTINE H5ESget_op_counter_f

  SUBROUTINE H5ESwait_f(es_id, timeout, num_in_progress, op_failed, hdferr)

    IMPLICIT NONE

    INTEGER(HID_T)     :: es_id
    INTEGER(C_INT64_T) :: timeout
    INTEGER(SIZE_T)    :: num_in_progress
    LOGICAL            :: op_failed
    INTEGER            :: hdferr

    LOGICAL(C_BOOL)    :: op_failed_c = .FALSE.

    INTERFACE
       INTEGER FUNCTION H5ESwait(es_id, timeout, num_in_progress, op_failed) BIND(C,NAME='H5ESwait')
         IMPORT :: HID_T, C_INT64_T, SIZE_T, C_BOOL
         INTEGER(HID_T)    , VALUE :: es_id
         INTEGER(C_INT64_T), VALUE :: timeout
         INTEGER(SIZE_T)   , VALUE :: num_in_progress
         LOGICAL(C_BOOL)   , VALUE :: op_failed
       END FUNCTION H5ESwait
    END INTERFACE

    hdferr = H5ESwait(es_id, timeout, num_in_progress, op_failed_c)

    ! Transfer value of C c_bool type to Fortran LOGICAL
    op_failed = op_failed_c

  END SUBROUTINE H5ESwait_f

  SUBROUTINE H5EScancel_f(es_id, num_not_canceled, op_failed, hdferr)

    IMPLICIT NONE

    INTEGER(hid_t)  :: es_id
    INTEGER(size_t) :: num_not_canceled
    LOGICAL         :: op_failed
    INTEGER         :: hdferr

    LOGICAL(C_BOOL) :: op_failed_c = .FALSE.

    INTERFACE
       INTEGER FUNCTION H5EScancel(es_id, num_not_canceled, op_failed) BIND(C,NAME='H5EScancel')
         IMPORT :: HID_T, SIZE_T, C_BOOL
         INTEGER(HID_T)    , VALUE :: es_id
         INTEGER(SIZE_T)   , VALUE :: num_not_canceled
         LOGICAL(C_BOOL)   , VALUE :: op_failed
       END FUNCTION H5EScancel
    END INTERFACE

    hdferr = H5EScancel(es_id, num_not_canceled, op_failed_c)

    ! Transfer value of C c_bool type to Fortran LOGICAL
    op_failed = op_failed_c

  END SUBROUTINE H5EScancel_f


  SUBROUTINE H5ESget_err_status_f(es_id, err_status, hdferr)

    IMPLICIT NONE

    INTEGER(hid_t) :: es_id
    LOGICAL        :: err_status
    INTEGER        :: hdferr

    LOGICAL(C_BOOL) :: err_status_c = .FALSE.

    INTERFACE
       INTEGER FUNCTION H5ESget_err_status(es_id, err_status) BIND(C,NAME='H5ESget_err_status')
         IMPORT :: HID_T, C_BOOL
         INTEGER(HID_T) , VALUE :: es_id
         LOGICAL(C_BOOL), VALUE :: err_status
       END FUNCTION H5ESget_err_status
    END INTERFACE

    hdferr = H5ESget_err_status(es_id, err_status_c)

    ! Transfer value of C c_bool type to Fortran LOGICAL
    err_status = err_status_c

  END SUBROUTINE H5ESget_err_status_f

  SUBROUTINE H5ESget_err_count_f(es_id, num_errs, hdferr)

    IMPLICIT NONE

    INTEGER(HID_T)  :: es_id
    INTEGER(SIZE_T) :: num_errs
    INTEGER         :: hdferr

    INTERFACE
       INTEGER FUNCTION H5ESget_err_count(es_id, num_errs) BIND(C,NAME='H5ESget_err_count')
         IMPORT :: HID_T, SIZE_T
         INTEGER(HID_T) , VALUE :: es_id
         INTEGER(SIZE_T), VALUE :: num_errs
       END FUNCTION H5ESget_err_count
    END INTERFACE

    hdferr = H5ESget_err_count(es_id, num_errs)

  END SUBROUTINE H5ESget_err_count_f
#if 0
  SUBROUTINE H5ESget_err_info_f(es_id, num_err_info, err_info, num_cleared, hdferr)

    IMPLICIT NONE

    INTEGER(HID_T)        :: es_id
    INTEGER(SIZE_T)       :: num_err_info
    TYPE(H5ES_err_info_t) :: err_info
    INTEGER(SIZE_T)       :: num_cleared
    INTEGER               :: hdferr

    INTERFACE
       INTEGER FUNCTION H5ESget_err_info(es_id, num_err_info, err_info, num_cleared) BIND(C,NAME='H5ESget_err_info')
         IMPORT :: HID_T, SIZE_T, H5ES_err_info_t
         INTEGER(HID_T)       , VALUE :: es_id
         INTEGER(SIZE_T)      , VALUE :: num_err_info
         TYPE(H5ES_err_info_t), VALUE :: err_info
         INTEGER(SIZE_T)      , VALUE :: num_cleared
       END FUNCTION H5ESget_err_info
    END INTERFACE

    hdferr = H5ESget_err_info(es_id, num_err_info, err_info, num_cleared)

  END SUBROUTINE H5ESget_err_info_f
#endif

  SUBROUTINE H5ESclose_f(es_id, hdferr)

    IMPLICIT NONE

    INTEGER(HID_T) :: es_id
    INTEGER        :: hdferr

    INTERFACE
       INTEGER FUNCTION H5ESclose(es_id) BIND(C,NAME='H5ESclose')
         IMPORT :: HID_T
         INTEGER(HID_T) :: es_id
       END FUNCTION H5ESclose
    END INTERFACE

    hdferr = H5ESclose(es_id)

  END SUBROUTINE H5ESclose_f

END MODULE H5ES
