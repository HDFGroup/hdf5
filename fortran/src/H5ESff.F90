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

!>
!! \ingroup FH5ES
!!
!! \brief Creates an event set.
!!
!! \param es_id  \es_id
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5EScreate()
!!
  SUBROUTINE H5EScreate_f(es_id, hdferr)
    IMPLICIT NONE

    INTEGER(HID_T), INTENT(OUT) :: es_id
    INTEGER       , INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(HID_T) FUNCTION H5EScreate() BIND(C,NAME='H5EScreate')
         IMPORT :: HID_T
       END FUNCTION H5EScreate
    END INTERFACE

    es_id = H5EScreate()

    hdferr = 0
    IF(es_id.LT.0) hdferr = -1

  END SUBROUTINE H5EScreate_f
!>
!! \ingroup FH5ES
!!
!! \brief Retrieves number of events in an event set.
!!
!! \param es_id  \es_id
!! \param count  The number of events in the event set
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5ESget_count()
!!
  SUBROUTINE H5ESget_count_f(es_id, count, hdferr)
    IMPLICIT NONE

    INTEGER(hid_t),  INTENT(IN)  :: es_id
    INTEGER(size_t), INTENT(OUT) :: count
    INTEGER,         INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5ESget_count(es_id, count) BIND(C,NAME='H5ESget_count')
         IMPORT :: C_INT
         IMPORT :: HID_T, SIZE_T
         INTEGER(HID_T), VALUE  :: es_id
         INTEGER(SIZE_T), VALUE :: count
       END FUNCTION H5ESget_count
    END INTERFACE

    hdferr = INT(H5ESget_count(es_id, count))

  END SUBROUTINE H5ESget_count_f
!>
!! \ingroup FH5ES
!!
!! \brief Retrieves the next operation counter to be assigned in an event set.
!!
!! \param es_id   \es_id
!! \param counter The number of events in the event set
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5ESget_op_counter()
!!
  SUBROUTINE H5ESget_op_counter_f(es_id, counter, hdferr)

    IMPLICIT NONE

    INTEGER(HID_T)    , INTENT(IN)  :: es_id
    INTEGER(C_INT64_T), INTENT(OUT) :: counter
    INTEGER           , INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5ESget_op_counter(es_id, counter) BIND(C,NAME='H5ESget_op_counter')
         IMPORT :: C_INT
         IMPORT :: HID_T, C_INT64_T
         INTEGER(HID_T),     VALUE :: es_id
         INTEGER(C_INT64_T), VALUE :: counter
       END FUNCTION H5ESget_op_counter
    END INTERFACE

    hdferr = INT(H5ESget_op_counter(es_id, counter))

  END SUBROUTINE H5ESget_op_counter_f
!>
!! \ingroup FH5ES
!!
!! \brief Waits for operations in event set to complete.
!!
!! \param es_id           \es_id
!! \param timeout         The number of events in the event set
!! \param num_in_progress The number of operations still in progress
!! \param err_occurred    Flag if an operation in the event set failed
!! \param hdferr          \fortran_error
!!
!! See C API: @ref H5ESwait()
!!
  SUBROUTINE H5ESwait_f(es_id, timeout, num_in_progress, err_occurred, hdferr)

    IMPLICIT NONE

    INTEGER(HID_T)    , INTENT(IN)  :: es_id
    INTEGER(C_INT64_T), INTENT(IN)  :: timeout
    INTEGER(SIZE_T)   , INTENT(OUT) :: num_in_progress
    LOGICAL           , INTENT(OUT) :: err_occurred
    INTEGER           , INTENT(OUT) :: hdferr

    LOGICAL(C_BOOL)    :: err_occurred_c = .FALSE.

    INTERFACE
       INTEGER(C_INT) FUNCTION H5ESwait(es_id, timeout, num_in_progress, err_occurred) BIND(C,NAME='H5ESwait')
         IMPORT :: C_INT
         IMPORT :: HID_T, C_INT64_T, SIZE_T, C_BOOL
         INTEGER(HID_T)    , VALUE :: es_id
         INTEGER(C_INT64_T), VALUE :: timeout
         INTEGER(SIZE_T)   , VALUE :: num_in_progress
         LOGICAL(C_BOOL)   , VALUE :: err_occurred
       END FUNCTION H5ESwait
    END INTERFACE

    hdferr = INT(H5ESwait(es_id, timeout, num_in_progress, err_occurred_c))

    ! Transfer value of C c_bool type to Fortran LOGICAL
    err_occurred = err_occurred_c

  END SUBROUTINE H5ESwait_f
!>
!! \ingroup FH5ES
!!
!! \brief Attempt to cancel operations in an event set.
!!
!! \param es_id            \es_id
!! \param num_not_canceled The number of events not canceled
!! \param err_occurred     Status indicating if error is present in the event set
!! \param hdferr           \fortran_error
!!
!! See C API: @ref H5EScancel()
!!
  SUBROUTINE H5EScancel_f(es_id, num_not_canceled, err_occurred, hdferr)

    IMPLICIT NONE

    INTEGER(hid_t)  :: es_id
    INTEGER(size_t) :: num_not_canceled
    LOGICAL         :: err_occurred
    INTEGER         :: hdferr

    LOGICAL(C_BOOL) :: err_occurred_c = .FALSE.

    INTERFACE
       INTEGER(C_INT) FUNCTION H5EScancel(es_id, num_not_canceled, err_occurred) BIND(C,NAME='H5EScancel')
         IMPORT :: C_INT
         IMPORT :: HID_T, SIZE_T, C_BOOL
         INTEGER(HID_T)    , VALUE :: es_id
         INTEGER(SIZE_T)   , VALUE :: num_not_canceled
         LOGICAL(C_BOOL)   , VALUE :: err_occurred
       END FUNCTION H5EScancel
    END INTERFACE

    hdferr = INT(H5EScancel(es_id, num_not_canceled, err_occurred_c))

    ! Transfer value of C c_bool type to Fortran LOGICAL
    err_occurred = err_occurred_c

  END SUBROUTINE H5EScancel_f
!>
!! \ingroup FH5ES
!!
!! \brief Attempt to cancel operations in an event set.
!!
!! \param es_id        \es_id
!! \param err_occurred Status indicating if error is present in the event set
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5ESget_err_status()
!!
  SUBROUTINE H5ESget_err_status_f(es_id, err_occurred, hdferr)

    IMPLICIT NONE

    INTEGER(hid_t), INTENT(IN)  :: es_id
    LOGICAL       , INTENT(OUT) :: err_occurred
    INTEGER       , INTENT(OUT) :: hdferr

    LOGICAL(C_BOOL) :: err_occurred_c = .FALSE.

    INTERFACE
       INTEGER(C_INT) FUNCTION H5ESget_err_status(es_id, err_occurred) BIND(C,NAME='H5ESget_err_status')
         IMPORT :: C_INT
         IMPORT :: HID_T, C_BOOL
         INTEGER(HID_T) , VALUE :: es_id
         LOGICAL(C_BOOL), VALUE :: err_occurred
       END FUNCTION H5ESget_err_status
    END INTERFACE

    hdferr = INT(H5ESget_err_status(es_id, err_occurred_c))

    ! Transfer value of C c_bool type to Fortran LOGICAL
    err_occurred = err_occurred_c

  END SUBROUTINE H5ESget_err_status_f
!>
!! \ingroup FH5ES
!!
!! \brief Retrieves the number of failed operations.
!!
!! \param es_id    \es_id
!! \param num_errs Number of errors
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5ESget_err_count()
!!
  SUBROUTINE H5ESget_err_count_f(es_id, num_errs, hdferr)

    IMPLICIT NONE

    INTEGER(HID_T) , INTENT(IN)  :: es_id
    INTEGER(SIZE_T), INTENT(OUT) :: num_errs
    INTEGER        , INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5ESget_err_count(es_id, num_errs) BIND(C,NAME='H5ESget_err_count')
         IMPORT :: C_INT
         IMPORT :: HID_T, SIZE_T
         INTEGER(HID_T) , VALUE :: es_id
         INTEGER(SIZE_T), VALUE :: num_errs
       END FUNCTION H5ESget_err_count
    END INTERFACE

    hdferr = INT(H5ESget_err_count(es_id, num_errs))

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
       INTEGER(C_INT) FUNCTION H5ESget_err_info(es_id, num_err_info, err_info, num_cleared) BIND(C,NAME='H5ESget_err_info')
         IMPORT :: C_INT
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

!>
!! \ingroup FH5ES
!!
!! \brief Terminates access to an event set.
!!
!! \param es_id       \es_id
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5ESclose()
!!
  SUBROUTINE H5ESclose_f(es_id, hdferr)

    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: es_id
    INTEGER       , INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5ESclose(es_id) BIND(C,NAME='H5ESclose')
         IMPORT :: C_INT
         IMPORT :: HID_T
         INTEGER(HID_T) :: es_id
       END FUNCTION H5ESclose
    END INTERFACE

    hdferr = INT(H5ESclose(es_id))

  END SUBROUTINE H5ESclose_f

END MODULE H5ES
