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
! NOTES
!       _____ __  __ _____   ____  _____ _______       _   _ _______
!      |_   _|  \/  |  __ \ / __ \|  __ \__   __|/\   | \ | |__   __|
! ****   | | | \  / | |__) | |  | | |__) | | |  /  \  |  \| |  | |    ****
! ****   | | | |\/| |  ___/| |  | |  _  /  | | / /\ \ | . ` |  | |    ****
! ****  _| |_| |  | | |    | |__| | | \ \  | |/ ____ \| |\  |  | |    ****
!      |_____|_|  |_|_|     \____/|_|  \_\ |_/_/    \_\_| \_|  |_|
!
!  If you add a new H5ES function to the module you must add the function name
!  to the Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!

MODULE H5ES

  USE H5GLOBAL
  IMPLICIT NONE

CONTAINS

!>
!! \ingroup FH5ES
!!
!! \brief Creates an event set.
!!
!! \param es_id  \fortran_es_id
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5EScreate()
!!
  SUBROUTINE h5escreate_f(es_id, hdferr)
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

  END SUBROUTINE h5escreate_f
!>
!! \ingroup FH5ES
!!
!! \brief Retrieves number of events in an event set.
!!
!! \param es_id  \fortran_es_id
!! \param count  The number of events in the event set
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5ESget_count()
!!
  SUBROUTINE h5esget_count_f(es_id, count, hdferr)
    IMPLICIT NONE

    INTEGER(hid_t),  INTENT(IN)  :: es_id
    INTEGER(size_t), INTENT(OUT) :: count
    INTEGER,         INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5ESget_count(es_id, count) BIND(C,NAME='H5ESget_count')
         IMPORT :: C_INT
         IMPORT :: HID_T, SIZE_T
         INTEGER(HID_T), VALUE  :: es_id
         INTEGER(SIZE_T)        :: count
       END FUNCTION H5ESget_count
    END INTERFACE

    hdferr = INT(H5ESget_count(es_id, count))

  END SUBROUTINE h5esget_count_f
!>
!! \ingroup FH5ES
!!
!! \brief Retrieves the next operation counter to be assigned in an event set.
!!
!! \param es_id   \fortran_es_id
!! \param counter The number of events in the event set
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5ESget_op_counter()
!!
  SUBROUTINE h5esget_op_counter_f(es_id, counter, hdferr)

    IMPLICIT NONE

    INTEGER(HID_T)    , INTENT(IN)  :: es_id
    INTEGER(C_INT64_T), INTENT(OUT) :: counter
    INTEGER           , INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5ESget_op_counter(es_id, counter) BIND(C,NAME='H5ESget_op_counter')
         IMPORT :: C_INT
         IMPORT :: HID_T, C_INT64_T
         INTEGER(HID_T)    , VALUE :: es_id
         INTEGER(C_INT64_T)        :: counter
       END FUNCTION H5ESget_op_counter
    END INTERFACE

    hdferr = INT(H5ESget_op_counter(es_id, counter))

  END SUBROUTINE h5esget_op_counter_f
!>
!! \ingroup FH5ES
!!
!! \brief Waits for operations in event set to complete.
!!
!! \param es_id           \fortran_es_id
!! \param timeout         The number of events in the event set
!! \param num_in_progress The number of operations still in progress
!! \param err_occurred    Flag if an operation in the event set failed
!! \param hdferr          \fortran_error
!!
!! See C API: @ref H5ESwait()
!!
  SUBROUTINE h5eswait_f(es_id, timeout, num_in_progress, err_occurred, hdferr)

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
         INTEGER(SIZE_T)           :: num_in_progress
         LOGICAL(C_BOOL)           :: err_occurred
       END FUNCTION H5ESwait
    END INTERFACE

    hdferr = INT(H5ESwait(es_id, timeout, num_in_progress, err_occurred_c))

    ! Transfer value of C c_bool type to Fortran LOGICAL
    err_occurred = err_occurred_c

  END SUBROUTINE h5eswait_f
!>
!! \ingroup FH5ES
!!
!! \brief Attempt to cancel operations in an event set.
!!
!! \param es_id            \fortran_es_id
!! \param num_not_canceled The number of events not canceled
!! \param err_occurred     Status indicating if error is present in the event set
!! \param hdferr           \fortran_error
!!
!! See C API: @ref H5EScancel()
!!
  SUBROUTINE h5escancel_f(es_id, num_not_canceled, err_occurred, hdferr)

    IMPLICIT NONE

    INTEGER(hid_t) , INTENT(IN)  :: es_id
    INTEGER(size_t), INTENT(OUT) :: num_not_canceled
    LOGICAL        , INTENT(OUT) :: err_occurred
    INTEGER        , INTENT(OUT) :: hdferr

    LOGICAL(C_BOOL) :: err_occurred_c = .FALSE.

    INTERFACE
       INTEGER(C_INT) FUNCTION H5EScancel(es_id, num_not_canceled, err_occurred) BIND(C,NAME='H5EScancel')
         IMPORT :: C_INT
         IMPORT :: HID_T, SIZE_T, C_BOOL
         INTEGER(HID_T) , VALUE :: es_id
         INTEGER(SIZE_T)        :: num_not_canceled
         LOGICAL(C_BOOL)        :: err_occurred
       END FUNCTION H5EScancel
    END INTERFACE

    hdferr = INT(H5EScancel(es_id, num_not_canceled, err_occurred_c))

    ! Transfer value of C c_bool type to Fortran LOGICAL
    err_occurred = err_occurred_c

  END SUBROUTINE h5escancel_f
!>
!! \ingroup FH5ES
!!
!! \brief Checks for failed operations.
!!
!! \param es_id        \fortran_es_id
!! \param err_occurred Status indicating if error is present in the event set
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5ESget_err_status()
!!
  SUBROUTINE h5esget_err_status_f(es_id, err_occurred, hdferr)

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
         LOGICAL(C_BOOL)        :: err_occurred
       END FUNCTION H5ESget_err_status
    END INTERFACE

    hdferr = INT(H5ESget_err_status(es_id, err_occurred_c))

    ! Transfer value of C c_bool type to Fortran LOGICAL
    err_occurred = err_occurred_c

  END SUBROUTINE h5esget_err_status_f
!>
!! \ingroup FH5ES
!!
!! \brief Retrieves the number of failed operations.
!!
!! \param es_id    \fortran_es_id
!! \param num_errs Number of errors
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5ESget_err_count()
!!
  SUBROUTINE h5esget_err_count_f(es_id, num_errs, hdferr)

    IMPLICIT NONE

    INTEGER(HID_T) , INTENT(IN)  :: es_id
    INTEGER(SIZE_T), INTENT(OUT) :: num_errs
    INTEGER        , INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5ESget_err_count(es_id, num_errs) BIND(C,NAME='H5ESget_err_count')
         IMPORT :: C_INT
         IMPORT :: HID_T, SIZE_T
         INTEGER(HID_T) , VALUE :: es_id
         INTEGER(SIZE_T)        :: num_errs
       END FUNCTION H5ESget_err_count
    END INTERFACE

    hdferr = INT(H5ESget_err_count(es_id, num_errs))

  END SUBROUTINE h5esget_err_count_f

!>
!! \ingroup FH5ES
!!
!! \brief Terminates access to an event set.
!!
!! \param es_id       \fortran_es_id
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5ESclose()
!!
  SUBROUTINE h5esclose_f(es_id, hdferr)

    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: es_id
    INTEGER       , INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5ESclose(es_id) BIND(C,NAME='H5ESclose')
         IMPORT :: C_INT
         IMPORT :: HID_T
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5ESclose
    END INTERFACE

    hdferr = INT(H5ESclose(es_id))

  END SUBROUTINE h5esclose_f

END MODULE H5ES
