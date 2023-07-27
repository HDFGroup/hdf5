!> @defgroup FH5E Fortran Error (H5E) Interface
!!
!! @see H5E, C-API
!!
!! @see @ref H5E_UG, User Guide
!!

!> @ingroup FH5E
!!
!! @brief This module contains Fortran interfaces for H5E functions.
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
!  If you add a new H5E function to the module you must add the function name
!  to the Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!

MODULE H5E

  USE H5GLOBAL
  IMPLICIT NONE

  INTEGER, PARAMETER :: PRINTON  = 1 !< Turn on automatic printing of errors
  INTEGER, PARAMETER :: PRINTOFF = 0 !< Turn off automatic printing of errors

CONTAINS

!>
!! \ingroup FH5E
!!
!! \brief Clears the error stack for the current thread.
!!
!! \param hdferr    \fortran_error
!! \param estack_id Error Stack id
!!
!! See C API: @ref H5Eclear2()
!!
  SUBROUTINE h5eclear_f(hdferr, estack_id)
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: estack_id
    INTEGER(HID_T) :: estack_id_default

    INTERFACE
       INTEGER FUNCTION h5eclear_c(estack_id_default) BIND(C,NAME='h5eclear_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T) :: estack_id_default
       END FUNCTION h5eclear_c
    END INTERFACE

    estack_id_default = H5E_DEFAULT_F
    IF(PRESENT(estack_id)) estack_id_default = estack_id

    hdferr = h5eclear_c(estack_id_default)
  END SUBROUTINE h5eclear_f

!>
!! \ingroup FH5E
!!
!! \brief Prints the error stack in a default manner.
!!
!! \param hdferr \fortran_error
!! \param name   Name of the file that contains print output
!!
!! See C API: @ref H5Eprint2()
!!
  SUBROUTINE h5eprint_f(hdferr, name)
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: name
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: namelen

    INTERFACE
       INTEGER FUNCTION h5eprint_c1(name, namelen) BIND(C,NAME='h5eprint_c1')
         IMPORT :: C_CHAR
         IMPLICIT NONE
         INTEGER :: namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
       END FUNCTION h5eprint_c1
    END INTERFACE

    INTERFACE
       INTEGER FUNCTION h5eprint_c2()  BIND(C,NAME='h5eprint_c2')
       END FUNCTION h5eprint_c2
    END INTERFACE

    IF (PRESENT(name)) THEN
       namelen = LEN(NAME)
       hdferr = h5eprint_c1(name, namelen)
    ELSE
       hdferr = h5eprint_c2()
    ENDIF
  END SUBROUTINE h5eprint_f
!>
!! \ingroup FH5E
!!
!! \brief Returns a character string describing an error specified by a major error number.
!!
!! \param error_no Major error number.
!! \param name     Character string describing the error.
!! \param namelen  Number of characters in the name buffer.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Eget_major()
!!
  SUBROUTINE h5eget_major_f(error_no, name, namelen, hdferr)
    INTEGER, INTENT(IN) :: error_no
    CHARACTER(LEN=*), INTENT(OUT) :: name
    INTEGER(SIZE_T), INTENT(IN) :: namelen
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5eget_major_c(error_no, name, namelen)  BIND(C,NAME='h5eget_major_c')
         IMPORT :: C_CHAR
         IMPORT :: SIZE_T
         IMPLICIT NONE
         INTEGER :: error_no
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         INTEGER(SIZE_T), INTENT(IN) :: namelen
       END FUNCTION h5eget_major_c
    END INTERFACE

    hdferr = h5eget_major_c(error_no, name, namelen)
  END SUBROUTINE h5eget_major_f
!>
!! \ingroup FH5E
!!
!! \brief Returns a character string describing an error specified by a minor error number.
!!
!! \param error_no Minor error number.
!! \param name     Character string describing the error.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Eget_minor()
!!
  SUBROUTINE h5eget_minor_f(error_no, name, hdferr)
    INTEGER, INTENT(IN) :: error_no
    CHARACTER(LEN=*), INTENT(OUT) :: name
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5eget_minor_c(error_no, name) BIND(C,NAME='h5eget_minor_c')
         IMPORT :: C_CHAR
         INTEGER :: error_no
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: name
       END FUNCTION h5eget_minor_c
    END INTERFACE

    hdferr = h5eget_minor_c(error_no, name)
  END SUBROUTINE h5eget_minor_f

!>
!! \ingroup FH5E
!!
!! \brief Returns settings for automatic error stack traversal function and its data.
!!
!! \param printflag   Flag to turn automatic error printing on or off; possible values are:
!!                    \li printon (1)
!!                    \li printoff(0)
!! \param estack_id   Error stack identifier.
!! \param func        Function to be called upon an error condition.
!! \param client_data Data passed to the error function.
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Eset_auto2()
!!
  SUBROUTINE h5eset_auto_f(printflag, hdferr, estack_id, func, client_data)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR, C_FUNPTR
    INTEGER       , INTENT(IN)            :: printflag
    INTEGER       , INTENT(OUT)           :: hdferr
    INTEGER(HID_T), INTENT(IN) , OPTIONAL :: estack_id
    TYPE(C_FUNPTR), INTENT(IN) , OPTIONAL :: func
    TYPE(C_PTR)   , INTENT(IN) , OPTIONAL :: client_data
    INTEGER(HID_T) :: estack_id_default
    TYPE(C_FUNPTR) :: func_default
    TYPE(C_PTR)    :: client_data_default
    INTERFACE
       INTEGER FUNCTION h5eset_auto2_c(printflag, estack_id, func, client_data) &
            BIND(C, NAME='h5eset_auto2_c')
         IMPORT :: c_ptr, c_funptr
         IMPORT :: HID_T
         INTEGER :: printflag
         INTEGER(HID_T) :: estack_id
         TYPE(C_FUNPTR), VALUE :: func
         TYPE(C_PTR), VALUE :: client_data
       END FUNCTION h5eset_auto2_c
    END INTERFACE

    estack_id_default = -1
    func_default = C_NULL_FUNPTR
    client_data_default = C_NULL_PTR

    IF(PRESENT(estack_id)) estack_id_default = estack_id
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(client_data)) client_data_default = client_data

    hdferr = h5eset_auto2_c(printflag, estack_id_default, func_default, client_data_default)
  END SUBROUTINE h5eset_auto_f

END MODULE H5E

