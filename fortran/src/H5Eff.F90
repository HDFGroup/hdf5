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
! MISSING: H5Eauto_is_v2, H5Eget_auto2

MODULE H5E

  USE H5GLOBAL
  USE H5fortkit
  IMPLICIT NONE

  INTEGER, PARAMETER :: PRINTON  = 1 !< Turn on automatic printing of errors
  INTEGER, PARAMETER :: PRINTOFF = 0 !< Turn off automatic printing of errors

!> @brief h5e_error_t derived type
  TYPE, BIND(C) :: h5e_error_t
     INTEGER(HID_T) :: cls_id  !< Class ID
     INTEGER(HID_T) :: maj_num !< Major error ID
     INTEGER(HID_T) :: min_num !< Minor error number
     INTEGER(C_INT) :: line      !< Line in file where error occurs
     TYPE(C_PTR)    :: func_name !< Function in which error occurred
     TYPE(C_PTR)    :: file_name !< File in which error occurred
     TYPE(C_PTR)    :: desc      !< Optional supplied description
  END TYPE h5e_error_t

  INTERFACE h5eprint_f
     MODULE PROCEDURE h5eprint1_f
     MODULE PROCEDURE h5eprint2_f
  END INTERFACE h5eprint_f

  INTERFACE
     INTEGER FUNCTION h5eprint_c(err_stack, name, namelen) BIND(C,NAME='h5eprint_c')
       IMPORT :: C_CHAR, HID_T, C_PTR
       IMPLICIT NONE
       INTEGER(HID_T)     :: err_stack
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
       TYPE(C_PTR), VALUE :: namelen
     END FUNCTION h5eprint_c
  END INTERFACE

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
       INTEGER(C_INT) FUNCTION H5Eclear(err_stack) BIND(C,NAME='H5Eclear2')
         IMPORT :: C_INT, HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: err_stack
       END FUNCTION H5Eclear
    END INTERFACE

    estack_id_default = H5E_DEFAULT_F
    IF(PRESENT(estack_id)) estack_id_default = estack_id

    hdferr = INT(H5Eclear(estack_id_default))
  END SUBROUTINE h5eclear_f

#ifdef H5_DOXYGEN
!>
!! \ingroup FH5E
!!
!! \brief Prints the error stack in a default manner.
!!
!! \param hdferr \fortran_error
!! \param name   Name of the file that contains print output
!!
!! \note If \p name is not specified, the output will be sent to
!!       the standard error (stderr).
!!
!! \attention Deprecated.
!!
!! See C API: @ref H5Eprint1()
!!
  SUBROUTINE h5eprint_f(hdferr, name)
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: name
    INTEGER, INTENT(OUT) :: hdferr
  END SUBROUTINE h5eprint_f

!! \ingroup FH5E
!!
!! \brief Prints the error stack in a default manner.
!!
!! \param err_stack Error stack identifier
!! \param hdferr    \fortran_error
!! \param name      Name of the file that contains print output
!!
!! \note If \p name is not specified, the output will be sent to
!!       the standard error (stderr).
!!
!! See C API: @ref H5Eprint2()
!!
  SUBROUTINE h5eprint_f(err_stack, hdferr, name)
    INTEGER(HID_T)  , INTENT(IN)  :: err_stack
    INTEGER         , INTENT(OUT) :: hdferr
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: name
  END SUBROUTINE h5eprint_f

#else

  SUBROUTINE h5eprint1_f(hdferr, name)
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: name
    INTEGER, INTENT(OUT) :: hdferr

    CALL h5eprint2_f(H5E_DEFAULT_F, hdferr, name)

  END SUBROUTINE h5eprint1_f

  SUBROUTINE h5eprint2_f(err_stack, hdferr, name)
    INTEGER(HID_T),   INTENT(IN) :: err_stack
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: name
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER(SIZE_T), TARGET :: namelen
    TYPE(C_PTR)            :: c_namelen

    IF (PRESENT(name)) THEN
       namelen = LEN(NAME, SIZE_T)
       c_namelen = C_LOC(namelen)
       hdferr = h5eprint_c(err_stack, name, c_namelen)
    ELSE
       hdferr = h5eprint_c(err_stack, C_NULL_CHAR, C_NULL_PTR)
    ENDIF
  END SUBROUTINE h5eprint2_f

#endif

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
!! \attention Deprecated: use H5Eget_msg_f() instead.
!!
!! See C API: @ref H5Eget_major()
!!
  SUBROUTINE h5eget_major_f(error_no, name, namelen, hdferr)
    INTEGER(HID_T)  , INTENT(IN)    :: error_no
    CHARACTER(LEN=*), INTENT(OUT)   :: name
    INTEGER(SIZE_T) , INTENT(INOUT) :: namelen
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER :: msg_type
    INTEGER(SIZE_T) :: namelen2

    namelen2 = namelen

    CALL H5Eget_msg_f(error_no, msg_type, name, hdferr, namelen2)

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
!! \attention Deprecated: use H5Eget_msg_f() instead.
!!
!! See C API: @ref H5Eget_minor()
!!
  SUBROUTINE h5eget_minor_f(error_no, name, hdferr)
    INTEGER(HID_T)  , INTENT(IN) :: error_no
    CHARACTER(LEN=*), INTENT(OUT) :: name
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER :: msg_type

    CALL H5Eget_msg_f(error_no, msg_type, name, hdferr)

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
!>
!! \ingroup FH5E
!!
!! \brief Pushes a new error record onto an error stack.
!!
!! \param err_stack Error stack identifier. If the identifier is H5E_DEFAULT_F, the error
!!                  record will be pushed to the current stack.
!! \param file   Name of the file in which the error was detected
!! \param func   Name of the function in which the error was detected
!! \param line   Line number in the file where the error was detected
!! \param cls_id Error class identifier
!! \param maj_id Major error identifier
!! \param min_id Minor error identifier
!! \param msg    Error description string
!! \param hdferr \fortran_error
!! \param arg1   C style format control strings
!! \param arg2   C style format control strings
!! \param arg3   C style format control strings
!! \param arg4   C style format control strings
!! \param arg5   C style format control strings
!! \param arg6   C style format control strings
!! \param arg7   C style format control strings
!! \param arg8   C style format control strings
!! \param arg9   C style format control strings
!! \param arg10  C style format control strings
!! \param arg11  C style format control strings
!! \param arg12  C style format control strings
!! \param arg13  C style format control strings
!! \param arg14  C style format control strings
!! \param arg15  C style format control strings
!! \param arg16  C style format control strings
!! \param arg17  C style format control strings
!! \param arg18  C style format control strings
!! \param arg19  C style format control strings
!! \param arg20  C style format control strings
!!
!! \note \p arg[1-20] expects C-style format strings, similar to the system and C functions printf() and fprintf().
!!       Furthermore, special characters, such as ANSI escapes, will only be interpreted correctly if the Fortran
!!       equivalent is used. For example, to print \p msg "TEXT" in red would be:
!!       <br /><br />
!!       \code
!!       (..., "%s TEXT %s", hdferr, ..., arg1=ACHAR(27)//"[31m"//C_NULL_CHAR, arg2=ACHAR(27)//"[0m"//C_NULL_CHAR )
!!       \endcode
!!       <br />Using "\n" instead of C_NEW_LINE will not be interpereted correctly, and similarly,
!!             using "\x1B" instead of ACHAR(27). Also, all \p arg[1-20] characters strings must be
!!             NULL terminated.
!!
!! See C API: @ref H5Epush2()
!!
  SUBROUTINE h5epush_f(err_stack, file, func, line, cls_id, maj_id, min_id, msg, hdferr, &
       arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, &
       arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)  :: err_stack
    CHARACTER(LEN=*), INTENT(IN)  :: file
    CHARACTER(LEN=*), INTENT(IN)  :: func
    INTEGER         , INTENT(IN)  :: line
    INTEGER(HID_T)  , INTENT(IN)  :: cls_id
    INTEGER(HID_T)  , INTENT(IN)  :: maj_id
    INTEGER(HID_T)  , INTENT(IN)  :: min_id
    CHARACTER(LEN=*), INTENT(IN)  :: msg
    INTEGER         , INTENT(OUT) :: hdferr

    CHARACTER(LEN=*), OPTIONAL, TARGET :: arg1, arg2, arg3, arg4, arg5, &
         arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, &
         arg16, arg17, arg18, arg19, arg20

    TYPE(C_PTR) :: arg1_def = C_NULL_PTR, arg2_def = C_NULL_PTR, &
         arg3_def = C_NULL_PTR, arg4_def = C_NULL_PTR, &
         arg5_def = C_NULL_PTR, arg6_def = C_NULL_PTR, &
         arg7_def = C_NULL_PTR, arg8_def = C_NULL_PTR, &
         arg9_def = C_NULL_PTR, arg10_def = C_NULL_PTR, &
         arg11_def = C_NULL_PTR, arg12_def = C_NULL_PTR, &
         arg13_def = C_NULL_PTR, arg14_def = C_NULL_PTR, &
         arg15_def = C_NULL_PTR, arg16_def = C_NULL_PTR, &
         arg17_def = C_NULL_PTR, arg18_def = C_NULL_PTR, &
         arg19_def = C_NULL_PTR, arg20_def = C_NULL_PTR

    INTERFACE
       INTEGER FUNCTION h5epush_c(err_stack, &
            file, file_len, func, func_len, line, &
            cls_id, maj_id, min_id, msg, msg_len, &
            arg1, arg2, arg3, arg4, arg5, &
            arg6, arg7, arg8, arg9, arg10, &
            arg11, arg12, arg13, arg14, arg15, &
            arg16, arg17, arg18, arg19, arg20) BIND(C, NAME='h5epush_c')

         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T) :: err_stack
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: file
         INTEGER :: file_len
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: func
         INTEGER :: func_len
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T) :: cls_id
         INTEGER(HID_T) :: maj_id
         INTEGER(HID_T) :: min_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: msg
         INTEGER :: msg_len

         TYPE(C_PTR), VALUE :: arg1, arg2, arg3, arg4, arg5, &
              arg6, arg7, arg8, arg9, arg10, &
              arg11, arg12, arg13, arg14, arg15, &
              arg16, arg17, arg18, arg19, arg20

       END FUNCTION h5epush_c
    END INTERFACE

    IF (PRESENT(arg1)) arg1_def = C_LOC(arg1(1:1))
    IF (PRESENT(arg2)) arg2_def = C_LOC(arg2(1:1))
    IF (PRESENT(arg3)) arg3_def = C_LOC(arg3(1:1))
    IF (PRESENT(arg4)) arg4_def = C_LOC(arg4(1:1))
    IF (PRESENT(arg5)) arg5_def = C_LOC(arg5(1:1))
    IF (PRESENT(arg6)) arg6_def = C_LOC(arg6(1:1))
    IF (PRESENT(arg7)) arg7_def = C_LOC(arg7(1:1))
    IF (PRESENT(arg8)) arg8_def = C_LOC(arg8(1:1))
    IF (PRESENT(arg9)) arg9_def = C_LOC(arg9(1:1))
    IF (PRESENT(arg10)) arg10_def = C_LOC(arg10(1:1))
    IF (PRESENT(arg11)) arg11_def = C_LOC(arg11(1:1))
    IF (PRESENT(arg12)) arg12_def = C_LOC(arg12(1:1))
    IF (PRESENT(arg13)) arg13_def = C_LOC(arg13(1:1))
    IF (PRESENT(arg14)) arg14_def = C_LOC(arg14(1:1))
    IF (PRESENT(arg15)) arg15_def = C_LOC(arg15(1:1))
    IF (PRESENT(arg16)) arg16_def = C_LOC(arg16(1:1))
    IF (PRESENT(arg17)) arg17_def = C_LOC(arg17(1:1))
    IF (PRESENT(arg18)) arg18_def = C_LOC(arg18(1:1))
    IF (PRESENT(arg19)) arg19_def = C_LOC(arg19(1:1))
    IF (PRESENT(arg20)) arg20_def = C_LOC(arg20(1:1))

    hdferr = h5epush_c(err_stack, file, LEN(file), func, LEN(func), INT(line,C_INT), &
         cls_id, maj_id, min_id, msg, LEN(msg), &
         arg1_def, arg2_def, arg3_def, arg4_def, arg5_def, &
         arg6_def, arg7_def, arg8_def, arg9_def, arg10_def, &
         arg11_def, arg12_def, arg13_def, arg14_def, arg15_def, &
         arg16_def, arg17_def, arg18_def, arg19_def, arg20_def)

  END SUBROUTINE h5epush_f

!>
!! \ingroup FH5E
!!
!! \brief Registers a client library or application program to the HDF5 error API.
!!
!! \param cls_name Name of the error class
!! \param lib_name Name of the client library or application to which the error class belongs
!! \param version  Version of the client library or application to which the error class belongs. It can be NULL.
!! \param class_id Class identifier
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Eregister_class()
!!
  SUBROUTINE h5eregister_class_f(cls_name, lib_name, version, class_id, hdferr)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)  :: cls_name
    CHARACTER(LEN=*), INTENT(IN)  :: lib_name
    CHARACTER(LEN=*), INTENT(IN)  :: version
    INTEGER(HID_T)  , INTENT(OUT) :: class_id
    INTEGER, INTENT(OUT) :: hdferr

    CHARACTER(LEN=LEN_TRIM(cls_name)+1,KIND=C_CHAR) :: c_cls_name
    CHARACTER(LEN=LEN_TRIM(lib_name)+1,KIND=C_CHAR) :: c_lib_name
    CHARACTER(LEN=LEN_TRIM(version)+1,KIND=C_CHAR) :: c_version
    INTERFACE
       INTEGER(HID_T) FUNCTION H5Eregister_class(cls_name, lib_name, version) &
            BIND(C,NAME='H5Eregister_class')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: cls_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: lib_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: version

       END FUNCTION H5Eregister_class
    END INTERFACE

    c_cls_name = TRIM(cls_name)//C_NULL_CHAR
    c_lib_name = TRIM(lib_name)//C_NULL_CHAR
    c_version  = TRIM(version)//C_NULL_CHAR

    class_id = H5Eregister_class(c_cls_name, c_lib_name, c_version)

    hdferr = 0
    IF(class_id.LT.0) hdferr = -1

  END SUBROUTINE h5eregister_class_f
!>
!! \ingroup FH5E
!!
!! \brief Removes an error class.
!!
!! \param class_id Class identifier
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Eunregister_class()
!!
  SUBROUTINE h5eunregister_class_f(class_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class_id
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Eunregister_class(class_id) BIND(C, NAME='H5Eunregister_class')
         IMPORT :: HID_T, C_INT
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: class_id
       END FUNCTION H5Eunregister_class
    END INTERFACE

    hdferr = INT(H5Eunregister_class(class_id))

  END SUBROUTINE h5eunregister_class_f
!>
!! \ingroup FH5E
!!
!! \brief Adds a major or minor error message to an error class.
!!
!! \param class_id An error class identifier
!! \param msg_type The type of the error message
!! \param msg	   Error message
!! \param err_id   Error identifier
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Ecreate_msg()
!!
  SUBROUTINE h5ecreate_msg_f(class_id, msg_type, msg, err_id, hdferr)
    IMPLICIT NONE

    INTEGER(HID_T)  , INTENT(IN)  :: class_id
    INTEGER         , INTENT(IN)  :: msg_type
    CHARACTER(LEN=*), INTENT(IN)  :: msg
    INTEGER(HID_T)  , INTENT(OUT) :: err_id
    INTEGER, INTENT(OUT) :: hdferr

    CHARACTER(LEN=LEN_TRIM(msg)+1,KIND=C_CHAR) :: c_msg

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Ecreate_msg(class_id, msg_type, msg) &
            BIND(C,NAME='H5Ecreate_msg')
         IMPORT :: C_CHAR, C_INT
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE  :: class_id
         INTEGER(C_INT), VALUE  :: msg_type
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: msg
       END FUNCTION H5Ecreate_msg
    END INTERFACE

    c_msg = TRIM(msg)//C_NULL_CHAR

    err_id = H5Ecreate_msg(class_id, INT(msg_type, C_INT), c_msg)

    hdferr = 0
    IF(err_id.LT.0) hdferr = -1

  END SUBROUTINE h5ecreate_msg_f
!>
!! \ingroup FH5E
!!
!! \brief Closes an error message.
!!
!! \param err_id An error message identifier
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Eclose_msg()
!!
  SUBROUTINE h5eclose_msg_f(err_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: err_id
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Eclose_msg(err_id) BIND(C, NAME='H5Eclose_msg')
         IMPORT :: HID_T, C_INT
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: err_id
       END FUNCTION H5Eclose_msg
    END INTERFACE

    hdferr = INT(H5Eclose_msg(err_id))

  END SUBROUTINE h5eclose_msg_f
!>
!! \ingroup FH5E
!!
!! \brief Retrieves an error message.
!!
!! \param msg_id   Error message identifier
!! \param msg_type The type of the error message. Valid values are H5E_MAJOR_F and H5E_MINOR_F.
!! \param msg	   Error message buffer
!! \param hdferr   \fortran_error
!! \param msg_size The length of error message to be returned by this function
!!
!! If \p msg_size is omitted, the API will copy up to the length of \p msg, and it
!! is the application's responsibility to provide a large enough buffer. If \p msg_size
!! is zero, the required buffer size will be returned, and \p msg is not accessed.
!! If \p msg_size is greater than zero, the function will copy up to the length
!! of \p msg_size info \p msg.
!!
!! See C API: @ref H5Eget_msg()
!!
  SUBROUTINE H5Eget_msg_f(msg_id, msg_type, msg, hdferr, msg_size)
    IMPLICIT NONE

    INTEGER(HID_T)  , INTENT(IN)    :: msg_id
    INTEGER         , INTENT(OUT)   :: msg_type
    CHARACTER(LEN=*)                :: msg
    INTEGER         , INTENT(OUT)   :: hdferr
    INTEGER(SIZE_T) , INTENT(INOUT), OPTIONAL :: msg_size

    CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(:), ALLOCATABLE, TARGET :: c_msg
    INTEGER(C_INT) :: c_msg_type
    TYPE(C_PTR) :: f_ptr
    INTEGER(SIZE_T) :: msg_cp_sz
    INTEGER(SIZE_T) :: c_msg_size

    INTERFACE
       INTEGER(SIZE_T) FUNCTION H5Eget_msg(msg_id, msg_type, msg, size) &
            BIND(C,NAME='H5Eget_msg')
         IMPORT :: C_CHAR, C_PTR, C_INT
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE  :: msg_id
         INTEGER(C_INT)         :: msg_type
         TYPE(C_PTR)    , VALUE :: msg
         INTEGER(SIZE_T), VALUE :: size
       END FUNCTION H5Eget_msg
    END INTERFACE

    hdferr = 0
    msg_cp_sz = 0
    IF(PRESENT(msg_size))THEN
       IF(msg_size .EQ. 0)THEN
          c_msg_size = H5Eget_msg(msg_id, c_msg_type, C_NULL_PTR, 0_SIZE_T)

          IF(PRESENT(msg_size)) msg_size = c_msg_size
          msg_type = INT(c_msg_type)

          IF(c_msg_size.LT.0) hdferr = -1
          RETURN
       ELSE
          msg_cp_sz = msg_size
       ENDIF
    ENDIF

    IF(msg_cp_sz.EQ.0) msg_cp_sz = LEN(msg)

    ALLOCATE(c_msg(1:msg_cp_sz+1), stat=hdferr)
    IF (hdferr .NE. 0) THEN
       hdferr = -1
       RETURN
    ENDIF
    f_ptr = C_LOC(c_msg(1)(1:1))
    c_msg_size = H5Eget_msg(msg_id, c_msg_type, f_ptr, msg_cp_sz+1_SIZE_T)

    CALL HD5c2fstring(msg, c_msg, msg_cp_sz, msg_cp_sz+1_SIZE_T)

    DEALLOCATE(c_msg)

    IF(PRESENT(msg_size))THEN
       msg_size = c_msg_size
    ENDIF

    msg_type = INT(c_msg_type)

    IF(c_msg_size.LT.0) hdferr = -1

  END SUBROUTINE H5Eget_msg_f

!>
!! \ingroup FH5E
!!
!! \brief Retrieves the number of error messages in an error stack.
!!
!! \param error_stack_id An error message identifier
!! \param count          Number of error messages in \p err_id
!! \param hdferr         \fortran_error
!!
!! See C API: @ref H5Eget_num()
!!
  SUBROUTINE h5eget_num_f(error_stack_id, count, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T) , INTENT(IN)  :: error_stack_id
    INTEGER(SIZE_T), INTENT(OUT) :: count
    INTEGER        , INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(SIZE_T) FUNCTION H5Eget_num(error_stack_id) BIND(C, NAME='H5Eget_num')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: error_stack_id
       END FUNCTION H5Eget_num
    END INTERFACE

    count = H5Eget_num(error_stack_id)

    hdferr = 0
    IF(count.LT.0) hdferr = -1

  END SUBROUTINE h5eget_num_f

!>
!! \ingroup FH5E
!!
!! \brief Walks the specified error stack, calling the specified function.
!!
!! \param err_stack Error stack identifier
!! \param direction Direction in which the error stack is to be walked
!! \param op        Function to be called for each error encountered
!! \param op_data   Data to be passed to func
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Ewalk2()
!!
  SUBROUTINE h5ewalk_f(err_stack, direction, op, op_data, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T) , INTENT(IN)    :: err_stack
    INTEGER        , INTENT(IN)    :: direction
    TYPE(C_FUNPTR) , INTENT(IN)    :: op
    TYPE(C_PTR)    , INTENT(INOUT) :: op_data ! Declare INOUT to bypass gfortran 4.8.5 issue
    INTEGER        , INTENT(OUT)   :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Ewalk(err_stack, direction, op, op_data) &
            BIND(C, NAME='H5Ewalk2')
         IMPORT :: HID_T, C_FUNPTR, C_PTR, C_INT
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: err_stack
         INTEGER(C_INT), VALUE :: direction
         TYPE(C_FUNPTR), VALUE :: op
         TYPE(C_PTR)   , VALUE :: op_data
       END FUNCTION H5Ewalk
    END INTERFACE

    hdferr = INT(H5Ewalk(err_stack, INT(direction, C_INT), op, op_data))

  END SUBROUTINE h5ewalk_f

!>
!! \ingroup FH5E
!!
!! \brief Retrieves an error message.
!!
!! \param class_id Error class identifier
!! \param name     Buffer for the error class name
!! \param hdferr   \fortran_error
!! \param size     The maximum number of characters of the class name to be returned by this function in \p name.
!!
!! If \p size is omitted, the API will copy up to the length of \p name, and it
!! is the application's responsibility to provide a large enough buffer. If \p size
!! is zero, the required buffer size will be returned, and \p name is not accessed.
!! If \p size is greater than zero, the function will copy up to the length
!! of \p size info \p name.
!!
!! See C API: @ref H5Eget_class_name()
!!
  SUBROUTINE H5Eget_class_name_f(class_id, name,  hdferr, size)
    IMPLICIT NONE

    INTEGER(HID_T)  , INTENT(IN)    :: class_id
    CHARACTER(LEN=*)                :: name
    INTEGER         , INTENT(OUT)   :: hdferr
    INTEGER(SIZE_T) , INTENT(INOUT), OPTIONAL :: size

    CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(:), ALLOCATABLE, TARGET :: c_name
    TYPE(C_PTR) :: f_ptr
    INTEGER(SIZE_T) :: name_cp_sz
    INTEGER(SIZE_T) :: c_size

    INTERFACE
       INTEGER(SIZE_T) FUNCTION H5Eget_class_name(class_id, name, size) &
            BIND(C,NAME='H5Eget_class_name')
         IMPORT :: C_PTR, C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T) , VALUE :: class_id
         TYPE(C_PTR)    , VALUE :: name
         INTEGER(SIZE_T), VALUE :: size
       END FUNCTION H5Eget_class_name
    END INTERFACE

    hdferr = 0
    name_cp_sz = 0
    IF(PRESENT(size))THEN
       IF(size .EQ. 0)THEN
          c_size = H5Eget_class_name(class_id, C_NULL_PTR, 0_SIZE_T)

          IF(PRESENT(size)) size = c_size
          IF(c_size.LT.0) hdferr = -1
          RETURN
       ELSE
          name_cp_sz = size
       ENDIF
    ENDIF

    IF(name_cp_sz.EQ.0) name_cp_sz = LEN(name)

    ALLOCATE(c_name(1:name_cp_sz+1), stat=hdferr)
    IF (hdferr .NE. 0) THEN
       hdferr = -1
       RETURN
    ENDIF
    f_ptr = C_LOC(c_name)
    c_size = H5Eget_class_name(class_id, f_ptr, name_cp_sz+1_SIZE_T)

    CALL HD5c2fstring(name, c_name, name_cp_sz, name_cp_sz+1_SIZE_T)
    DEALLOCATE(c_name)

    IF(PRESENT(size))THEN
       size = c_size
    ENDIF

    IF(c_size.LT.0) hdferr = -1

  END SUBROUTINE H5Eget_class_name_f

!>
!! \ingroup FH5E
!!
!! \brief Appends one error stack to another, optionally closing the source stack.
!!
!! \param dst_stack_id       Error stack identifier
!! \param src_stack_id       Error stack identifier
!! \param close_source_stack Flag to indicate whether to close the source stack
!! \param hdferr             \fortran_error
!!
!! See C API: @ref H5Eappend_stack()
!!
  SUBROUTINE H5Eappend_stack_f(dst_stack_id, src_stack_id, close_source_stack, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: dst_stack_id
    INTEGER(HID_T), INTENT(IN)  :: src_stack_id
    LOGICAL       , INTENT(IN)  :: close_source_stack
    INTEGER       , INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Eappend_stack(dst_stack_id, src_stack_id, close_source_stack) &
            BIND(C, NAME='H5Eappend_stack')
         IMPORT :: HID_T, C_BOOL, C_INT
         IMPLICIT NONE
         INTEGER(HID_T) , VALUE :: dst_stack_id
         INTEGER(HID_T) , VALUE :: src_stack_id
         LOGICAL(C_BOOL), VALUE :: close_source_stack
       END FUNCTION H5Eappend_stack
    END INTERFACE

    hdferr = INT(H5Eappend_stack(dst_stack_id, src_stack_id, LOGICAL(close_source_stack, C_BOOL)))

  END SUBROUTINE H5Eappend_stack_f

!>
!! \ingroup FH5E
!!
!! \brief Returns a copy of the current error stack.
!!
!! \param err_stack_id Error stack identifier
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Eget_current_stack()
!!
  SUBROUTINE H5Eget_current_stack_f(err_stack_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(OUT) :: err_stack_id
    INTEGER       , INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Eget_current_stack() BIND(C, NAME='H5Eget_current_stack')
         IMPORT :: HID_T
         IMPLICIT NONE
       END FUNCTION H5Eget_current_stack
    END INTERFACE

    err_stack_id = H5Eget_current_stack()

    hdferr = 0
    IF(err_stack_id.LT.0) hdferr = -1

  END SUBROUTINE H5Eget_current_stack_f

!>
!! \ingroup FH5E
!!
!! \brief Replaces the current error stack.
!!
!! \param err_stack_id Error stack identifier
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5Eset_current_stack()
!!
  SUBROUTINE H5Eset_current_stack_f(err_stack_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN ) :: err_stack_id
    INTEGER       , INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Eset_current_stack(err_stack_id) BIND(C, NAME='H5Eset_current_stack')
         IMPORT :: C_INT, HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: err_stack_id
       END FUNCTION H5Eset_current_stack
    END INTERFACE

    hdferr = INT(H5Eset_current_stack(err_stack_id))

  END SUBROUTINE H5Eset_current_stack_f

!>
!! \ingroup FH5E
!!
!! \brief Closes an error stack handle.
!!
!! \param err_stack_id Error stack identifier
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5Eclose_stack()
!!
  SUBROUTINE H5Eclose_stack_f(err_stack_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN ) :: err_stack_id
    INTEGER       , INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Eclose_stack(err_stack_id) BIND(C, NAME='H5Eclose_stack')
         IMPORT :: C_INT, HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: err_stack_id
       END FUNCTION H5Eclose_stack
    END INTERFACE

    hdferr = INT(H5Eclose_stack(err_stack_id))

  END SUBROUTINE H5Eclose_stack_f

!>
!! \ingroup FH5E
!!
!! \brief Creates a new, empty error stack.
!!
!! \param err_stack_id Error stack identifier
!! \param hdferr       \fortran_error
!!
!! See C API: @ref  H5Ecreate_stack()
!!
  SUBROUTINE H5Ecreate_stack_f(err_stack_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(OUT) :: err_stack_id
    INTEGER       , INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Ecreate_stack() BIND(C, NAME='H5Ecreate_stack')
         IMPORT :: HID_T
         IMPLICIT NONE
       END FUNCTION H5Ecreate_stack
    END INTERFACE

    err_stack_id = H5Ecreate_stack()

    hdferr = 0
    IF(err_stack_id.LT.0) hdferr = -1

  END SUBROUTINE H5Ecreate_stack_f

!>
!! \ingroup FH5E
!!
!! \brief Deletes specified number of error messages from the error stack.
!!
!! \param err_stack_id Error stack identifier
!! \param count        The number of error messages to be deleted from the top of error stack
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5Epop()
!!
  SUBROUTINE H5Epop_f(err_stack_id, count, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T) , INTENT(IN ) :: err_stack_id
    INTEGER(SIZE_T), INTENT(IN ) :: count
    INTEGER        , INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Epop(err_stack_id, count) BIND(C, NAME='H5Epop')
         IMPORT :: C_INT, HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T) , VALUE :: err_stack_id
         INTEGER(SIZE_T), VALUE :: count
       END FUNCTION H5Epop
    END INTERFACE

    hdferr = INT(H5Epop(err_stack_id, count))

  END SUBROUTINE H5Epop_f

END MODULE H5E

