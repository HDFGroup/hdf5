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
  USE H5fortkit
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
         INTEGER(SIZE_T) :: namelen
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

    INTEGER(SIZE_T) :: namelen
    INTERFACE
       INTEGER FUNCTION h5eget_minor_c(error_no, name, namelen) BIND(C,NAME='h5eget_minor_c')
         IMPORT :: C_CHAR, SIZE_T
         INTEGER :: error_no
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         INTEGER(SIZE_T) :: namelen
       END FUNCTION h5eget_minor_c
    END INTERFACE

    namelen = LEN(name)
    hdferr = h5eget_minor_c(error_no, name, namelen)

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
!! \param cls_id Error class identifier
!! \param maj_id Major error identifier
!! \param min_id Minor error identifier
!! \param msg Error description string
!! \param hdferr \fortran_error
!! \param file Name of the file in which the error was detected
!! \param func Name of the function in which the error was detected
!! \param line Line number in the file where the error was detected
!! \param arg[1-20] C style format control strings
!!
!! \note \p arg[1-20] expects C-style format strings, similar to the
!!       system and C functions printf() and fprintf().
!!       Furthermore, special characters, such as ANSI escapes,
!!       will only be interpreted correctly if the Fortran equivalent
!!       is used. For example, to print \p msg "TEXT" in red and has
!!       a space after the text would be:
!!       <br /><br />
!!       \code
!!       (..., "%s TEXT %s"//C_NEW_LINE, hdferr, ..., arg1=ACHAR(27)//"[31m", arg2=ACHAR(27)//"[0m" )
!!       \endcode
!!       <br />Using "\n" instead of C_NEW_LINE will not be interpereted correctly, and similary,
!!             using "\x1B" instead of ACHAR(27)
!!
!!
!! See C API: @ref H5Epush2()
!!
  SUBROUTINE h5epush_f(err_stack, cls_id, maj_id, min_id, msg, hdferr, &
       file, func, line, &
       arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, &
       arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: err_stack
    INTEGER(HID_T), INTENT(IN) :: cls_id
    INTEGER(HID_T), INTENT(IN) :: maj_id
    INTEGER(HID_T), INTENT(IN) :: min_id
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER, INTENT(OUT) :: hdferr

    TYPE(C_PTR), OPTIONAL :: file
    TYPE(C_PTR), OPTIONAL :: func
    TYPE(C_PTR), OPTIONAL :: line
    CHARACTER(LEN=*), OPTIONAL, TARGET :: arg1, arg2, arg3, arg4, arg5, &
         arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, &
         arg16, arg17, arg18, arg19, arg20

    TYPE(C_PTR) :: file_def = C_NULL_PTR
    TYPE(C_PTR) :: func_def = C_NULL_PTR
    TYPE(C_PTR) :: line_def = C_NULL_PTR
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
       INTEGER FUNCTION h5epush_c(err_stack, cls_id, maj_id, min_id, msg, msg_len, file, func, line, &
            arg1, arg2, arg3, arg4, arg5, &
            arg6, arg7, arg8, arg9, arg10, &
            arg11, arg12, arg13, arg14, arg15, &
            arg16, arg17, arg18, arg19, arg20) BIND(C, NAME='h5epush_c')

         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T) :: err_stack
         INTEGER(HID_T) :: cls_id
         INTEGER(HID_T) :: maj_id
         INTEGER(HID_T) :: min_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: msg
         INTEGER :: msg_len

         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         TYPE(C_PTR), VALUE :: line
         TYPE(C_PTR), VALUE :: arg1, arg2, arg3, arg4, &
         arg5, arg6, arg7, arg8, &
         arg9, arg10, arg11, arg12, &
         arg13, arg14, arg15, arg16, &
         arg17, arg18, arg19, arg20

       END FUNCTION h5epush_c
    END INTERFACE

    IF (PRESENT(file)) file_def = file
    IF (PRESENT(func)) func_def = func
    IF (PRESENT(line)) line_def = line

    IF (PRESENT(arg1)) arg1_def = C_LOC(arg1)
    IF (PRESENT(arg2)) arg2_def = C_LOC(arg2)
    IF (PRESENT(arg3)) arg3_def = C_LOC(arg3)
    IF (PRESENT(arg4)) arg4_def = C_LOC(arg4)
    IF (PRESENT(arg5)) arg5_def = C_LOC(arg5)
    IF (PRESENT(arg6)) arg6_def = C_LOC(arg6)
    IF (PRESENT(arg7)) arg7_def = C_LOC(arg7)
    IF (PRESENT(arg8)) arg8_def = C_LOC(arg8)
    IF (PRESENT(arg9)) arg9_def = C_LOC(arg9)
    IF (PRESENT(arg10)) arg10_def = C_LOC(arg10)
    IF (PRESENT(arg11)) arg11_def = C_LOC(arg11)
    IF (PRESENT(arg12)) arg12_def = C_LOC(arg12)
    IF (PRESENT(arg13)) arg13_def = C_LOC(arg13)
    IF (PRESENT(arg14)) arg14_def = C_LOC(arg14)
    IF (PRESENT(arg15)) arg15_def = C_LOC(arg15)
    IF (PRESENT(arg16)) arg16_def = C_LOC(arg16)
    IF (PRESENT(arg17)) arg17_def = C_LOC(arg17)
    IF (PRESENT(arg18)) arg18_def = C_LOC(arg18)
    IF (PRESENT(arg19)) arg19_def = C_LOC(arg19)
    IF (PRESENT(arg20)) arg20_def = C_LOC(arg20)   

    hdferr = h5epush_c(err_stack, cls_id, maj_id, min_id, msg, LEN(msg), &
         file_def, func_def, line_def, &
         arg1_def, arg2_def, arg3_def, arg4_def, arg5_def, &
         arg6_def, arg7_def, arg8_def, arg9_def, arg10_def, &
         arg11_def, arg12_def, arg13_def, arg14_def, arg15_def, &
         arg16_def, arg17_def, arg18_def, arg19_def, arg20_def)

  END SUBROUTINE h5epush_f

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
    c_msg_size = H5Eget_msg(msg_id, c_msg_type, f_ptr, msg_cp_sz+1)

    CALL HD5c2fstring(msg, c_msg, msg_cp_sz, msg_cp_sz+1_SIZE_T)

    DEALLOCATE(c_msg)

    IF(PRESENT(msg_size))THEN
       msg_size = c_msg_size
    ENDIF

    msg_type = INT(c_msg_type)

    IF(c_msg_size.LT.0) hdferr = -1

  END SUBROUTINE H5Eget_msg_f

#if 0
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
         INTEGER(SIZE_T) :: namelen
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
    INTEGER         , INTENT(IN)  :: error_no
    CHARACTER(LEN=*), INTENT(OUT) :: name
    INTEGER         , INTENT(OUT) :: hdferr

    CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(1:LEN(name)+1), TARGET :: c_name
    TYPE(C_PTR) :: f_ptr
    !CHARACTER(LEN=LEN(name), kind=c_char),  POINTER ::

    INTERFACE
       FUNCTION H5Eget_minor(error_no) RESULT(name) BIND(C,NAME='H5Eget_minor')
         IMPORT :: C_PTR, C_INT
         INTEGER(C_INT), VALUE :: error_no
         TYPE(C_PTR)    :: name
       END FUNCTION H5Eget_minor
    END INTERFACE

    f_ptr = C_LOC(c_name(1:1)(1:1))
    f_ptr = H5Eget_minor( INT(error_no, C_INT) )

    hdferr = 0
    IF( .not. c_associated(f_ptr))THEN
       hdferr = -1
       PRINT*, "NOT"
    ELSE
       PRINT*, "YES", c_name(1)
     !  CALL C_F_POINTER(c_name(1), data)
     !  f_ptr = C_LOC(c_name(1:1)(1:1)

       CALL HD5c2fstring(name, c_name, LEN(name))
    ENDIF

  END SUBROUTINE h5eget_minor_f
#endif

END MODULE H5E

