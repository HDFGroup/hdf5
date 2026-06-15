!> @defgroup FH5Z Fortran Filter (H5Z) Interface
!!
!! @see H5Z, C-API
!!
!! @see @ref H5Z_UG, User Guide
!!

!> @ingroup FH5Z
!!
!! @brief This module contains Fortran interfaces for H5Z functions.
!
! COPYRIGHT
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the LICENSE file, which can be found at the root of the source code       *
!   distribution tree, or in https://www.hdfgroup.org/licenses.               *
!   If you do not have access to either file, you may request a copy from     *
!   help@hdfgroup.org.                                                        *
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! NOTES!
!       _____ __  __ _____   ____  _____ _______       _   _ _______
!      |_   _|  \/  |  __ \ / __ \|  __ \__   __|/\   | \ | |__   __|
! ****   | | | \  / | |__) | |  | | |__) | | |  /  \  |  \| |  | |    ****
! ****   | | | |\/| |  ___/| |  | |  _  /  | | / /\ \ | . ` |  | |    ****
! ****  _| |_| |  | | |    | |__| | | \ \  | |/ ____ \| |\  |  | |    ****
!      |_____|_|  |_|_|     \____/|_|  \_\ |_/_/    \_\_| \_|  |_|
!
!  If you add a new H5Z function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!

MODULE H5Z

  USE H5GLOBAL
  USE H5fortkit
  IMPLICIT NONE

  PRIVATE :: h5zconfig_get_param_int_f
  PRIVATE :: h5zconfig_get_param_double_f
  PRIVATE :: h5zconfig_get_param_logical_f
  PRIVATE :: h5zconfig_get_param_str_f
  PRIVATE :: h5zget_filter_info_flags_f
  PRIVATE :: h5zget_filter_info_class_f

  INTEGER, PARAMETER :: H5Z_FILTER_MAX_NAME_LEN_F = 255 !< Maximum length of a filter canonical name
  INTEGER, PARAMETER :: H5Z_FILTER_MAX_DESC_LEN_F = 255 !< Maximum length of a filter description

  ! Private interoperable mirror of H5Z_class_info_t; string fields as C_PTR
  ! because Fortran BIND(C) types cannot hold variable-length C char arrays.
  TYPE, BIND(C) :: h5z_class_info_c_t
     INTEGER(C_INT)  :: id
     INTEGER(C_INT)  :: config_flags
     TYPE(C_PTR)     :: name
     TYPE(C_PTR)     :: description
     LOGICAL(C_BOOL) :: has_set_config
     LOGICAL(C_BOOL) :: has_get_config
  END TYPE h5z_class_info_c_t
  PRIVATE :: h5z_class_info_c_t

!> @brief Registry-level information about a filter (output of h5zget_filter_info_f variant 2)
  TYPE :: h5z_class_info_f_t
     INTEGER :: id           = 0       !< Numeric filter identifier
     INTEGER :: config_flags = 0       !< Bitwise OR of H5Z_FILTER_ENCODE/DECODE_ENABLED_F
     CHARACTER(LEN=H5Z_FILTER_MAX_NAME_LEN_F) :: name        = '' !< Canonical name (blank if none)
     CHARACTER(LEN=H5Z_FILTER_MAX_DESC_LEN_F) :: description = '' !< Human-readable description (blank if none)
     LOGICAL :: has_set_config = .FALSE. !< .TRUE. if plugin exposes set_config callback
     LOGICAL :: has_get_config = .FALSE. !< .TRUE. if plugin exposes get_config callback
  END TYPE h5z_class_info_f_t

!>
!! \ingroup FH5Z
!!
!! \brief Queries filter configuration or retrieves registry-level filter information.
!!
!! This generic interface dispatches based on the type of the second argument:
!! \li \c INTEGER — calls the original H5Zget_filter_info(), returning encode/decode flags only.
!! \li \c TYPE(h5z_class_info_f_t) — calls H5Zget_filter_info2(), returning the full
!!     registry record including the canonical name, description, and callback flags.
!!
!! See C APIs: @ref H5Zget_filter_info(), @ref H5Zget_filter_info2()
!!
  INTERFACE h5zget_filter_info_f
    MODULE PROCEDURE h5zget_filter_info_flags_f
    MODULE PROCEDURE h5zget_filter_info_class_f
  END INTERFACE h5zget_filter_info_f

!>
!! \ingroup FH5Z
!!
!! \brief Retrieves a parameter value from a filter parameter string.
!!
!! This generic interface dispatches to a type-specific implementation based
!! on the declared type of \p value:
!! \li \c INTEGER(C_INT64_T) — calls H5Zconfig_get_int()
!! \li \c REAL(C_DOUBLE)     — calls H5Zconfig_get_double()
!! \li \c LOGICAL            — calls H5Zconfig_get_bool()
!! \li \c CHARACTER(LEN=*)   — calls H5Zconfig_get_str() (uses \p buf_size instead of \p found)
!!
!! \param params   Full parameter string (e.g. \c "level = 6, mode = \"fast\"").
!! \param key      Name of the parameter to retrieve.
!! \param value    Receives the parameter value if found.
!!                 Type determines which C function is called.
!! \param found    (non-string variants) .TRUE. if \p key was present, .FALSE. otherwise.
!! \param buf_size (string variant only) On entry: capacity of \p value in characters;
!!                 on exit: actual length of the value string written.
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Zconfig_get_int(), @ref H5Zconfig_get_double(),
!!            @ref H5Zconfig_get_bool(), @ref H5Zconfig_get_str()
!!
  INTERFACE h5zconfig_get_param_f
    MODULE PROCEDURE h5zconfig_get_param_int_f
    MODULE PROCEDURE h5zconfig_get_param_double_f
    MODULE PROCEDURE h5zconfig_get_param_logical_f
    MODULE PROCEDURE h5zconfig_get_param_str_f
  END INTERFACE h5zconfig_get_param_f

CONTAINS

!>
!! \ingroup FH5Z
!!
!! \brief Unregisters specified filters.
!!
!! \param filter Filter; may have one of the following values:
!!               \li H5Z_FILTER_DEFLATE_F
!!               \li H5Z_FILTER_SZIP_F
!!               \li H5Z_FILTER_NBIT_F
!!               \li H5Z_FILTER_SCALEOFFSET_F
!!               \li H5Z_FILTER_SHUFFLE_F
!!               \li H5Z_FILTER_FLETCHER32_F
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Zunregister()
!!
  SUBROUTINE h5zunregister_f(filter, hdferr)
    USE ISO_C_BINDING, ONLY : C_INT
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: filter
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER(C_INT) FUNCTION H5Zunregister(filter) &
            BIND(C, NAME='H5Zunregister')
         USE ISO_C_BINDING, ONLY : C_INT
         INTEGER(C_INT), VALUE, INTENT(IN) :: filter
       END FUNCTION H5Zunregister
    END INTERFACE
    hdferr = INT(H5Zunregister(INT(filter, C_INT)))
  END SUBROUTINE h5zunregister_f

!>
!! \ingroup FH5Z
!!
!! \brief Queries if filter is available
!!
!! \param filter  Filter; may be one of the following:
!!                \li H5Z_FILTER_DEFLATE_F
!!                \li H5Z_FILTER_SZIP_F
!!                \li H5Z_FILTER_NBIT_F
!!                \li H5Z_FILTER_SCALEOFFSET_F
!!                \li H5Z_FILTER_SHUFFLE_F
!!                \li H5Z_FILTER_FLETCHER32_F
!! \param status  Flag; .TRUE. if filter is available, .FALSE. otherwise.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Zfilter_avail()
!!
  SUBROUTINE h5zfilter_avail_f(filter, status, hdferr)
    USE ISO_C_BINDING, ONLY : C_INT
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: filter
    LOGICAL, INTENT(OUT) :: status
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Zfilter_avail(filter) &
            BIND(C, NAME='H5Zfilter_avail')
         USE ISO_C_BINDING, ONLY : C_INT
         INTEGER(C_INT), VALUE, INTENT(IN) :: filter
       END FUNCTION H5Zfilter_avail
    END INTERFACE

    INTEGER(C_INT) :: c_ret
    c_ret = H5Zfilter_avail(INT(filter, C_INT))
    IF (c_ret < 0_C_INT) THEN
       hdferr = -1
       status = .FALSE.
    ELSE
       hdferr = 0
       status = (c_ret > 0_C_INT)
    END IF

  END SUBROUTINE h5zfilter_avail_f

!>
!! \ingroup FH5Z
!!
!! \brief Queries if filter has its encoder and/or decoder available.
!!
!! \param filter       Filter; may be one of the following:
!!                     \li H5Z_FILTER_DEFLATE_F
!!                     \li H5Z_FILTER_SZIP_F
!!                     \li H5Z_FILTER_NBIT_F
!!                     \li H5Z_FILTER_SCALEOFFSET_F
!!                     \li H5Z_FILTER_SHUFFLE_F
!!                     \li H5Z_FILTER_FLETCHER32_Ffilter
!! \param config_flags Flag, indicates if filter has its encoder and/or decoder available, possible values:
!!                     \li H5Z_FILTER_ENCODE_ENABLED_F
!!                     \li H5Z_FILTER_DECODE_ENABLED_F
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5Zget_filter_info()
!!
  SUBROUTINE h5zget_filter_info_flags_f(filter, config_flags, hdferr)
    USE ISO_C_BINDING, ONLY : C_INT
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: filter
    INTEGER, INTENT(OUT) :: config_flags
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Zget_filter_info(filter, config_flags) &
            BIND(C, NAME='H5Zget_filter_info')
         USE ISO_C_BINDING, ONLY : C_INT
         INTEGER(C_INT), VALUE, INTENT(IN)  :: filter
         INTEGER(C_INT),        INTENT(OUT) :: config_flags
       END FUNCTION H5Zget_filter_info
    END INTERFACE

    INTEGER(C_INT) :: c_flags
    hdferr = INT(H5Zget_filter_info(INT(filter, C_INT), c_flags))
    config_flags = INT(c_flags)

  END SUBROUTINE h5zget_filter_info_flags_f

!>
!! \ingroup FH5Z
!!
!! \brief Retrieves registry-level information about a registered filter.
!!
!! \param filter  Filter identifier (e.g. H5Z_FILTER_DEFLATE_F).
!! \param info    Receives the filter's registry record; see \c h5z_class_info_f_t.
!!                String fields are blank when not provided by the plugin.
!!                Fields \c name and \c description are silently truncated to
!!                \c H5Z_FILTER_MAX_NAME_LEN_F and \c H5Z_FILTER_MAX_DESC_LEN_F
!!                characters respectively.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Zget_filter_info2()
!!
  SUBROUTINE h5zget_filter_info_class_f(filter, info, hdferr)
    USE ISO_C_BINDING, ONLY : C_INT, C_CHAR, C_PTR, C_F_POINTER, c_associated
    IMPLICIT NONE
    INTEGER,                  INTENT(IN)  :: filter
    TYPE(h5z_class_info_f_t), INTENT(OUT) :: info
    INTEGER,                  INTENT(OUT) :: hdferr

    TYPE(h5z_class_info_c_t)                      :: c_info
    CHARACTER(KIND=C_CHAR), POINTER, DIMENSION(:) :: cp

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Zget_filter_info2(filter, info) &
            BIND(C, NAME='H5Zget_filter_info2')
         USE ISO_C_BINDING, ONLY : C_INT
         IMPORT :: h5z_class_info_c_t
         INTEGER(C_INT),           VALUE, INTENT(IN)  :: filter
         TYPE(h5z_class_info_c_t),        INTENT(OUT) :: info
       END FUNCTION H5Zget_filter_info2
    END INTERFACE

    hdferr = INT(H5Zget_filter_info2(INT(filter, C_INT), c_info))

    IF (hdferr >= 0) THEN
       info%id           = INT(c_info%id)
       info%config_flags = INT(c_info%config_flags)
       info%has_set_config = LOGICAL(c_info%has_set_config)
       info%has_get_config = LOGICAL(c_info%has_get_config)
       info%name        = ' '
       info%description = ' '
       ! Dereference C string pointers using the known name-length bound (255 bytes per RFC).
       ! HD5c2fstring stops at the first C_NULL_CHAR, so over-allocation is safe.
       IF (c_associated(c_info%name)) THEN
          CALL c_f_pointer(c_info%name, cp, [H5Z_FILTER_MAX_NAME_LEN_F + 1])
          CALL HD5c2fstring(info%name, cp, &
                            INT(H5Z_FILTER_MAX_NAME_LEN_F, SIZE_T), &
                            INT(H5Z_FILTER_MAX_NAME_LEN_F + 1, SIZE_T))
       END IF
       IF (c_associated(c_info%description)) THEN
          CALL c_f_pointer(c_info%description, cp, [H5Z_FILTER_MAX_DESC_LEN_F + 1])
          CALL HD5c2fstring(info%description, cp, &
                            INT(H5Z_FILTER_MAX_DESC_LEN_F, SIZE_T), &
                            INT(H5Z_FILTER_MAX_DESC_LEN_F + 1, SIZE_T))
       END IF
    END IF

  END SUBROUTINE h5zget_filter_info_class_f

!>
!! \ingroup FH5Z
!!
!! \brief Checks whether a key exists in a filter parameter string.
!!
!! \param params  Full parameter string (e.g. "level = 6, mode = \"fast\"").
!! \param key     Name of the parameter to look for.
!! \param found   .TRUE. if key exists, .FALSE. otherwise.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Zconfig_has_key()
!!
  SUBROUTINE h5zconfig_has_key_f(params, key, found, hdferr)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)  :: params
    CHARACTER(LEN=*), INTENT(IN)  :: key
    LOGICAL,          INTENT(OUT) :: found
    INTEGER,          INTENT(OUT) :: hdferr

    CHARACTER(LEN=LEN_TRIM(params)+1,KIND=C_CHAR) :: c_params
    CHARACTER(LEN=LEN_TRIM(key)+1,KIND=C_CHAR)    :: c_key
    INTEGER(C_INT)                                :: status

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Zconfig_has_key_c(params_c, key_c) &
            BIND(C,NAME='H5Zconfig_has_key')
         IMPORT :: C_INT, C_CHAR
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: params_c
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: key_c
       END FUNCTION H5Zconfig_has_key_c
    END INTERFACE

    c_params = TRIM(params)//C_NULL_CHAR
    c_key    = TRIM(key)//C_NULL_CHAR
    status   = H5Zconfig_has_key_c(c_params, c_key)
    hdferr   = INT(status)
    found    = (status > 0)
  END SUBROUTINE h5zconfig_has_key_f

  SUBROUTINE h5zconfig_get_param_int_f(params, key, value, found, hdferr)
    IMPLICIT NONE
    CHARACTER(LEN=*),    INTENT(IN)  :: params
    CHARACTER(LEN=*),    INTENT(IN)  :: key
    INTEGER(C_INT64_T),  INTENT(OUT) :: value
    LOGICAL,             INTENT(OUT) :: found
    INTEGER,             INTENT(OUT) :: hdferr

    CHARACTER(LEN=LEN_TRIM(params)+1,KIND=C_CHAR) :: c_params
    CHARACTER(LEN=LEN_TRIM(key)+1,KIND=C_CHAR)    :: c_key
    INTEGER(C_INT64_T)                             :: c_val
    INTEGER(C_INT)                                 :: status

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Zconfig_get_int_c(params_c, key_c, out_c) &
            BIND(C,NAME='H5Zconfig_get_int')
         IMPORT :: C_INT, C_CHAR, C_INT64_T
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN)  :: params_c
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN)  :: key_c
         INTEGER(C_INT64_T),                   INTENT(OUT) :: out_c
       END FUNCTION H5Zconfig_get_int_c
    END INTERFACE

    c_params = TRIM(params)//C_NULL_CHAR
    c_key    = TRIM(key)//C_NULL_CHAR
    c_val    = 0_C_INT64_T
    status   = H5Zconfig_get_int_c(c_params, c_key, c_val)
    hdferr   = INT(status)
    found    = (status > 0)
    IF (found) value = c_val
  END SUBROUTINE h5zconfig_get_param_int_f

  SUBROUTINE h5zconfig_get_param_double_f(params, key, value, found, hdferr)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)  :: params
    CHARACTER(LEN=*), INTENT(IN)  :: key
    REAL(C_DOUBLE),   INTENT(OUT) :: value
    LOGICAL,          INTENT(OUT) :: found
    INTEGER,          INTENT(OUT) :: hdferr

    CHARACTER(LEN=LEN_TRIM(params)+1,KIND=C_CHAR) :: c_params
    CHARACTER(LEN=LEN_TRIM(key)+1,KIND=C_CHAR)    :: c_key
    REAL(C_DOUBLE)                                 :: c_val
    INTEGER(C_INT)                                 :: status

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Zconfig_get_double_c(params_c, key_c, out_c) &
            BIND(C,NAME='H5Zconfig_get_double')
         IMPORT :: C_INT, C_CHAR, C_DOUBLE
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN)  :: params_c
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN)  :: key_c
         REAL(C_DOUBLE),                       INTENT(OUT) :: out_c
       END FUNCTION H5Zconfig_get_double_c
    END INTERFACE

    c_params = TRIM(params)//C_NULL_CHAR
    c_key    = TRIM(key)//C_NULL_CHAR
    c_val    = 0.0_C_DOUBLE
    status   = H5Zconfig_get_double_c(c_params, c_key, c_val)
    hdferr   = INT(status)
    found    = (status > 0)
    IF (found) value = c_val
  END SUBROUTINE h5zconfig_get_param_double_f

  SUBROUTINE h5zconfig_get_param_logical_f(params, key, value, found, hdferr)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)  :: params
    CHARACTER(LEN=*), INTENT(IN)  :: key
    LOGICAL,          INTENT(OUT) :: value
    LOGICAL,          INTENT(OUT) :: found
    INTEGER,          INTENT(OUT) :: hdferr

    CHARACTER(LEN=LEN_TRIM(params)+1,KIND=C_CHAR) :: c_params
    CHARACTER(LEN=LEN_TRIM(key)+1,KIND=C_CHAR)    :: c_key
    LOGICAL(C_BOOL)                                :: c_val   ! matches hbool_t = bool (1 byte)
    INTEGER(C_INT)                                 :: status

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Zconfig_get_bool_c(params_c, key_c, out_c) &
            BIND(C,NAME='H5Zconfig_get_bool')
         IMPORT :: C_INT, C_BOOL, C_CHAR
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN)  :: params_c
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN)  :: key_c
         LOGICAL(C_BOOL),                      INTENT(OUT) :: out_c
       END FUNCTION H5Zconfig_get_bool_c
    END INTERFACE

    c_params = TRIM(params)//C_NULL_CHAR
    c_key    = TRIM(key)//C_NULL_CHAR
    c_val    = .FALSE._C_BOOL
    status   = H5Zconfig_get_bool_c(c_params, c_key, c_val)
    hdferr   = INT(status)
    found    = (status > 0)
    IF (found) value = LOGICAL(c_val)
  END SUBROUTINE h5zconfig_get_param_logical_f

  SUBROUTINE h5zconfig_get_param_str_f(params, key, value, buf_size, hdferr)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)    :: params
    CHARACTER(LEN=*), INTENT(IN)    :: key
    CHARACTER(LEN=*), INTENT(OUT)   :: value
    INTEGER(SIZE_T),  INTENT(INOUT) :: buf_size
    INTEGER,          INTENT(OUT)   :: hdferr

    CHARACTER(LEN=LEN_TRIM(params)+1,KIND=C_CHAR)             :: c_params
    CHARACTER(LEN=LEN_TRIM(key)+1,KIND=C_CHAR)                :: c_key
    CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(1:LEN(value)+1)   :: c_valbuf
    INTEGER(SIZE_T)                                            :: c_bufsz

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Zconfig_get_str_c(params_c, key_c, value_buf_c, buf_size_c) &
            BIND(C,NAME='H5Zconfig_get_str')
         IMPORT :: C_INT, C_CHAR, SIZE_T
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN)    :: params_c
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN)    :: key_c
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT)   :: value_buf_c
         INTEGER(SIZE_T),                      INTENT(INOUT) :: buf_size_c
       END FUNCTION H5Zconfig_get_str_c
    END INTERFACE

    c_params = TRIM(params)//C_NULL_CHAR
    c_key    = TRIM(key)//C_NULL_CHAR
    c_bufsz  = INT(LEN(value) + 1_SIZE_T, SIZE_T)
    hdferr   = INT(H5Zconfig_get_str_c(c_params, c_key, c_valbuf, c_bufsz))
    buf_size = c_bufsz
    IF (hdferr > 0) &
       CALL HD5c2fstring(value, c_valbuf, &
                         MIN(c_bufsz, INT(LEN(value), SIZE_T)), &
                         MIN(c_bufsz, INT(LEN(value), SIZE_T)) + 1_SIZE_T)
  END SUBROUTINE h5zconfig_get_param_str_f

END MODULE H5Z
