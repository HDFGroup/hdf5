!> @defgroup FH5F Fortran File (H5F) Interface
!!
!! @see H5F, C-API
!!
!! @see @ref H5F_UG, User Guide
!!

!> @ingroup FH5F
!!
!! @brief This module contains Fortran interfaces for H5F functions.
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
!  If you add a new H5F function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!

MODULE H5F

  USE H5GLOBAL
  IMPLICIT NONE

  ! Number of objects opened in H5open_f
  INTEGER(SIZE_T) :: H5OPEN_NUM_OBJ


#ifndef H5_DOXYGEN
  INTERFACE
     INTEGER(C_INT) FUNCTION h5fis_accessible(name, &
          access_prp_default) BIND(C,NAME='H5Fis_accessible')
       IMPORT :: C_CHAR
       IMPORT :: HID_T
       IMPORT :: C_INT
       IMPLICIT NONE
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
       INTEGER(HID_T), INTENT(IN), VALUE :: access_prp_default
     END FUNCTION h5fis_accessible
  END INTERFACE
#endif

!> @brief H5F_info_t_super derived type.
  TYPE, BIND(C) :: H5F_info_super_t
     INTEGER(C_INT)   :: version        !< Superblock version number
     INTEGER(HSIZE_T) :: super_size     !< Superblock size
     INTEGER(HSIZE_T) :: super_ext_size !< Superblock extension size
  END TYPE  H5F_info_super_t

!> @brief H5F_info_t_free derived type.
  TYPE, BIND(C) :: H5F_info_free_t
     INTEGER(C_INT)   :: version   !< Version # of file free space management
     INTEGER(HSIZE_T) :: meta_size !< Free space manager metadata size
     INTEGER(HSIZE_T) :: tot_space !< Amount of free space in the file
  END TYPE H5F_info_free_t

!> @brief H5F_info_t_sohm derived type.
  TYPE, BIND(C) :: H5F_info_sohm_t
        INTEGER(C_INT)     :: version  !< Version # of shared object header info
        INTEGER(HSIZE_T)   :: hdr_size  !< Shared object header message header size
        TYPE(H5_ih_info_t) :: msgs_info !< Shared object header message index & heap size
  END TYPE H5F_info_sohm_t

!> @brief h5f_info_t derived type.
  TYPE, BIND(C) :: h5f_info_t
     TYPE(H5F_info_super_t) :: super
     TYPE(H5F_info_free_t)  :: free
     TYPE(H5F_info_sohm_t)  :: sohm
  END TYPE h5f_info_t

CONTAINS
!>
!! \ingroup FH5F
!!
!! \brief Creates HDF5 files.
!!
!! \param name         Name of the file to create.
!! \param access_flags File access flags. Allowable values are:
!!                     \li H5F_ACC_TRUNC_F
!!                     \li H5F_ACC_EXCL_F
!! \param file_id      File identifier.
!! \param hdferr       \fortran_error
!! \param creation_prp File creation property list identifier.
!! \param access_prp   File access property list identifier.
!!
!! See C API: @ref H5Fcreate()
!!
  SUBROUTINE h5fcreate_f(name, access_flags, file_id, hdferr, &
       creation_prp, access_prp)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(IN) :: access_flags
    INTEGER(HID_T), INTENT(OUT) :: file_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: creation_prp
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: access_prp

    INTEGER(HID_T) :: creation_prp_default
    INTEGER(HID_T) :: access_prp_default
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Fcreate(name, access_flags, &
            creation_prp_default, access_prp_default) BIND(C,NAME='H5Fcreate')
         IMPORT :: C_CHAR, C_INT
         IMPORT :: HID_T
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         INTEGER(C_INT), VALUE :: access_flags
         INTEGER(HID_T), VALUE :: creation_prp_default
         INTEGER(HID_T), VALUE :: access_prp_default
       END FUNCTION H5Fcreate
    END INTERFACE

    c_name  = TRIM(name)//C_NULL_CHAR

    creation_prp_default = H5P_DEFAULT_F
    access_prp_default = H5P_DEFAULT_F

    IF (PRESENT(creation_prp)) creation_prp_default = creation_prp
    IF (PRESENT(access_prp))   access_prp_default   = access_prp

    file_id = h5fcreate(c_name, INT(access_flags, C_INT), &
         creation_prp_default, access_prp_default)

    hdferr = 0
    IF(file_id.LT.0) hdferr = -1

  END SUBROUTINE h5fcreate_f

!>
!! \ingroup FH5F
!!
!! \brief Asynchronously creates HDF5 files.
!!
!! \param name         Name of the file to create
!! \param access_flags File access flags. Allowable values are:
!!                     \li H5F_ACC_TRUNC_F
!!                     \li H5F_ACC_EXCL_F
!! \param file_id      File identifier
!! \param es_id        \fortran_es_id
!! \param hdferr       \fortran_error
!! \param creation_prp File creation property list identifier
!! \param access_prp   File access property list identifier
!! \param file         \fortran_file
!! \param func         \fortran_func
!! \param line         \fortran_line
!!
!! See C API: @ref H5Fcreate_async()
!!
  SUBROUTINE h5fcreate_async_f(name, access_flags, file_id, es_id, hdferr, &
       creation_prp, access_prp, file, func, line)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(IN) :: access_flags
    INTEGER(HID_T), INTENT(OUT) :: file_id
    INTEGER(HID_T), INTENT(IN)  :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: creation_prp
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: access_prp
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    INTEGER(HID_T) :: creation_prp_default
    INTEGER(HID_T) :: access_prp_default
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name
    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Fcreate_async(file, func, line, name, access_flags, &
            creation_prp_default, access_prp_default, es_id) BIND(C,NAME='H5Fcreate_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         INTEGER, VALUE :: access_flags
         INTEGER(HID_T), VALUE :: creation_prp_default
         INTEGER(HID_T), VALUE :: access_prp_default
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Fcreate_async
    END INTERFACE

    c_name  = TRIM(name)//C_NULL_CHAR

    creation_prp_default = H5P_DEFAULT_F
    access_prp_default = H5P_DEFAULT_F

    IF(PRESENT(creation_prp)) creation_prp_default = creation_prp
    IF(PRESENT(access_prp))   access_prp_default   = access_prp
    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    file_id = H5Fcreate_async(file_default, func_default, line_default, &
         c_name, access_flags, creation_prp_default, access_prp_default, es_id)

    hdferr = 0
    IF(file_id.LT.0) hdferr = -1

  END SUBROUTINE h5fcreate_async_f
!>
!! \ingroup FH5F
!!
!! \brief Flushes all buffers associated with a file to disk.
!!
!! \param object_id Identifier of object used to identify the file.
!! \param scope     Specifies the scope of the flushing action. Possible values are:
!!                  \li H5F_SCOPE_GLOBAL_F
!!                  \li H5F_SCOPE_LOCAL_F
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Fflush()
!!
  SUBROUTINE h5fflush_f(object_id, scope, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: object_id
    INTEGER, INTENT(IN) :: scope
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION H5Fflush(object_id, scope) BIND(C,NAME='H5Fflush')
         IMPORT :: C_INT
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: object_id
         INTEGER(C_INT), VALUE :: scope
       END FUNCTION H5Fflush
    END INTERFACE

    hdferr = H5Fflush(object_id, INT(scope, C_INT))

  END SUBROUTINE h5fflush_f
!>
!! \ingroup FH5F
!!
!! \brief Deletes an HDF5 file
!!
!! \param name       Name of the file to delete
!! \param hdferr     \fortran_error
!! \param access_prp File access property list identifier
!!
!! See C API: @ref H5Fdelete()
!!
  SUBROUTINE h5fdelete_f(name, hdferr, access_prp)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)           :: name
    INTEGER         , INTENT(OUT)          :: hdferr
    INTEGER(HID_T)  , INTENT(IN), OPTIONAL :: access_prp

    INTEGER(HID_T) :: access_prp_default
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Fdelete(name, access_prp_default) BIND(C,NAME='H5Fdelete')
         IMPORT :: C_CHAR, C_INT
         IMPORT :: HID_T
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         INTEGER(HID_T), VALUE :: access_prp_default
       END FUNCTION H5Fdelete
    END INTERFACE

    c_name  = TRIM(name)//C_NULL_CHAR

    access_prp_default = H5P_DEFAULT_F
    IF (PRESENT(access_prp)) access_prp_default = access_prp

    hdferr = INT(H5Fdelete(c_name, access_prp_default))

  END SUBROUTINE h5fdelete_f
!>
!! \ingroup FH5F
!!
!! \brief Asynchronously flushes all buffers associated with a file to disk.
!!
!! \param object_id Identifier of object used to identify the file.
!! \param scope     Specifies the scope of the flushing action. Possible values are:
!!                  \li H5F_SCOPE_GLOBAL_F
!!                  \li H5F_SCOPE_LOCAL_F
!! \param es_id     \fortran_es_id
!! \param hdferr    \fortran_error
!! \param file    \fortran_file
!! \param func    \fortran_func
!! \param line    \fortran_line
!!
!! See C API: @ref H5Fflush_async()
!!
  SUBROUTINE h5fflush_async_f(object_id, scope, es_id, hdferr, file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: object_id
    INTEGER, INTENT(IN) :: scope
    INTEGER(HID_T), INTENT(IN)  :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Fflush_async(file, func, line, object_id, scope, es_id) &
            BIND(C,NAME='H5Fflush_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: object_id
         INTEGER(C_INT), VALUE :: scope
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Fflush_async
    END INTERFACE

    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    hdferr = INT(H5Fflush_async(file_default, func_default, line_default, &
         object_id, INT(scope, C_INT), es_id))

  END SUBROUTINE h5fflush_async_f
!>
!! \ingroup FH5F
!!
!! \brief Mounts a file.
!!
!! \param loc_id     The identifier for of file or group in which name is defined.
!! \param name       The name of the group onto which the file specified by child_id is to be mounted.
!! \param child_id   The identifier of the file to be mounted.
!! \param hdferr     \fortran_error
!! \param access_prp The identifier of the property list to be used.
!!
!! See C API: @ref H5Fmount()
!!
  SUBROUTINE h5fmount_f(loc_id, name, child_id, hdferr, access_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(HID_T), INTENT(IN) :: child_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: access_prp
    INTEGER(HID_T) :: access_prp_default
    INTEGER :: namelen ! Length of the name character string

    INTERFACE
       INTEGER FUNCTION h5fmount_c(loc_id, name, namelen, &
            child_id, access_prp_default) BIND(C,NAME='h5fmount_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER(HID_T), INTENT(IN) :: child_id
         INTEGER(HID_T), INTENT(IN) :: access_prp_default
         INTEGER :: namelen
       END FUNCTION h5fmount_c
    END INTERFACE

    access_prp_default = H5P_DEFAULT_F
    IF (PRESENT(access_prp))   access_prp_default   = access_prp
    namelen = LEN_TRIM(name)
    hdferr = h5fmount_c(loc_id, name, namelen, child_id, access_prp_default)

  END SUBROUTINE h5fmount_f

!>
!! \ingroup FH5F
!!
!! \brief Unmounts a file.
!!
!! \param loc_id The identifier for of file or group in which name is defined.
!! \param name   The name of the mount point.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Funmount()
!!
  SUBROUTINE h5funmount_f(loc_id, name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: namelen ! Length of the name character string

    INTERFACE
       INTEGER FUNCTION h5funmount_c(loc_id, name, namelen) BIND(C,NAME='h5funmount_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER :: namelen
       END FUNCTION h5funmount_c
    END INTERFACE

    namelen = LEN_TRIM(name)
    hdferr = h5funmount_c(loc_id, name, namelen)

  END SUBROUTINE h5funmount_f

!>
!! \ingroup FH5F
!!
!! \brief Opens HDF5 file.
!!
!! \param name         Name of the file to acecss.
!! \param access_flags File access flags. Allowable values are:
!!                     \li H5F_ACC_RDWR_F
!!                     \li H5F_ACC_RDONLY_F
!! \param file_id      File identifier.
!! \param hdferr       \fortran_error
!! \param access_prp   File access property list identifier.
!!
!! See C API: @ref H5Fopen()
!!
  SUBROUTINE h5fopen_f(name, access_flags, file_id, hdferr, access_prp)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(IN) :: access_flags
    INTEGER(HID_T), INTENT(OUT) :: file_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: access_prp

    INTEGER(HID_T) :: access_prp_default
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Fopen(name, access_flags, access_prp_default) &
            BIND(C,NAME='H5Fopen')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         INTEGER(C_INT), VALUE :: access_flags
         INTEGER(HID_T), VALUE :: access_prp_default
       END FUNCTION H5Fopen
    END INTERFACE

    c_name  = TRIM(name)//C_NULL_CHAR

    access_prp_default = H5P_DEFAULT_F

    IF(PRESENT(access_prp)) access_prp_default = access_prp

    file_id = H5Fopen(c_name, INT(access_flags, C_INT), access_prp_default)

    hdferr = 0
    IF(file_id.LT.0) hdferr = -1

  END SUBROUTINE h5fopen_f

!>
!! \ingroup FH5F
!!
!! \brief Asynchronously opens HDF5 file.
!!
!! \param name         Name of the file to acecss.
!! \param access_flags File access flags. Allowable values are:
!!                     \li H5F_ACC_RDWR_F
!!                     \li H5F_ACC_RDONLY_F
!! \param file_id      File identifier
!! \param es_id        \fortran_es_id
!! \param hdferr       \fortran_error
!! \param access_prp   File access property list identifier
!! \param file         \fortran_file
!! \param func         \fortran_func
!! \param line         \fortran_line
!!
!! See C API: @ref H5Fopen_async()
!!
  SUBROUTINE h5fopen_async_f(name, access_flags, file_id, es_id, hdferr, &
       access_prp, file, func, line)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(IN) :: access_flags
    INTEGER(HID_T), INTENT(OUT) :: file_id
    INTEGER(HID_T), INTENT(IN)  :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: access_prp
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    INTEGER(HID_T) :: access_prp_default
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name
    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Fopen_async(file, func, line, name, access_flags, access_prp_default, es_id) &
            BIND(C,NAME='H5Fopen_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         INTEGER(C_INT), VALUE :: access_flags
         INTEGER(HID_T), VALUE :: access_prp_default
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Fopen_async
    END INTERFACE

    c_name  = TRIM(name)//C_NULL_CHAR

    access_prp_default = H5P_DEFAULT_F

    IF(PRESENT(access_prp)) access_prp_default = access_prp
    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    file_id = H5Fopen_async(file_default, func_default, line_default, &
         c_name, INT(access_flags, C_INT), access_prp_default, es_id)

    hdferr = 0
    IF(file_id.LT.0) hdferr = -1

  END SUBROUTINE h5fopen_async_f
!>
!! \ingroup FH5F
!!
!! \brief Reopens HDF5 file.
!!
!! \param file_id     Identifier of a file for which an additional identifier is required.
!! \param ret_file_id New file identifier.
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Freopen()
!!
  SUBROUTINE h5freopen_f(file_id, ret_file_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: file_id
    INTEGER(HID_T), INTENT(OUT) :: ret_file_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER(HID_T) FUNCTION H5Freopen(file_id) BIND(C,NAME='H5Freopen')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: file_id
       END FUNCTION H5Freopen
    END INTERFACE

    ret_file_id = h5freopen(file_id)

    hdferr = 0
    IF(ret_file_id.LT.0) hdferr = -1

  END SUBROUTINE h5freopen_f
!>
!! \ingroup FH5F
!!
!! \brief  Asynchronously reopens HDF5 file.
!!
!! \param file_id     Identifier of a file for which an additional identifier is required.
!! \param ret_file_id New file identifier.
!! \param es_id       \fortran_es_id
!! \param hdferr      \fortran_error
!! \param file        \fortran_file
!! \param func        \fortran_func
!! \param line        \fortran_line
!!
!! See C API: @ref H5Freopen_async()
!!
  SUBROUTINE h5freopen_async_f(file_id, ret_file_id, es_id, hdferr, file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: file_id
    INTEGER(HID_T), INTENT(OUT) :: ret_file_id
    INTEGER(HID_T), INTENT(IN)  :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Freopen_async(file, func, line, file_id, es_id) &
            BIND(C,NAME='H5Freopen_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: file_id
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Freopen_async
    END INTERFACE

    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    ret_file_id = h5freopen_async(file_default, func_default, line_default, file_id, es_id)

    hdferr = 0
    IF(ret_file_id.LT.0) hdferr = -1

  END SUBROUTINE h5freopen_async_f
!>
!! \ingroup FH5F
!!
!! \brief Returns a file creation property list identifier.
!!
!! \param file_id Identifier of a file to creation property list of.
!! \param prop_id Creation property list identifier.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Fget_create_plist()
!!
  SUBROUTINE h5fget_create_plist_f(file_id, prop_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: file_id
    INTEGER(HID_T), INTENT(OUT) :: prop_id
    INTEGER       , INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER(HID_T) FUNCTION H5Fget_create_plist(file_id) BIND(C,NAME='H5Fget_create_plist')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: file_id
       END FUNCTION H5Fget_create_plist
    END INTERFACE

    prop_id = H5Fget_create_plist(file_id)

    hdferr = 0
    IF(prop_id.LT.0) hdferr = -1

  END SUBROUTINE h5fget_create_plist_f
!>
!! \ingroup FH5F
!!
!! \brief Returns a file access property list identifier.
!!
!! \param file_id   Identifier of a file to creation property list of.
!! \param access_id Access property list identifier.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Fget_access_plist()
!!
  SUBROUTINE h5fget_access_plist_f(file_id, access_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: file_id
    INTEGER(HID_T), INTENT(OUT) :: access_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER(HID_T) FUNCTION H5Fget_access_plist(file_id) BIND(C,NAME='H5Fget_access_plist')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: file_id
       END FUNCTION H5Fget_access_plist
    END INTERFACE

    access_id = H5Fget_access_plist(file_id)

    hdferr = 0
    IF(access_id.LT.0) hdferr = -1

  END SUBROUTINE h5fget_access_plist_f

!>
!! \ingroup FH5F
!!
!! \brief Determines whether a file can be accessed as HDF5.
!!
!! \param name       Name of the file to check.
!! \param status     Indicates if file is and HDF5 file.
!! \param hdferr     \fortran_error
!! \param access_prp File access property list identifier.
!!
!! See C API: @ref H5Fis_accessible()
!!
  SUBROUTINE h5fis_accessible_f(name, status, hdferr, access_prp)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: name
    LOGICAL, INTENT(OUT) :: status
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: access_prp

    INTEGER(HID_T) :: access_prp_default
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name
    INTEGER(C_INT) :: flag    ! "TRUE/FALSE/ERROR" flag from C routine

    access_prp_default = H5P_DEFAULT_F
    IF (PRESENT(access_prp)) access_prp_default = access_prp

    c_name = TRIM(name)//C_NULL_CHAR

    flag = H5Fis_accessible(c_name, access_prp_default)

    hdferr = 0
    IF(flag.LT.0) hdferr = -1

    status = .TRUE.
    IF (flag .EQ. 0) status = .FALSE.

  END SUBROUTINE h5fis_accessible_f

! XXX (VOL_MERGE): This function should probably be marked as
!                  deprecated since H5Fis_hdf5() is deprecated.

!>
!! \ingroup FH5F
!!
!! \brief Determines whether a file is in the HDF5 format.
!!
!! \param name   Name of the file to check.
!! \param status Indicates if file is and HDF5 file.
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Fis_hdf5()
!!
  SUBROUTINE h5fis_hdf5_f(name, status, hdferr)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: name
    LOGICAL, INTENT(OUT) :: status
    INTEGER, INTENT(OUT) :: hdferr
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name
    INTEGER(C_INT) :: flag    ! "TRUE/FALSE/ERROR" flag from C routine
                              ! to define status value.

    c_name = TRIM(name)//C_NULL_CHAR

    flag = H5Fis_accessible(c_name, H5P_DEFAULT_F)

    hdferr = 0
    IF(flag.LT.0) hdferr = -1

    status = .TRUE.
    IF (flag .EQ. 0) status = .FALSE.

  END SUBROUTINE h5fis_hdf5_f

!>
!! \ingroup FH5F
!!
!! \brief Closes HDF5 file.
!!
!! \param file_id File identifier.
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Fclose()
!!
  SUBROUTINE h5fclose_f(file_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: file_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER(C_INT) FUNCTION H5Fclose(file_id) BIND(C,NAME='H5Fclose')
         IMPORT :: C_INT
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: file_id
       END FUNCTION H5Fclose
    END INTERFACE

    hdferr = INT(H5Fclose(file_id))

  END SUBROUTINE h5fclose_f

!>
!! \ingroup FH5F
!!
!! \brief Asynchronously closes HDF5 file.
!!
!! \param file_id File identifier
!! \param es_id   \fortran_es_id
!! \param hdferr  \fortran_error
!! \param file    \fortran_file
!! \param func    \fortran_func
!! \param line    \fortran_line
!!
!! See C API: @ref H5Fclose_async()
!!
  SUBROUTINE h5fclose_async_f(file_id, es_id, hdferr, file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: file_id
    INTEGER(HID_T), INTENT(IN)  :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Fclose_async(file, func, line, file_id, es_id) &
            BIND(C,NAME='H5Fclose_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: file_id
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Fclose_async
    END INTERFACE

    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    hdferr = INT(H5Fclose_async(file_default, func_default, line_default, file_id, es_id))

  END SUBROUTINE h5fclose_async_f

!>
!! \ingroup FH5F
!!
!! \brief Gets number of the objects open within a file.
!!
!! \param file_id   File identifier
!! \param obj_type  Type of the object; possible values are:
!!                  \li H5F_OBJ_FILE_F
!!                  \li H5F_OBJ_DATASET_F
!!                  \li H5F_OBJ_GROUP_F
!!                  \li H5F_OBJ_DATATYPE_F
!!                  \li H5F_OBJ_ALL_F
!! \param obj_count Number of open objects
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Fget_obj_count()
!!
  SUBROUTINE h5fget_obj_count_f(file_id, obj_type, obj_count, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T) , INTENT(IN)  :: file_id
    INTEGER        , INTENT(IN)  :: obj_type
    INTEGER(SIZE_T), INTENT(OUT) :: obj_count
    INTEGER        , INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(SIZE_T) FUNCTION H5Fget_obj_count(file_id, obj_type) BIND(C,NAME='H5Fget_obj_count')
         IMPORT :: C_INT
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: file_id
         INTEGER(C_INT), VALUE :: obj_type
       END FUNCTION H5Fget_obj_count
    END INTERFACE

    obj_count = H5Fget_obj_count(file_id, INT(obj_type, C_INT))

    hdferr = 0
    IF(obj_count.LT.0) hdferr = -1

    ! Don't include objects created by H5open in the H5F_OBJ_ALL_F count
    IF(file_id.EQ.INT(H5F_OBJ_ALL_F,HID_T))THEN
       obj_count = obj_count - H5OPEN_NUM_OBJ
    ENDIF

  END SUBROUTINE h5fget_obj_count_f

!>
!! \ingroup FH5F
!!
!! \brief Get list of open objects identifiers within a file.
!!
!! \param file_id  File identifier
!! \param obj_type Type of the object; possible values are:
!!                 \li H5F_OBJ_FILE_F
!!                 \li H5F_OBJ_DATASET_F
!!                 \li H5F_OBJ_GROUP_F
!!                 \li H5F_OBJ_DATATYPE_F
!!                 \li H5F_OBJ_ALL_F
!! \param max_objs Maximum # of objects to retrieve
!! \param obj_ids  Array of open object identifiers
!! \param hdferr   \fortran_error
!! \param num_objs Number of open objects
!!
!! See C API: @ref H5Fget_obj_ids()
!!
  SUBROUTINE h5fget_obj_ids_f(file_id, obj_type, max_objs, obj_ids, hdferr, num_objs)
    IMPLICIT NONE
    INTEGER(HID_T) , INTENT(IN)  :: file_id
    INTEGER        , INTENT(IN)  :: obj_type
    INTEGER(SIZE_T), INTENT(IN)  :: max_objs
    INTEGER(HID_T) , DIMENSION(*), INTENT(INOUT) :: obj_ids
    INTEGER        , INTENT(OUT) :: hdferr
    INTEGER(SIZE_T), INTENT(OUT), OPTIONAL :: num_objs

    INTEGER(SIZE_T) :: c_num_objs ! Number of open objects of the specified type

    INTERFACE
       INTEGER(SIZE_T) FUNCTION H5Fget_obj_ids(file_id, obj_type, max_objs, obj_ids) &
            BIND(C,NAME='H5Fget_obj_ids')
         IMPORT :: C_INT
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T) , VALUE :: file_id
         INTEGER(C_INT) , VALUE :: obj_type
         INTEGER(SIZE_T), VALUE :: max_objs
         INTEGER(HID_T) , DIMENSION(*) :: obj_ids
       END FUNCTION H5Fget_obj_ids
    END INTERFACE

    c_num_objs = H5Fget_obj_ids(file_id, INT(obj_type, C_INT), max_objs, obj_ids)

    hdferr = 0
    IF(c_num_objs.LT.0) hdferr = -1

    IF (PRESENT(num_objs)) num_objs= c_num_objs

  END SUBROUTINE h5fget_obj_ids_f
!>
!! \ingroup FH5F
!!
!! \brief Get amount of free space within a file.
!!
!! \param file_id    File identifier
!! \param free_space Amount of free space in file
!! \param hdferr     \fortran_error
!!
!! See C API: @ref H5Fget_freespace()
!!
  SUBROUTINE h5fget_freespace_f(file_id, free_space, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: file_id
    INTEGER(HSSIZE_T), INTENT(OUT) :: free_space
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5fget_freespace_c(file_id, free_space) &
            BIND(C,NAME='h5fget_freespace_c')
         IMPORT :: HID_T, HSSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: file_id
         INTEGER(HSSIZE_T), INTENT(OUT) :: free_space
       END FUNCTION h5fget_freespace_c
    END INTERFACE

    hdferr = h5fget_freespace_c(file_id, free_space)

  END SUBROUTINE h5fget_freespace_f
!>
!! \ingroup FH5F
!!
!! \brief Gets the name of the file from the object identifier.
!!
!! \param obj_id Object identifier
!! \param buf    Buffer to store the read name
!! \param size   Actual size of the name
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Fget_name()
!!
  SUBROUTINE h5fget_name_f(obj_id, buf, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    CHARACTER(LEN=*), INTENT(OUT) :: buf
    INTEGER(SIZE_T), INTENT(OUT) :: size
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(SIZE_T) :: buflen

    INTERFACE
       INTEGER FUNCTION h5fget_name_c(obj_id, size, buf, buflen) &
            BIND(C,NAME='h5fget_name_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: obj_id
         INTEGER(SIZE_T), INTENT(OUT) :: size
         INTEGER(SIZE_T) :: buflen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: buf
       END FUNCTION h5fget_name_c
    END INTERFACE
    buflen = LEN(buf)
    hdferr = h5fget_name_c(obj_id, size, buf, buflen)
  END SUBROUTINE h5fget_name_f
!>
!! \ingroup FH5F
!!
!! \brief Retrieves the file size of the HDF5 file.
!!
!! \param file_id File identifier
!! \param size    File size
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Fget_filesize()
!!
  SUBROUTINE h5fget_filesize_f(file_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: file_id
    INTEGER(HSIZE_T), INTENT(OUT) :: size
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5fget_filesize_c(file_id, size) &
            BIND(C,NAME='h5fget_filesize_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: file_id
         INTEGER(HSIZE_T), INTENT(OUT) :: size
       END FUNCTION h5fget_filesize_c
    END INTERFACE
    hdferr = h5fget_filesize_c(file_id, size)
  END SUBROUTINE h5fget_filesize_f

!>
!! \ingroup FH5F
!!
!! \brief Retrieves the file number of the HDF5 file.
!!
!! \param file_id File identifier
!! \param fileno  File number
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Fget_fileno()
!!
  SUBROUTINE h5fget_fileno_f(file_id, fileno, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: file_id
    INTEGER, INTENT(OUT) :: fileno
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5fget_fileno_c(file_id, fileno) &
            BIND(C,NAME='h5fget_fileno_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: file_id
         INTEGER, INTENT(OUT) :: fileno
       END FUNCTION h5fget_fileno_c
    END INTERFACE
    hdferr = h5fget_fileno_c(file_id, fileno)
  END SUBROUTINE h5fget_fileno_f

!>
!! \ingroup FH5F
!!
!! \brief Retrieves a copy of the image of an existing, open file.
!!
!! \param file_id  Target file identifier
!! \param buf_ptr  Pointer to the buffer into which the image of the HDF5 file is to be copied
!! \param buf_len  Size of the supplied buffer
!! \param hdferr   \fortran_error
!! \param buf_size Returns the size in bytes of the buffer required to store the file image, no data will be copied
!!
!! See C API: @ref H5Fget_file_image()
!!
  SUBROUTINE h5fget_file_image_f(file_id, buf_ptr, buf_len, hdferr, buf_size)
    IMPLICIT NONE
    INTEGER(HID_T) , INTENT(IN)              :: file_id
    TYPE(C_PTR)    , INTENT(INOUT)           :: buf_ptr
    INTEGER(SIZE_T), INTENT(IN)              :: buf_len
    INTEGER        , INTENT(OUT)             :: hdferr
    INTEGER(SIZE_T), INTENT(OUT)  , OPTIONAL :: buf_size

    INTEGER(SIZE_T) :: buf_size_default

    INTERFACE
       INTEGER FUNCTION h5fget_file_image_c(file_id, buf_ptr, buf_len, buf_size) BIND(C, NAME='h5fget_file_image_c')
         IMPORT :: C_PTR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T) , INTENT(IN) :: file_id
         TYPE(C_PTR)    , VALUE      :: buf_ptr
         INTEGER(SIZE_T), INTENT(IN) :: buf_len
         INTEGER(SIZE_T), INTENT(OUT) :: buf_size
       END FUNCTION h5fget_file_image_c
    END INTERFACE

    IF(PRESENT(buf_size))THEN
       buf_ptr = C_NULL_PTR
    ENDIF

    hdferr = h5fget_file_image_c(file_id, buf_ptr, buf_len, buf_size_default)

    IF(PRESENT(buf_size))THEN
       buf_size = buf_size_default
    ENDIF

  END SUBROUTINE h5fget_file_image_f

!>
!! \ingroup FH5F
!!
!! \brief Gets the value of the "minimize dataset headers" value which creates
!!        smaller dataset object headers when its set and no attributes are present.
!!
!! \param file_id  Target file identifier
!! \param minimize Value of the setting
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Fget_dset_no_attrs_hint()
!!
  SUBROUTINE h5fget_dset_no_attrs_hint_f(file_id, minimize, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T) , INTENT(IN)              :: file_id
    LOGICAL        , INTENT(OUT)             :: minimize
    INTEGER        , INTENT(OUT)             :: hdferr
    LOGICAL(C_BOOL) :: c_minimize

    INTERFACE
       INTEGER FUNCTION h5fget_dset_no_attrs_hint_c(file_id, minimize) BIND(C, NAME='H5Fget_dset_no_attrs_hint')
         IMPORT :: HID_T, C_BOOL
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: file_id
         LOGICAL(C_BOOL), INTENT(OUT) :: minimize
       END FUNCTION h5fget_dset_no_attrs_hint_c
    END INTERFACE

    hdferr = INT(h5fget_dset_no_attrs_hint_c(file_id, c_minimize))

    ! Transfer value of C C_BOOL type to Fortran LOGICAL
    minimize = c_minimize

  END SUBROUTINE h5fget_dset_no_attrs_hint_f

!>
!! \ingroup FH5F
!!
!! \brief Sets the value of the "minimize dataset headers" value which creates
!!        smaller dataset object headers when its set and no attributes are present.
!!
!! \param file_id  Target file identifier
!! \param minimize Value of the setting
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Fset_dset_no_attrs_hint()
!!
  SUBROUTINE h5fset_dset_no_attrs_hint_f(file_id, minimize, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T) , INTENT(IN)              :: file_id
    LOGICAL        , INTENT(IN)              :: minimize
    INTEGER        , INTENT(OUT)             :: hdferr
    LOGICAL(C_BOOL) :: c_minimize

    INTERFACE
       INTEGER FUNCTION h5fset_dset_no_attrs_hint_c(file_id, minimize) BIND(C, NAME='H5Fset_dset_no_attrs_hint')
         IMPORT :: HID_T, C_BOOL
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: file_id
         LOGICAL(C_BOOL), INTENT(IN), VALUE :: minimize
       END FUNCTION h5fset_dset_no_attrs_hint_c
    END INTERFACE

    ! Transfer value of Fortran LOGICAL to C C_BOOL type
    c_minimize = minimize

    hdferr = INT(h5fset_dset_no_attrs_hint_c(file_id, c_minimize))

  END SUBROUTINE h5fset_dset_no_attrs_hint_f

!>
!! \ingroup FH5F
!!
!! \brief Retrieves global file information
!!
!! \param obj_id    Object identifier. The identifier may be that of a file, group, dataset, named datatype, or attribute.
!! \param file_info Buffer for global file information
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Fget_info2()
!!
  SUBROUTINE H5Fget_info_f(obj_id, file_info, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)          :: obj_id
    TYPE(H5F_INFO_T), INTENT(OUT), TARGET :: file_info
    INTEGER         , INTENT(OUT)         :: hdferr

    TYPE(C_PTR) :: f_ptr

    INTERFACE
       INTEGER(C_INT) FUNCTION  H5Fget_info(obj_id, file_info) BIND(C, NAME='H5Fget_info2')
         IMPORT :: HID_T, C_PTR, C_INT, H5F_INFO_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: obj_id
         TYPE(C_PTR),   VALUE  :: file_info
       END FUNCTION H5Fget_info
    END INTERFACE

    f_ptr = C_LOC(file_info)

    hdferr = INT(H5Fget_info(obj_id, f_ptr))

  END SUBROUTINE H5Fget_info_f

!>
!! \ingroup FH5F
!!
!! \brief Determines the read/write or read-only status of a file.
!!
!! \param file_id File identifier
!! \param intent  Access mode flag as originally passed with H5Fopen_f()
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Fget_intent()
!!
  SUBROUTINE h5fget_intent_f(file_id, intent, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: file_id
    INTEGER, INTENT(OUT) :: intent
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER(C_INT) :: c_intent

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Fget_intent(file_id, intent) BIND(C,NAME='H5Fget_intent')
         IMPORT :: C_INT
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: file_id
         INTEGER(C_INT) :: intent
       END FUNCTION H5Fget_intent
    END INTERFACE

    hdferr = INT(H5Fget_intent(file_id, c_intent))
    intent = INT(c_intent)

  END SUBROUTINE h5fget_intent_f

END MODULE H5F

