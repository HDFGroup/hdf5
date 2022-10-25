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
!   Copyright by the Board of Trustees of the University of Illinois.         *
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
  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR, C_CHAR, C_NULL_PTR
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
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: creation_prp
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: access_prp
    INTEGER(HID_T) :: creation_prp_default
    INTEGER(HID_T) :: access_prp_default
    INTEGER :: namelen ! Length of the name character string

    INTERFACE
       INTEGER FUNCTION h5fcreate_c(name, namelen, access_flags, &
            creation_prp_default, access_prp_default, file_id) BIND(C,NAME='h5fcreate_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER, INTENT(IN) :: access_flags
         INTEGER(HID_T), INTENT(OUT) :: file_id
         INTEGER(HID_T), INTENT(IN) :: creation_prp_default
         INTEGER(HID_T), INTENT(IN) :: access_prp_default
         INTEGER :: namelen
       END FUNCTION h5fcreate_c
    END INTERFACE

    creation_prp_default = H5P_DEFAULT_F
    access_prp_default = H5P_DEFAULT_F

    IF (PRESENT(creation_prp)) creation_prp_default = creation_prp
    IF (PRESENT(access_prp))   access_prp_default   = access_prp
    namelen = LEN_TRIM(name)
    hdferr = h5fcreate_c(name, namelen, access_flags, &
         creation_prp_default, access_prp_default, file_id)

  END SUBROUTINE h5fcreate_f
!>
!! \ingroup FH5F
!!
!! \brief Flushes all buffers associated with a file to disk
!!
!! \param object_id    Identifier of object used to identify the file.
!! \param scope        Specifies the scope of the flushing action. Possible values are:
!!                     \li H5F_SCOPE_GLOBAL_F
!!                     \li H5F_SCOPE_LOCAL_F
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5Fflush()
!!
  SUBROUTINE h5fflush_f(object_id, scope, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: object_id
    INTEGER, INTENT(IN) :: scope
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5fflush_c(object_id, scope) BIND(C,NAME='h5fflush_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: object_id
         INTEGER, INTENT(IN) :: scope
       END FUNCTION h5fflush_c
    END INTERFACE

    hdferr = h5fflush_c(object_id, scope)

  END SUBROUTINE h5fflush_f
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
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: access_prp
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
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: access_prp
    INTEGER(HID_T) :: access_prp_default
    INTEGER :: namelen ! Length of the name character string

    INTERFACE
       INTEGER FUNCTION h5fopen_c(name, namelen, access_flags, &
            access_prp_default, file_id) BIND(C,NAME='h5fopen_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER :: namelen
         INTEGER, INTENT(IN) :: access_flags
         INTEGER(HID_T), INTENT(IN) :: access_prp_default
         INTEGER(HID_T), INTENT(OUT) :: file_id
       END FUNCTION h5fopen_c
    END INTERFACE

    access_prp_default = H5P_DEFAULT_F
    IF (PRESENT(access_prp))   access_prp_default   = access_prp
    namelen = LEN_TRIM(name)
    hdferr = h5fopen_c(name, namelen, access_flags, &
                               access_prp_default, file_id)
  END SUBROUTINE h5fopen_f
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
       INTEGER FUNCTION h5freopen_c(file_id, ret_file_id) BIND(C,NAME='h5freopen_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: file_id
         INTEGER(HID_T), INTENT(OUT) :: ret_file_id
       END FUNCTION h5freopen_c
    END INTERFACE

    hdferr = h5freopen_c(file_id, ret_file_id)

  END SUBROUTINE h5freopen_f
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
    INTEGER(HID_T), INTENT(IN) :: file_id
    INTEGER(HID_T), INTENT(OUT) :: prop_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5fget_create_plist_c(file_id, prop_id) BIND(C,NAME='h5fget_create_plist_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: file_id
         INTEGER(HID_T), INTENT(OUT) :: prop_id
       END FUNCTION h5fget_create_plist_c
    END INTERFACE

    hdferr = h5fget_create_plist_c(file_id, prop_id)

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
    INTEGER(HID_T), INTENT(IN) :: file_id
    INTEGER(HID_T), INTENT(OUT) :: access_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5fget_access_plist_c(file_id, access_id) BIND(C,NAME='h5fget_access_plist_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: file_id
         INTEGER(HID_T), INTENT(OUT) :: access_id
       END FUNCTION h5fget_access_plist_c
    END INTERFACE

    hdferr = h5fget_access_plist_c(file_id, access_id)

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
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: access_prp

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
       INTEGER FUNCTION h5fclose_c(file_id) BIND(C,NAME='h5fclose_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: file_id
       END FUNCTION h5fclose_c
    END INTERFACE

    hdferr = h5fclose_c(file_id)

  END SUBROUTINE h5fclose_f

!>
!! \ingroup FH5F
!!
!! \brief Gets number of the objects open within a file
!!
!! \param file_id   File identifier.
!! \param obj_type  Type of the object; possible values are:
!!                  \li H5F_OBJ_FILE_F
!!                  \li H5F_OBJ_DATASET_F
!!                  \li H5F_OBJ_GROUP_F
!!                  \li H5F_OBJ_DATATYPE_F
!!                  \li H5F_OBJ_ALL_F
!! \param obj_count Number of open objects.
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Fget_obj_count()
!!
  SUBROUTINE h5fget_obj_count_f(file_id, obj_type, obj_count, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: file_id
    INTEGER, INTENT(IN)  :: obj_type
    INTEGER(SIZE_T), INTENT(OUT) :: obj_count
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5fget_obj_count_c(file_id, obj_type, obj_count) BIND(C,NAME='h5fget_obj_count_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: file_id
         INTEGER, INTENT(IN)  :: obj_type
         INTEGER(SIZE_T), INTENT(OUT) :: obj_count
       END FUNCTION h5fget_obj_count_c
    END INTERFACE

    hdferr = h5fget_obj_count_c(file_id, obj_type, obj_count)

    ! Don't include objects created by H5open in the H5F_OBJ_ALL_F count
    IF(file_id.EQ.INT(H5F_OBJ_ALL_F,HID_T))THEN
       obj_count = obj_count - H5OPEN_NUM_OBJ
    ENDIF

  END SUBROUTINE h5fget_obj_count_f

!>
!! \ingroup FH5F
!!
!! \brief Get list of open objects identifiers within a file
!!
!! \param file_id  File identifier.
!! \param obj_type Type of the object; possible values are:
!!                 \li H5F_OBJ_FILE_F
!!                 \li H5F_OBJ_DATASET_F
!!                 \li H5F_OBJ_GROUP_F
!!                 \li H5F_OBJ_DATATYPE_F
!!                 \li H5F_OBJ_ALL_F
!! \param max_objs Maximum # of objects to retrieve.
!! \param obj_ids  Array of open object identifiers.
!! \param hdferr   \fortran_error
!! \param num_objs Number of open objects.
!!
!! See C API: @ref H5Fget_obj_ids()
!!
  SUBROUTINE h5fget_obj_ids_f(file_id, obj_type, max_objs, obj_ids, hdferr, num_objs)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: file_id
    INTEGER, INTENT(IN)  :: obj_type
    INTEGER(SIZE_T), INTENT(IN)  :: max_objs
    INTEGER(HID_T), DIMENSION(*), INTENT(INOUT) :: obj_ids
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(SIZE_T), INTENT(OUT), OPTIONAL :: num_objs

    INTEGER(SIZE_T) :: c_num_objs ! Number of open objects of the specified type

    INTERFACE
       INTEGER FUNCTION h5fget_obj_ids_c(file_id, obj_type, max_objs, obj_ids, c_num_objs) &
            BIND(C,NAME='h5fget_obj_ids_c')
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: file_id
         INTEGER, INTENT(IN)  :: obj_type
         INTEGER(SIZE_T), INTENT(IN)  :: max_objs
         INTEGER(HID_T), DIMENSION(*), INTENT(INOUT) :: obj_ids
         INTEGER(SIZE_T), INTENT(OUT) :: c_num_objs
       END FUNCTION h5fget_obj_ids_c
    END INTERFACE

    hdferr = h5fget_obj_ids_c(file_id, obj_type, max_objs, obj_ids, c_num_objs)
    IF (PRESENT(num_objs)) num_objs= c_num_objs

  END SUBROUTINE h5fget_obj_ids_f
!>
!! \ingroup FH5F
!!
!! \brief Get amount of free space within a file.
!!
!! \param file_id    File identifier.
!! \param free_space Amount of free space in file.
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
!! \param obj_id Object identifier.
!! \param buf    Buffer to store the read name.
!! \param size   Actual size of the name.
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
!! \param file_id File identifier.
!! \param size    File size.
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
!! \param file_id File identifier.
!! \param fileno  File number.
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
!! \param file_id  Target file identifier.
!! \param buf_ptr  Pointer to the buffer into which the image of the HDF5 file is to be copied.
!! \param buf_len  Size of the supplied buffer.
!! \param hdferr   \fortran_error
!! \param buf_size Returns the size in bytes of the buffer required to store the file image, no data will be copied.
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
!! \param file_id  Target file identifier.
!! \param minimize Value of the setting.
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
!! \param file_id  Target file identifier.
!! \param minimize Value of the setting.
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

END MODULE H5F

