!> @defgroup FH5L Fortran Link (H5L) Interface
!!
!! @see H5L, C-API
!!
!! @see @ref H5L_UG, User Guide
!!

!> @ingroup FH5L
!!
!! @brief This module contains Fortran interfaces for H5L functions.
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
!  If you add a new H5L function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!

MODULE H5L

  USE H5GLOBAL
  IMPLICIT NONE

  TYPE, bind(c) :: union_t
     TYPE(H5O_TOKEN_T_F) :: token !< Type for object tokens
     INTEGER(size_t)  :: val_size !< Size of a soft link or user-defined link value
  END TYPE union_t

!
! @brief Fortran2003 Derived Type for @ref H5L_info_t
!
  TYPE, bind(c) :: h5l_info_t
     INTEGER(c_int) :: type !< Specifies the link class. Valid values include the following:
                            !< \li H5L_TYPE_HARD_F     Hard link
                            !< \li H5L_TYPE_SOFT_F     Soft link
                            !< \li H5L_TYPE_EXTERNAL_F External link
                            !< \li H5L_TYPE_ERROR_F    Invalid link type id
 !   LOGICAL(c_bool) :: corder_valid ! bool corder_valid
     INTEGER(c_int64_t) :: corder !< Creation order
     INTEGER(c_int)     :: cset   !< Character set of link name is encoded. Valid values include the following:
                                  !< \li H5T_CSET_ASCII  US ASCII
                                  !< \li H5T_CSET_UTF8   UTF-8 Unicode encoding
     TYPE(union_t) :: u
  END TYPE h5l_info_t

CONTAINS

!>
!! \ingroup FH5L
!!
!! \brief Copies a link from one location to another.
!!
!! \param src_loc_id  Location identifier. The identifier may be that of a file, group, dataset, or named datatype.
!! \param src_name    Name of the link to be copied.
!! \param dest_loc_id Location identifier. The identifier may be that of a file, group, dataset, or named datatype.
!! \param dest_name   Name to be assigned to the new copy.
!! \param hdferr      \fortran_error
!! \param lcpl_id     Link creation property list identifier.
!! \param lapl_id     Link access property list identifier.
!!
!! See C API: @ref H5Lcopy()
!!
  SUBROUTINE h5lcopy_f(src_loc_id, src_name, dest_loc_id, dest_name, hdferr, &
       lcpl_id, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: src_loc_id
    CHARACTER(LEN=*), INTENT(IN) :: src_name
    INTEGER(HID_T), INTENT(IN) :: dest_loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dest_name

    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lcpl_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id
    INTEGER(HID_T) :: lcpl_id_default
    INTEGER(HID_T) :: lapl_id_default

    INTEGER(SIZE_T) :: src_namelen
    INTEGER(SIZE_T) :: dest_namelen

    INTERFACE
       INTEGER FUNCTION h5lcopy_c(src_loc_id, src_name, src_namelen, dest_loc_id, dest_name, dest_namelen, &
            lcpl_id_default, lapl_id_default) BIND(C,name='h5lcopy_c')
         IMPORT ::  c_char
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: src_loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: src_name
         INTEGER(HID_T), INTENT(IN) :: dest_loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: dest_name

         INTEGER(HID_T) :: lcpl_id_default
         INTEGER(HID_T) :: lapl_id_default

         INTEGER(SIZE_T) :: src_namelen
         INTEGER(SIZE_T) :: dest_namelen
       END FUNCTION h5lcopy_c
    END INTERFACE

    src_namelen  = LEN(src_name)
    dest_namelen = LEN(dest_name)

    lcpl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lcpl_id)) lcpl_id_default = lcpl_id
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5lcopy_c(src_loc_id, src_name, src_namelen, dest_loc_id, dest_name, dest_namelen, &
         lcpl_id_default, lapl_id_default)

  END SUBROUTINE h5lcopy_f

!>
!! \ingroup FH5L
!!
!! \brief Removes a link from a group.
!!
!! \param loc_id  Identifier of the file or group containing the object.
!! \param name    Name of the link to delete.
!! \param hdferr  \fortran_error
!! \param lapl_id Link access property list identifier.
!!
!! See C API: @ref H5Ldelete()
!!
  SUBROUTINE h5ldelete_f(loc_id, name, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id

    INTEGER(HID_T) :: lapl_id_default
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Ldelete(loc_id, name, lapl_id_default) BIND(C,name='H5Ldelete')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         INTEGER(HID_T), VALUE :: lapl_id_default
       END FUNCTION h5ldelete
    END INTERFACE

    c_name  = TRIM(name)//C_NULL_CHAR

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = INT(H5Ldelete(loc_id, c_name, lapl_id_default))

  END SUBROUTINE h5ldelete_f

!>
!! \ingroup FH5L
!!
!! \brief Asynchronously removes a link from a group.
!!
!! \param loc_id  Identifier of the file or group containing the object.
!! \param name    Name of the link to delete.
!! \param es_id   \fortran_es_id
!! \param hdferr  \fortran_error
!! \param lapl_id Link access property list identifier.
!! \param file    \fortran_file
!! \param func    \fortran_func
!! \param line    \fortran_line
!!
!! See C API: @ref H5Ldelete_async()
!!
  SUBROUTINE h5ldelete_async_f(loc_id, name, es_id, hdferr, lapl_id, file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(HID_T), INTENT(IN)  :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , OPTIONAL, INTENT(IN) :: line

    INTEGER(HID_T) :: lapl_id_default
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name
    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Ldelete_async(file, func, line, loc_id, name, lapl_id_default, es_id) &
            BIND(C,name='H5Ldelete_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         INTEGER(HID_T), VALUE :: lapl_id_default
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Ldelete_async
    END INTERFACE

    c_name  = TRIM(name)//C_NULL_CHAR

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id
    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    hdferr = INT(H5Ldelete_async(file_default, func_default, line_default, &
         loc_id, c_name, lapl_id_default, es_id))

  END SUBROUTINE h5ldelete_async_f

!>
!! \ingroup FH5L
!!
!! \brief Creates a soft link to an object.
!!
!! \param target_path Path to the target object, which is not required to exist.
!! \param link_loc_id The file or group identifier for the new link.
!! \param link_name   The name of the new link.
!! \param hdferr      \fortran_error
!! \param lcpl_id     Link creation property list identifier.
!! \param lapl_id     Link access property list identifier.
!!
!! See C API: @ref H5Lcreate_soft()
!!
  SUBROUTINE h5lcreate_soft_f(target_path, link_loc_id, link_name, hdferr, lcpl_id, lapl_id)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: target_path
    INTEGER(HID_T), INTENT(IN) :: link_loc_id
    CHARACTER(LEN=*), INTENT(IN) :: link_name
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lcpl_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id

    INTEGER(HID_T) :: lcpl_id_default
    INTEGER(HID_T) :: lapl_id_default
    CHARACTER(LEN=LEN_TRIM(target_path) +1,KIND=C_CHAR) :: c_target_path
    CHARACTER(LEN=LEN_TRIM(link_name)+1,KIND=C_CHAR) :: c_link_name

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Lcreate_soft(target_path, link_loc_id, link_name, &
            lcpl_id_default, lapl_id_default ) BIND(C,NAME='H5Lcreate_soft')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: target_path
         INTEGER(HID_T), VALUE ::   link_loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: link_name
         INTEGER(HID_T), VALUE :: lcpl_id_default
         INTEGER(HID_T), VALUE :: lapl_id_default
       END FUNCTION H5Lcreate_soft
    END INTERFACE

    c_target_path  = TRIM(target_path)//C_NULL_CHAR
    c_link_name = TRIM(link_name)//C_NULL_CHAR

    lcpl_id_default = H5P_DEFAULT_F
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lcpl_id)) lcpl_id_default = lcpl_id
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = INT(H5Lcreate_soft(c_target_path, link_loc_id, c_link_name, &
         lcpl_id_default, lapl_id_default))

  END SUBROUTINE h5lcreate_soft_f

!>
!! \ingroup FH5L
!!
!! \brief Asynchronously creates a soft link to an object.
!!
!! \param target_path Path to the target object, which is not required to exist.
!! \param link_loc_id The file or group identifier for the new link.
!! \param link_name   The name of the new link.
!! \param es_id       \fortran_es_id
!! \param hdferr      \fortran_error
!! \param lcpl_id     Link creation property list identifier.
!! \param lapl_id     Link access property list identifier.
!! \param file        \fortran_file
!! \param func        \fortran_func
!! \param line        \fortran_line
!!
!! See C API: @ref H5Lcreate_soft_async()
!!
  SUBROUTINE h5lcreate_soft_async_f(target_path, link_loc_id, link_name, es_id, hdferr,&
       lcpl_id, lapl_id, file, func, line)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: target_path
    INTEGER(HID_T), INTENT(IN) :: link_loc_id
    CHARACTER(LEN=*), INTENT(IN) :: link_name
    INTEGER(HID_T), INTENT(IN)    :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lcpl_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    INTEGER(HID_T) :: lcpl_id_default
    INTEGER(HID_T) :: lapl_id_default
    CHARACTER(LEN=LEN_TRIM(target_path) +1,KIND=C_CHAR) :: c_target_path
    CHARACTER(LEN=LEN_TRIM(link_name)+1,KIND=C_CHAR) :: c_link_name
    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Lcreate_soft_async(file, func, line, target_path, link_loc_id, link_name, &
            lcpl_id_default, lapl_id_default, es_id) BIND(C,NAME='H5Lcreate_soft_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: target_path
         INTEGER(HID_T), VALUE ::   link_loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: link_name
         INTEGER(HID_T), VALUE :: lcpl_id_default
         INTEGER(HID_T), VALUE :: lapl_id_default
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Lcreate_soft_async
    END INTERFACE

    c_target_path  = TRIM(target_path)//C_NULL_CHAR
    c_link_name = TRIM(link_name)//C_NULL_CHAR

    lcpl_id_default = H5P_DEFAULT_F
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lcpl_id)) lcpl_id_default = lcpl_id
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    hdferr = INT(H5Lcreate_soft_async(file_default, func_default, line_default,&
         c_target_path, link_loc_id, c_link_name, lcpl_id_default, lapl_id_default, es_id))

  END SUBROUTINE h5lcreate_soft_async_f
!>
!! \ingroup FH5L
!!
!! \brief Creates a hard link to an object.
!!
!! \param obj_loc_id  The file or group identifier for the target object.
!! \param obj_name    Name of the target object, which must already exist.
!! \param link_loc_id The file or group identifier for the new link.
!! \param link_name   The name of the new link.
!! \param hdferr      \fortran_error
!! \param lcpl_id     Link creation property list identifier.
!! \param lapl_id     Link access property list identifier.
!!
!! See C API: @ref H5Lcreate_hard()
!!
  SUBROUTINE h5lcreate_hard_f(obj_loc_id, obj_name, link_loc_id, link_name, hdferr, &
       lcpl_id, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_loc_id
    CHARACTER(LEN=*), INTENT(IN) :: obj_name
    INTEGER(HID_T), INTENT(IN) :: link_loc_id
    CHARACTER(LEN=*), INTENT(IN) :: link_name
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lcpl_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id

    INTEGER(HID_T) :: lcpl_id_default
    INTEGER(HID_T) :: lapl_id_default
    CHARACTER(LEN=LEN_TRIM(obj_name) +1,KIND=C_CHAR) :: c_obj_name
    CHARACTER(LEN=LEN_TRIM(link_name)+1,KIND=C_CHAR) :: c_link_name
    INTERFACE
       INTEGER(C_INT) FUNCTION H5Lcreate_hard(obj_loc_id, obj_name, &
            link_loc_id, link_name, lcpl_id_default, lapl_id_default) BIND(C,NAME='H5Lcreate_hard')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: obj_loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: obj_name
         INTEGER(HID_T), VALUE :: link_loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: link_name
         INTEGER(HID_T), VALUE :: lcpl_id_default
         INTEGER(HID_T), VALUE :: lapl_id_default
       END FUNCTION H5Lcreate_hard
    END INTERFACE

    c_obj_name  = TRIM(obj_name)//C_NULL_CHAR
    c_link_name = TRIM(link_name)//C_NULL_CHAR

    lcpl_id_default = H5P_DEFAULT_F
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lcpl_id)) lcpl_id_default = lcpl_id
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = INT(H5Lcreate_hard(obj_loc_id, c_obj_name, &
         link_loc_id, c_link_name, lcpl_id_default, lapl_id_default))

  END SUBROUTINE h5lcreate_hard_f
!>
!! \ingroup FH5L
!!
!! \brief Asynchronously creates a hard link to an object.
!!
!! \param obj_loc_id  The file or group identifier for the target object.
!! \param obj_name    Name of the target object, which must already exist.
!! \param link_loc_id The file or group identifier for the new link.
!! \param link_name   The name of the new link.
!! \param es_id       \fortran_es_id
!! \param hdferr      \fortran_error
!! \param lcpl_id     Link creation property list identifier.
!! \param lapl_id     Link access property list identifier.
!! \param file        \fortran_file
!! \param func        \fortran_func
!! \param line        \fortran_line
!!
!! See C API: @ref H5Lcreate_hard_async()
!!
  SUBROUTINE h5lcreate_hard_async_f(obj_loc_id, obj_name, link_loc_id, link_name, es_id, hdferr, &
       lcpl_id, lapl_id, file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_loc_id
    CHARACTER(LEN=*), INTENT(IN) :: obj_name
    INTEGER(HID_T), INTENT(IN) :: link_loc_id
    CHARACTER(LEN=*), INTENT(IN) :: link_name
    INTEGER(HID_T), INTENT(IN) :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lcpl_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    INTEGER(HID_T) :: lcpl_id_default
    INTEGER(HID_T) :: lapl_id_default
    CHARACTER(LEN=LEN_TRIM(obj_name) +1,KIND=C_CHAR) :: c_obj_name
    CHARACTER(LEN=LEN_TRIM(link_name)+1,KIND=C_CHAR) :: c_link_name
    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Lcreate_hard_async(file, func, line, obj_loc_id, obj_name, &
            link_loc_id, link_name, lcpl_id_default, lapl_id_default, es_id) BIND(C,NAME='H5Lcreate_hard_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: obj_loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: obj_name
         INTEGER(HID_T), VALUE :: link_loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: link_name
         INTEGER(HID_T), VALUE :: lcpl_id_default
         INTEGER(HID_T), VALUE :: lapl_id_default
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Lcreate_hard_async
    END INTERFACE

    c_obj_name  = TRIM(obj_name)//C_NULL_CHAR
    c_link_name = TRIM(link_name)//C_NULL_CHAR

    lcpl_id_default = H5P_DEFAULT_F
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lcpl_id)) lcpl_id_default = lcpl_id
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id
    IF (PRESENT(file)) file_default = file
    IF (PRESENT(func)) func_default = func
    IF (PRESENT(line)) line_default = INT(line, C_INT)

    hdferr = INT(H5Lcreate_hard_async(file_default, func_default, line_default, obj_loc_id, c_obj_name, &
         link_loc_id, c_link_name, lcpl_id_default, lapl_id_default, es_id))

  END SUBROUTINE h5lcreate_hard_async_f

!>
!! \ingroup FH5L
!!
!! \brief Creates a soft link to an object in a different file.
!!
!! \param file_name   Name of the file containing the target object. Neither the file nor the target object is
!!                    required to exist. May be the file the link is being created in.
!! \param obj_name    Path within the target file to the target object.
!! \param link_loc_id The file or group identifier for the new link.
!! \param link_name   The name of the new link.
!! \param hdferr      \fortran_error
!! \param lcpl_id     Link creation property list identifier.
!! \param lapl_id     Link access property list identifier.
!!
!! See C API: @ref H5Lcreate_external()
!!
  SUBROUTINE h5lcreate_external_f(file_name, obj_name, link_loc_id, link_name, hdferr, lcpl_id, lapl_id)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: file_name
    CHARACTER(LEN=*), INTENT(IN) :: obj_name
    INTEGER(HID_T), INTENT(IN) :: link_loc_id
    CHARACTER(LEN=*), INTENT(IN) :: link_name

    INTEGER, INTENT(OUT) :: hdferr

    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lcpl_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id

    INTEGER(HID_T) :: lcpl_id_default
    INTEGER(HID_T) :: lapl_id_default

    INTEGER(SIZE_T) :: file_namelen
    INTEGER(SIZE_T) :: obj_namelen
    INTEGER(SIZE_T) :: link_namelen

    INTERFACE
       INTEGER FUNCTION h5lcreate_external_c(file_name, file_namelen, obj_name, obj_namelen, &
            link_loc_id, link_name, link_namelen, lcpl_id_default, lapl_id_default) BIND(C,NAME='h5lcreate_external_c')
         IMPORT :: c_char
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: file_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: obj_name
         INTEGER(HID_T), INTENT(IN) :: link_loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: link_name
         INTEGER(SIZE_T) :: file_namelen
         INTEGER(SIZE_T) :: obj_namelen
         INTEGER(SIZE_T) :: link_namelen
         INTEGER(HID_T) :: lcpl_id_default
         INTEGER(HID_T) :: lapl_id_default
       END FUNCTION h5lcreate_external_c
    END INTERFACE
    file_namelen = LEN(file_name)
    obj_namelen = LEN(obj_name)
    link_namelen = LEN(link_name)

    lcpl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lcpl_id)) lcpl_id_default = lcpl_id
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5lcreate_external_c(file_name, file_namelen, obj_name, obj_namelen, &
            link_loc_id, link_name, link_namelen, lcpl_id_default, lapl_id_default)

  END SUBROUTINE h5lcreate_external_f

!>
!! \ingroup FH5L
!!
!! \brief Removes the nth link in a group.
!!
!! \param loc_id      File or group identifier specifying location of subject group.
!! \param group_name  Name of subject group.
!! \param index_field Type of index; Possible values are:
!!                    \li H5_INDEX_UNKNOWN_F = -1 - Unknown index type
!!                    \li H5_INDEX_NAME_F         - Index on names
!!                    \li H5_INDEX_CRT_ORDER_F    - Index on creation order
!!                    \li H5_INDEX_N_F            - Number of indices defined
!! \param order       Order within field or index; Possible values are:
!!                    \li H5_ITER_UNKNOWN_F - Unknown order
!!                    \li H5_ITER_INC_F     - Increasing order
!!                    \li H5_ITER_DEC_F     - Decreasing order
!!                    \li H5_ITER_NATIVE_F  - No particular order, whatever is fastest
!!                    \li H5_ITER_N_F       - Number of iteration orders
!! \param n           Link for which to retrieve information.
!! \param hdferr      \fortran_error
!! \param lapl_id     Link access property list.
!!
!! See C API: @ref H5Ldelete_by_idx()
!!
  SUBROUTINE h5ldelete_by_idx_f(loc_id, group_name, index_field, order, n, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: group_name
    INTEGER, INTENT(IN) :: index_field
    INTEGER, INTENT(IN) :: order
    INTEGER(HSIZE_T), INTENT(IN) :: n
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id

    INTEGER(HID_T) :: lapl_id_default
    CHARACTER(LEN=LEN_TRIM(group_name)+1,KIND=C_CHAR) :: c_group_name

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Ldelete_by_idx(loc_id, group_name, index_field, order, n, lapl_id_default) &
            BIND(C,NAME='H5Ldelete_by_idx')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: group_name
         INTEGER(C_INT), VALUE :: index_field
         INTEGER(C_INT), VALUE :: order
         INTEGER(HSIZE_T), VALUE :: n
         INTEGER(HID_T), VALUE :: lapl_id_default
       END FUNCTION H5Ldelete_by_idx
    END INTERFACE

    c_group_name  = TRIM(group_name)//C_NULL_CHAR

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = INT(H5Ldelete_by_idx(loc_id, c_group_name, INT(index_field,C_INT), INT(order, C_INT), n, lapl_id_default))

  END SUBROUTINE h5ldelete_by_idx_f

!>
!! \ingroup FH5L
!!
!! \brief Asynchronously removes the nth link in a group.
!!
!! \param loc_id      File or group identifier specifying location of subject group.
!! \param group_name  Name of subject group.
!! \param index_field Type of index; Possible values are:
!!                    \li H5_INDEX_UNKNOWN_F = -1 - Unknown index type
!!                    \li H5_INDEX_NAME_F         - Index on names
!!                    \li H5_INDEX_CRT_ORDER_F    - Index on creation order
!!                    \li H5_INDEX_N_F            - Number of indices defined
!! \param order       Order within field or index; Possible values are:
!!                    \li H5_ITER_UNKNOWN_F - Unknown order
!!                    \li H5_ITER_INC_F     - Increasing order
!!                    \li H5_ITER_DEC_F     - Decreasing order
!!                    \li H5_ITER_NATIVE_F  - No particular order, whatever is fastest
!!                    \li H5_ITER_N_F       - Number of iteration orders
!! \param n           Link for which to retrieve information.
!! \param es_id       \fortran_es_id
!! \param hdferr      \fortran_error
!! \param lapl_id     Link access property list.
!! \param file        \fortran_file
!! \param func        \fortran_func
!! \param line        \fortran_line
!!
!! See C API: @ref H5Ldelete_by_idx_async()
!!
  SUBROUTINE h5ldelete_by_idx_async_f(loc_id, group_name, index_field, order, n, es_id, hdferr, &
       lapl_id, file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: group_name
    INTEGER, INTENT(IN) :: index_field
    INTEGER, INTENT(IN) :: order
    INTEGER(HSIZE_T), INTENT(IN) :: n
    INTEGER(HID_T), INTENT(IN) :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    INTEGER(HID_T) :: lapl_id_default
    CHARACTER(LEN=LEN_TRIM(group_name)+1,KIND=C_CHAR) :: c_group_name
    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Ldelete_by_idx_async(file, func, line, loc_id, group_name, index_field, order, n, &
            lapl_id_default, es_id) BIND(C,NAME='H5Ldelete_by_idx_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: group_name
         INTEGER(C_INT), VALUE :: index_field
         INTEGER(C_INT), VALUE :: order
         INTEGER(HSIZE_T), VALUE :: n
         INTEGER(HID_T), VALUE :: lapl_id_default
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Ldelete_by_idx_async
    END INTERFACE

    c_group_name  = TRIM(group_name)//C_NULL_CHAR

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id
    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    hdferr = INT(H5Ldelete_by_idx_async(file_default, func_default, line_default, &
         loc_id, c_group_name, INT(index_field,C_INT), INT(order, C_INT), n, lapl_id_default, es_id))

  END SUBROUTINE h5ldelete_by_idx_async_f

!>
!! \ingroup FH5L
!!
!! \brief Check if a link with a particular name exists in a group.
!!
!! \param loc_id      Identifier of the file or group to query.
!! \param name        Link name to check.
!! \param link_exists Link exists status (.TRUE.,.FALSE.).
!! \param hdferr      \fortran_error
!! \param lapl_id     Link access property list identifier.
!!
!! See C API: @ref H5Lexists()
!!
  SUBROUTINE h5lexists_f(loc_id, name, link_exists, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    LOGICAL, INTENT(OUT) :: link_exists
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id

    INTEGER(C_INT) :: link_exists_c
    INTEGER(HID_T)  :: lapl_id_default
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Lexists(loc_id, name, lapl_id_default) BIND(C,NAME='H5Lexists')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         INTEGER(HID_T), VALUE :: lapl_id_default

       END FUNCTION H5Lexists
    END INTERFACE

    c_name = TRIM(name)//C_NULL_CHAR

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    link_exists_c = H5Lexists(loc_id, c_name, lapl_id_default)

    link_exists = .FALSE.
    IF(link_exists_c.GT.0_C_INT) link_exists = .TRUE.

    hdferr = 0
    IF(link_exists_c.LT.0_C_INT) hdferr = -1

    hdferr = 0
    IF(link_exists_c.LT.0) hdferr = -1

  END SUBROUTINE h5lexists_f

!>
!! \ingroup FH5L
!!
!! \brief Asynchronously checks if a link with a particular name exists in a group.
!!
!! \param loc_id      Identifier of the file or group to query.
!! \param name        Link name to check.
!! \param link_exists Pointer to link exists status. It should be declared INTEGER(C_INT) and initialized
!!                    to zero (false) for portability. It will return one when true. LOGICAL(C_BOOL) is also
!!                    acceptable but may encounter atypical anomalies. It should be initialized to false when used.
!! \param es_id       \fortran_es_id
!! \param hdferr      \fortran_error
!! \param lapl_id     Link access property list identifier.
!! \param file        \fortran_file
!! \param func        \fortran_func
!! \param line        \fortran_line
!!
!! See C API: @ref H5Lexists_async()
!!
  SUBROUTINE h5lexists_async_f(loc_id, name, link_exists, es_id, hdferr, lapl_id, file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    TYPE(C_PTR)     , INTENT(IN) :: link_exists
    INTEGER(HID_T), INTENT(IN)  :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    INTEGER(HID_T)  :: lapl_id_default
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name
    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Lexists_async(file, func, line, &
            loc_id, name, exists, lapl_id_default, es_id) BIND(C,NAME='H5Lexists_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         TYPE(C_PTR)   , VALUE :: exists
         INTEGER(HID_T), VALUE :: lapl_id_default
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Lexists_async
    END INTERFACE

    c_name = TRIM(name)//C_NULL_CHAR

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id
    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    hdferr = INT(H5Lexists_async(file_default, func_default, line_default, loc_id, c_name, &
         link_exists, lapl_id_default, es_id))

  END SUBROUTINE h5lexists_async_f

!>
!! \ingroup FH5L
!!
!! \brief Returns information about a link.
!!
!! \param link_loc_id File or group identifier.
!! \param link_name   Name of the link for which information is being sought.
!!                     NOTE: In C these are contained in the structure H5L_info_t
!! \param cset         Indicates the character set used for link&apos;s name.
!! \param corder       Specifies the link&apos;s creation order position.
!! \param f_corder_valid Indicates whether the value in corder is valid.
!! \param link_type    Specifies the link class:
!!                     \li H5L_TYPE_HARD_F     - Hard link
!!                     \li H5L_TYPE_SOFT_F     - Soft link
!!                     \li H5L_TYPE_EXTERNAL_F - External link
!!                     \li H5L_TYPE_ERROR_ F   - Error
!! \param token        If the link is a hard link, token specifies the object token that the link points to.
!! \param val_size     If the link is a symbolic link, val_size will be the length of the link value, e.g.,
!!                     the length of the name of the pointed-to object with a null terminator.
!! \param hdferr       \fortran_error
!! \param lapl_id      Link access property list.
!!
!! See C API: @ref H5Lget_info2()
!!
  SUBROUTINE h5lget_info_f(link_loc_id, link_name, &
       cset, corder, f_corder_valid, link_type, token, val_size, &
       hdferr, lapl_id)
    IMPLICIT NONE

    INTEGER(HID_T), INTENT(IN) :: link_loc_id
    CHARACTER(LEN=*), INTENT(IN) :: link_name

    INTEGER, INTENT(OUT) :: cset
    INTEGER, INTENT(OUT) :: corder
    LOGICAL, INTENT(OUT) :: f_corder_valid
    INTEGER, INTENT(OUT) :: link_type
    TYPE(H5O_TOKEN_T_F), INTENT(OUT), TARGET :: token
    INTEGER(SIZE_T), INTENT(OUT) :: val_size
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id
    INTEGER(SIZE_T) :: link_namelen
    INTEGER(HID_T) :: lapl_id_default
    INTEGER :: corder_valid

    INTERFACE
       INTEGER FUNCTION h5lget_info_c(link_loc_id, link_name, link_namelen, &
            cset, corder, corder_valid, link_type, token, val_size, &
            lapl_id_default) BIND(C,NAME='h5lget_info_c')
         IMPORT :: c_char
         IMPORT :: HID_T, SIZE_T, H5O_TOKEN_T_F
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: link_loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: link_name
         INTEGER, INTENT(OUT) :: cset
         INTEGER, INTENT(OUT) :: corder
         INTEGER, INTENT(OUT) :: link_type
         TYPE(H5O_TOKEN_T_F), INTENT(OUT) :: token
         INTEGER(SIZE_T), INTENT(OUT) :: val_size
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(SIZE_T) :: link_namelen
         INTEGER :: corder_valid
       END FUNCTION h5lget_info_c
    END INTERFACE

    link_namelen = LEN(link_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5lget_info_c(link_loc_id, link_name, link_namelen, cset, &
         corder, corder_valid, link_type, token, val_size, lapl_id_default)

    f_corder_valid =.FALSE.
    IF(corder_valid .EQ. 1) f_corder_valid =.TRUE.

  END SUBROUTINE h5lget_info_f

!>
!! \ingroup FH5L
!!
!! \brief Retrieves metadata for a link in a group, according to the order within a field or index.
!!
!! \param loc_id         File or group identifier specifying location of subject group.
!! \param group_name     Name of subject group.
!! \param index_field    Index or field which determines the order:
!!                       \li H5_INDEX_UNKNOWN_F = -1  - Unknown index type
!!                       \li H5_INDEX_NAME_F          - Index on names
!!                       \li H5_INDEX_CRT_ORDER_F     - Index on creation order
!!                       \li H5_INDEX_N_F             - Number of indices defined
!! \param order          Order within field or index:
!!                       \li H5_ITER_UNKNOWN_F  - Unknown order
!!                       \li H5_ITER_INC_F      - Increasing order
!!                       \li H5_ITER_DEC_F      - Decreasing order
!!                       \li H5_ITER_NATIVE_F   - No particular order, whatever is fastest
!!                       \li H5_ITER_N_F        - Number of iteration orders
!! \param n              Link for which to retrieve information.
!!                       NOTE: In C these are defined as a structure: H5L_info_t
!! \param link_type      Specifies the link class:
!!                       \li H5L_TYPE_HARD_F      - Hard link
!!                       \li H5L_TYPE_SOFT_F      - Soft link
!!                       \li H5L_TYPE_EXTERNAL_F  - External link
!!                       \li H5L_TYPE_ERROR _F    - Error
!! \param f_corder_valid Indicates whether the creation order data is valid for this attribute.
!! \param corder         Is a positive integer containing the creation order of the attribute.
!! \param cset           Indicates the character set used for the attribute&apos;s name.
!! \param token          If the link is a hard link, token specifies the object token that the link points to.
!! \param val_size       If the link is a symbolic link, val_size will be the length of the link value, e.g.,
!!                       the length of the name of the pointed-to object with a null terminator.
!! \param hdferr         \fortran_error
!!
!! \param lapl_id        Link access property list.
!!
!! See C API: @ref H5Lget_info_by_idx2()
!!
  SUBROUTINE h5lget_info_by_idx_f(loc_id, group_name, index_field, order, n, &
       link_type, f_corder_valid, corder, cset, token, val_size, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: group_name
    INTEGER, INTENT(IN) :: index_field
    INTEGER, INTENT(IN) :: order
    INTEGER(HSIZE_T), INTENT(IN) :: n
    INTEGER, INTENT(OUT) :: link_type
    LOGICAL, INTENT(OUT) :: f_corder_valid
    INTEGER, INTENT(OUT) :: corder
    INTEGER, INTENT(OUT) :: cset
    TYPE(H5O_TOKEN_T_F), INTENT(OUT), TARGET :: token
    INTEGER(SIZE_T), INTENT(OUT) :: val_size
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id
    INTEGER :: corder_valid
    INTEGER(SIZE_T)  :: group_namelen
    INTEGER(HID_T) :: lapl_id_default

    INTERFACE
       INTEGER FUNCTION h5lget_info_by_idx_c(loc_id, group_name, group_namelen, index_field, order, n, &
            link_type, corder_valid, corder, cset, token, val_size, lapl_id_default) &
            BIND(C,NAME='h5lget_info_by_idx_c')
         IMPORT :: c_char
         IMPORT :: HID_T, SIZE_T, HSIZE_T, H5O_TOKEN_T_F
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: group_name
         INTEGER(SIZE_T)  :: group_namelen
         INTEGER, INTENT(IN) :: index_field
         INTEGER, INTENT(IN) :: order
         INTEGER(HSIZE_T), INTENT(IN) :: n
         INTEGER, INTENT(OUT) :: link_type
         INTEGER :: corder_valid
         INTEGER, INTENT(OUT) :: corder
         INTEGER, INTENT(OUT) :: cset
         TYPE(H5O_TOKEN_T_F), INTENT(OUT) :: token
         INTEGER(SIZE_T), INTENT(OUT) :: val_size
         INTEGER(HID_T) :: lapl_id_default
       END FUNCTION h5lget_info_by_idx_c
    END INTERFACE

    group_namelen = LEN(group_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5lget_info_by_idx_c(loc_id, group_name, group_namelen, index_field, order, n, &
         link_type, corder_valid, corder, cset, token, val_size, lapl_id_default)

    f_corder_valid =.FALSE.
    IF (corder_valid .EQ. 1) f_corder_valid =.TRUE.

  END SUBROUTINE h5lget_info_by_idx_f

!>
!! \ingroup FH5L
!!
!! \brief Determines whether a class of user-defined links is registered.
!!
!! \param link_cls_id User-defined link class identifier.
!! \param registered  .TRUE. if the link class has been registered.
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Lis_registered()
!!
  SUBROUTINE h5lis_registered_f(link_cls_id, registered, hdferr)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: link_cls_id
    LOGICAL, INTENT(OUT) :: registered
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5lis_registered_c(link_cls_id) BIND(C,NAME='h5lis_registered_c')
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: link_cls_id
       END FUNCTION h5lis_registered_c
    END INTERFACE

    hdferr = h5lis_registered_c(link_cls_id)

    IF(hdferr.GT.0)THEN
       registered = .TRUE.
    ELSE IF(hdferr.EQ.0)THEN
       registered = .FALSE.
    ENDIF

  END SUBROUTINE h5lis_registered_f

!>
!! \ingroup FH5L
!!
!! \brief Renames a link within an HDF5 file.
!!
!! \param src_loc_id  Original file or group identifier.
!! \param src_name    Original link name.
!! \param dest_loc_id Destination file or group identifier.
!! \param dest_name   NEW link name.
!! \param hdferr      \fortran_error
!! \param lcpl_id     Link creation property list identifier to be associated WITH the NEW link.
!! \param lapl_id     Link access property list identifier to be associated WITH the NEW link.
!!
!! See C API: @ref H5Lmove()
!!
  SUBROUTINE h5lmove_f(src_loc_id, src_name, dest_loc_id, dest_name, hdferr, lcpl_id, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: src_loc_id
    CHARACTER(LEN=*), INTENT(IN) :: src_name
    INTEGER(HID_T), INTENT(IN) :: dest_loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dest_name
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lcpl_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id
    INTEGER(SIZE_T) :: src_namelen
    INTEGER(SIZE_T) :: dest_namelen

    INTEGER(HID_T) :: lcpl_id_default
    INTEGER(HID_T) :: lapl_id_default

    INTERFACE
       INTEGER FUNCTION h5lmove_c(src_loc_id, src_name, src_namelen, dest_loc_id, &
            dest_name, dest_namelen, lcpl_id_default, lapl_id_default) BIND(C,NAME='h5lmove_c')
         IMPORT :: c_char
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: src_loc_id

         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: src_name
         INTEGER(SIZE_T) :: src_namelen
         INTEGER(HID_T), INTENT(IN) :: dest_loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: dest_name
         INTEGER(SIZE_T) :: dest_namelen

         INTEGER(HID_T) :: lcpl_id_default
         INTEGER(HID_T) :: lapl_id_default

       END FUNCTION h5lmove_c
    END INTERFACE

    lcpl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lcpl_id)) lcpl_id_default = lcpl_id
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    src_namelen = LEN(src_name)
    dest_namelen = LEN(dest_name)

    hdferr = h5lmove_c(src_loc_id, src_name, src_namelen, dest_loc_id, &
         dest_name, dest_namelen, lcpl_id_default, lapl_id_default)

  END SUBROUTINE h5lmove_f

!>
!! \ingroup FH5L
!!
!! \brief Retrieves name of the nth link in a group, according to the order within a specified field or index.
!!
!! \param loc_id      File or group identifier specifying location of subject group.
!! \param group_name  Name of subject group.
!! \param index_field Index or field which determines the order:
!!                    \li H5_INDEX_UNKNOWN_F = -1  - Unknown index type
!!                    \li H5_INDEX_NAME_F          - Index on names
!!                    \li H5_INDEX_CRT_ORDER_F     - Index on creation order
!!                    \li H5_INDEX_N_F             - Number of indices defined
!! \param order       Order within field or index:
!!                    \li H5_ITER_UNKNOWN_F  - Unknown order
!!                    \li H5_ITER_INC_F      - Increasing order
!!                    \li H5_ITER_DEC_F      - Decreasing order
!!                    \li H5_ITER_NATIVE_F   - No particular order, whatever is fastest
!!                    \li H5_ITER_N_F        - Number of iteration orders
!! \param n           Link for which to retrieve information.
!! \param name        Buffer in which link value is returned.
!! \param hdferr      \fortran_error
!! \param lapl_id     List access property list identifier.
!! \param size        Maximum number of characters of link value to be returned.
!!
!! See C API: @ref H5Lget_name_by_idx()
!!
  SUBROUTINE h5lget_name_by_idx_f(loc_id, group_name, index_field, order, n, &
        name, hdferr, size, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: group_name
    INTEGER, INTENT(IN) :: index_field
    INTEGER, INTENT(IN) :: order
    INTEGER(HSIZE_T), INTENT(IN) :: n
    CHARACTER(LEN=*), INTENT(OUT) :: name
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(SIZE_T)  :: group_namelen
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id
    INTEGER(HID_T) :: lapl_id_default
    INTEGER(SIZE_T), INTENT(OUT), OPTIONAL :: size
    INTEGER(SIZE_T) :: size_default

    INTERFACE
       INTEGER FUNCTION h5lget_name_by_idx_c(loc_id, group_name, group_namelen, index_field, order, n, &
             size_default, name, lapl_id_default) BIND(C,NAME='h5lget_name_by_idx_c')
         IMPORT :: c_char
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id

         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: group_name
         INTEGER(SIZE_T)  :: group_namelen
         INTEGER, INTENT(IN) :: index_field
         INTEGER, INTENT(IN) :: order
         INTEGER(HSIZE_T), INTENT(IN) :: n
         INTEGER(SIZE_T) :: size_default
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: name
         INTEGER(HID_T) :: lapl_id_default
       END FUNCTION h5lget_name_by_idx_c
    END INTERFACE

    group_namelen = LEN(group_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    size_default = LEN(name)

    hdferr = h5lget_name_by_idx_c(loc_id, group_name, group_namelen, index_field, order, n, &
             size_default, name, lapl_id_default)

    IF(PRESENT(size)) size = size_default


  END SUBROUTINE h5lget_name_by_idx_f


!  HAS PROBLEM WITH void pointer in C
!!$!
!>
!!$  SUBROUTINE h5lget_val_by_idx_f(loc_id, group_name, index_field, order, n, &
!!$       f_corder_valid, corder, cset, data_size, hdferr, lapl_id)
!!$    IMPLICIT NONE

!!$                                        !    H5_INDEX_N_F            - Number of indices defined

!!$                                        !    H5_ITER_NATIVE_F   - No particular order, whatever is fastest


!!$       INTEGER FUNCTION h5lget_val_by_idx_c(loc_id, group_name, group_namelen, index_field, order, n, &
!!$            corder_valid, corder, cset, data_size, lapl_id_default)
!!$         USE H5GLOBAL
!!$         !DEC$IF DEFINED(HDF5F90_WINDOWS)
!!$         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LGET_VAL_BY_IDX_C'::h5lget_val_by_idx_c
!!$         !DEC$ENDIF

!!$
!!$    group_namelen = LEN(group_name)
!!$
!!$    lapl_id_default = H5P_DEFAULT_F
!!$    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id
!!$
!!$    hdferr = h5lget_info_by_idx_c(loc_id, group_name, group_namelen, index_field, order, n, &
!!$         corder_valid, corder, cset, data_size, lapl_id_default)
!!$
!!$    f_corder_valid =.FALSE.
!!$    IF (corder_valid .EQ. 1) f_corder_valid =.TRUE.
!!$
!!$  END SUBROUTINE h5lget_val_by_idx_f

!!$!
!>
!!$  SUBROUTINE h5lget_val_f(link_loc_id, link_name, size, linkval_buff,   &
!!$       hdferr, lapl_id)
!!$    IMPLICIT NONE

!!$    INTEGER :: corder_valid
!!$
!!$    INTEGER :: link_namelen
!!$    INTEGER(HID_T) :: lapl_id_default
!!$
!!$!  MS FORTRAN needs explicit interface for C functions called here.
!!$!
!!$    INTERFACE
!!$       INTEGER FUNCTION h5lget_val_c(link_loc_id, link_name, link_namelen, size, linkval_buff, &
!!$            lapl_id_default)
!!$         USE H5GLOBAL
!!$         !DEC$IF DEFINED(HDF5F90_WINDOWS)
!!$         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LGET_VAL_C'::h5lget_val_c
!!$         !DEC$ENDIF

!!$
!!$       END FUNCTION h5lget_val_c
!!$    END INTERFACE
!!$
!!$    link_namelen = LEN(link_name)
!!$
!!$    lapl_id_default = H5P_DEFAULT_F
!!$    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id
!!$
!!$    hdferr = h5lget_val_c(link_loc_id, link_name, link_namelen, size, linkval_buff, &
!!$         lapl_id_default)
!!$
!!$  END SUBROUTINE h5lget_val_f

!!$!
!>
!!$  SUBROUTINE H5Lregistered_f(version, class_id, comment, create_func, &
!!$       move_func, copy_func, trav_func, del_func, query_func, hdferr)
!!$    IMPLICIT NONE

!!$    INTEGER :: move_func_len
!!$    INTEGER :: copy_func_len
!!$    INTEGER :: trav_func_len
!!$    INTEGER :: del_func_len
!!$    INTEGER :: query_func_len
!!$
!!$    INTERFACE
!!$       INTEGER FUNCTION H5Lregistered_c(version, class_id, comment, &
!!$            create_func, create_func_len, &
!!$            move_func, move_func_len, &
!!$            copy_func, copy_func_len, &
!!$            trav_func, trav_func_len, &
!!$            del_func, del_func_len, &
!!$            query_func,query_func_len)
!!$         USE H5GLOBAL
!!$         !DEC$IF DEFINED(HDF5F90_WINDOWS)
!!$         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LREGISTERED_C'::H5Lregistered_c
!!$         !DEC$ENDIF

!!$         INTEGER :: move_func_len
!!$         INTEGER :: copy_func_len
!!$         INTEGER :: trav_func_len
!!$         INTEGER :: del_func_len
!!$         INTEGER :: query_func_len
!!$
!!$       END FUNCTION H5Lregistered_c
!!$    END INTERFACE
!!$
!!$    comment_len     = LEN(comment)
!!$    create_func_len = LEN(create_func)
!!$    move_func_len   = LEN(move_func)
!!$    copy_func_len   = LEN(copy_func)
!!$    trav_func_len   = LEN(trav_func)
!!$    del_func_len    = LEN(del_func)
!!$    query_func_len  = LEN(query_func)
!!$
!!$    hdferr = H5Lregistered_c(version, class_id, comment, &
!!$         create_func, create_func_len, &
!!$         move_func, move_func_len, &
!!$         copy_func, copy_func_len, &
!!$         trav_func, trav_func_len, &
!!$         del_func, del_func_len, &
!!$         query_func, query_func_len)
!!$
!!$  END SUBROUTINE H5Lregistered_f

!>
!! \ingroup FH5L
!!
!! \brief Iterates through links in a group.
!!
!! \param group_id     Identifier specifying subject group.
!! \param idx_type     Type of index which determines the order:
!!                      \li H5_INDEX_NAME_F      - Alphanumeric index on name
!!                      \li H5_INDEX_CRT_ORDER_F - Index on creation order
!! \param order        Order within index:
!!                      \li H5_ITER_INC_F    - Increasing order
!!                      \li H5_ITER_DEC_F    - Decreasing order
!!                      \li H5_ITER_NATIVE_F - Fastest available order
!! \param idx          Iteration position at which to start, or <br />
!!                     Position at which an interrupted iteration may be restarted
!! \param op           Callback function passing data regarding the link to the calling application.
!! \param op_data      User-defined pointer to data required by the application for its processing of the link.
!! \param return_value Return context:
!!                      \li Success: The return value of the first operator that
!!                               returns non-zero, or zero if all members were processed with no operator returning non-zero.
!!                      \li Failure: Negative if something goes wrong within the
!!                               library, or the negative value returned by one of the operators.
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5Literate2()
!!
  SUBROUTINE h5literate_f(group_id, idx_type, order, idx, op, op_data, return_value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)    :: group_id
    INTEGER         , INTENT(IN)    :: idx_type
    INTEGER         , INTENT(IN)    :: order
    INTEGER(HSIZE_T), INTENT(INOUT) :: idx
    TYPE(C_FUNPTR)  , INTENT(IN)    :: op
    TYPE(C_PTR)     , INTENT(IN)    :: op_data
    INTEGER         , INTENT(OUT)   :: return_value
    INTEGER         , INTENT(OUT)   :: hdferr

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Literate2(group_id, idx_type, order, idx, op, op_data) &
            BIND(C, NAME='H5Literate2')
         IMPORT :: C_INT, C_PTR, C_FUNPTR
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T)  , VALUE :: group_id
         INTEGER(C_INT) , VALUE :: idx_type
         INTEGER(C_INT) , VALUE :: order
         INTEGER(HSIZE_T)        :: idx
         TYPE(C_FUNPTR)  , VALUE :: op
         TYPE(C_PTR)     , VALUE :: op_data
       END FUNCTION H5Literate2
    END INTERFACE

    return_value = INT(H5Literate2(group_id, INT(idx_type, C_INT), INT(order, C_INT), idx, op, op_data))

    IF(return_value.GE.0)THEN
       hdferr = 0
    ELSE
       hdferr = -1
    END IF

  END SUBROUTINE h5literate_f

!>
!! \ingroup FH5L
!!
!! \brief Asynchronously iterates through links in a group.
!!
!! \param group_id    Identifier specifying subject group.
!! \param idx_type    Type of index which determines the order:
!!                      \li H5_INDEX_NAME_F      - Alphanumeric index on name
!!                      \li H5_INDEX_CRT_ORDER_F - Index on creation order
!! \param order         Order within index:
!!                      \li H5_ITER_INC_F    - Increasing order
!!                      \li H5_ITER_DEC_F    - Decreasing order
!!                      \li H5_ITER_NATIVE_F - Fastest available order
!! \param idx          Iteration position at which to start, or <br />
!!                     Position at which an interrupted iteration may be restarted
!! \param op           Callback function passing data regarding the link to the calling application.
!! \param op_data      User-defined pointer to data required by the application for its processing of the link.
!! \param return_value N/A
!!                     
!! \warning            The returned value of the callback routine op will not be set
!!                     in \p return_value for H5Literate_async_f(), so \p return_value should
!!                     not be used for determining the return state of the callback routine.
!!
!! \param es_id        \fortran_es_id
!! \param hdferr       \fortran_error
!! \param file         \fortran_file
!! \param func         \fortran_func
!! \param line         \fortran_line
!!
!! See C API: @ref H5Literate_async()
!!
  SUBROUTINE h5literate_async_f(group_id, idx_type, order, idx, op, op_data, return_value, es_id, hdferr, &
       file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)    :: group_id
    INTEGER         , INTENT(IN)    :: idx_type
    INTEGER         , INTENT(IN)    :: order
    INTEGER(HSIZE_T), INTENT(INOUT) :: idx
    TYPE(C_FUNPTR)  , INTENT(IN)    :: op
    TYPE(C_PTR)     , INTENT(IN)    :: op_data
    INTEGER         , INTENT(OUT)   :: return_value
    INTEGER(HID_T)  , INTENT(IN)    :: es_id
    INTEGER         , INTENT(OUT)   :: hdferr
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Literate_async(file, func, line, &
            group_id, idx_type, order, idx, op, op_data, es_id) BIND(C, NAME='H5Literate_async')
         IMPORT :: C_CHAR, C_INT, C_PTR, C_FUNPTR
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T)  , VALUE :: group_id
         INTEGER(C_INT) , VALUE :: idx_type
         INTEGER(C_INT) , VALUE :: order
         INTEGER(HSIZE_T)        :: idx
         TYPE(C_FUNPTR)  , VALUE :: op
         TYPE(C_PTR)     , VALUE :: op_data
         INTEGER(HID_T)  , VALUE :: es_id
       END FUNCTION H5Literate_async
    END INTERFACE

    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    return_value = INT(H5Literate_async(file_default, func_default, line_default, &
         group_id, INT(idx_type, C_INT), INT(order, C_INT), idx, op, op_data, es_id))

    IF(return_value.GE.0)THEN
       hdferr = 0
    ELSE
       hdferr = -1
    END IF

  END SUBROUTINE h5literate_async_f

!>
!! \ingroup FH5L
!!
!! \brief Iterates through links in a group.
!!
!! \param loc_id        File or group identifier specifying location of subject group.
!! \param group_name    Name of subject group.
!! \param index_type    Type of index which determines the order:
!!                      \li H5_INDEX_NAME_F      - Alphanumeric index on name
!!                      \li H5_INDEX_CRT_ORDER_F - Index on creation order
!! \param order        Order within index:
!!                     \li H5_ITER_INC_F    - Increasing order
!!                     \li H5_ITER_DEC_F    - Decreasing order
!!                     \li H5_ITER_NATIVE_F - Fastest available order
!! \param idx          Iteration position at which to start, or <br />
!!                     Position at which an interrupted iteration may be restarted
!! \param op           Callback function passing data regarding the link to the calling application.
!! \param op_data      User-defined pointer to data required by the application for its processing of the link.
!! \param return_value Return context:
!!                      \li Success: The return value of the first operator that returns non-zero, or zero if
!!                               all members were processed with no operator returning non-zero.
!!                      \li Failure: Negative if something goes wrong within the
!!                               library, or the negative value returned by one of the operators.
!! \param hdferr       \fortran_error
!! \param lapl_id      Link access property list
!!
!! See C API: @ref H5Literate_by_name2()
!!
  SUBROUTINE h5literate_by_name_f(loc_id, group_name, index_type, order, &
       idx, op, op_data, return_value, hdferr, lapl_id)

    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)           :: loc_id
    CHARACTER(LEN=*), INTENT(IN)           :: group_name
    INTEGER         , INTENT(IN)           :: index_type
    INTEGER         , INTENT(IN)           :: order
    INTEGER(HSIZE_T), INTENT(INOUT)        :: idx
    TYPE(C_FUNPTR)  , INTENT(IN)           :: op
    TYPE(C_PTR)     , INTENT(IN)           :: op_data
    INTEGER         , INTENT(OUT)          :: return_value
    INTEGER         , INTENT(OUT)          :: hdferr
    INTEGER(HID_T)  , INTENT(IN), OPTIONAL :: lapl_id
    INTEGER(HID_T) :: lapl_id_default
    INTEGER(SIZE_T) :: namelen

    INTERFACE
       INTEGER FUNCTION h5literate_by_name_c(loc_id, name, namelen, index_type, order,&
            idx, op, op_data, lapl_id_default) BIND(C, NAME='h5literate_by_name_c')
         IMPORT :: c_char, c_ptr, c_funptr
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T)  , INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER(SIZE_T) , INTENT(IN) :: namelen
         INTEGER         , INTENT(IN) :: index_type
         INTEGER         , INTENT(IN) :: order
         INTEGER(HSIZE_T), INTENT(INOUT) :: idx
         TYPE(C_FUNPTR), VALUE :: op
         TYPE(C_PTR), VALUE :: op_data
         INTEGER(HID_T)  , INTENT(IN) :: lapl_id_default
       END FUNCTION
    END INTERFACE

    namelen  = LEN(group_name)
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    return_value = h5literate_by_name_c(loc_id, group_name, namelen, index_type, order, idx, op, op_data, lapl_id_default)

    IF(return_value.GE.0)THEN
       hdferr = 0
    ELSE
       hdferr = -1
    END IF

  END SUBROUTINE h5literate_by_name_f

!>
!! \ingroup FH5L
!!
!! \brief Recursively visits all links starting from a specified group.
!!
!! \param grp_id       Group identifier
!! \param idx_type     Index type
!! \param order	       Iteration order
!! \param op	       Callback function
!! \param op_data      User-defined callback function context
!! \param return_value The return value of the first operator that returns non-zero, or zero if
!!                     all members were processed with no operator returning non-zero.
!! \param hdferr       \fortran_error
!!
!! See C API: @ref H5Lvisit2()
!!
  SUBROUTINE h5lvisit_f(grp_id, idx_type, order, op, op_data, return_value, hdferr)

    IMPLICIT NONE

    INTEGER(hid_t), INTENT(IN)    :: grp_id
    INTEGER       , INTENT(IN)    :: idx_type
    INTEGER       , INTENT(IN)    :: order
    TYPE(C_FUNPTR), INTENT(IN)    :: op
    TYPE(C_PTR)   , INTENT(INOUT) :: op_data  ! Declare INOUT to bypass gfortran 4.8.5 issue
    INTEGER       , INTENT(OUT)   :: return_value
    INTEGER       , INTENT(OUT)   :: hdferr

    INTEGER(C_INT) :: return_value_c

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Lvisit(grp_id, idx_type, order, op, op_data) BIND(C, NAME='H5Lvisit2')
         IMPORT :: c_char, c_int, c_ptr, c_funptr
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: grp_id
         INTEGER(C_INT), VALUE :: idx_type
         INTEGER(C_INT), VALUE :: order
         TYPE(C_FUNPTR), VALUE :: op
         TYPE(C_PTR)   , VALUE :: op_data
       END FUNCTION H5Lvisit
    END INTERFACE

    return_value_c = INT(H5Lvisit(grp_id, INT(idx_type, C_INT), INT(order, C_INT), op, op_data))
    return_value = INT(return_value_c)

    IF(return_value.GE.0)THEN
       hdferr = 0
    ELSE
       hdferr = -1
    END IF

  END SUBROUTINE H5Lvisit_f

!>
!! \ingroup FH5L
!!
!! \brief Recursively visits all links starting from a specified group.
!!
!! \param loc_id       Location identifier
!! \param group_name   Group name
!! \param idx_type     Index type
!! \param order	       Iteration order
!! \param op	       Callback function
!! \param op_data      User-defined callback function context
!! \param return_value The return value of the first operator that returns non-zero, or zero if
!!                     all members were processed with no operator returning non-zero.
!! \param hdferr       \fortran_error
!! \param lapl_id      Link access property list
!!
!!
!! See C API: @ref H5Lvisit_by_name2()
!!
  SUBROUTINE H5Lvisit_by_name_f(loc_id, group_name, idx_type, order, op, op_data, return_value, hdferr, lapl_id)

    IMPLICIT NONE

    INTEGER(HID_T)  , INTENT(IN)    :: loc_id
    CHARACTER(LEN=*), INTENT(IN)    :: group_name
    INTEGER         , INTENT(IN)    :: idx_type
    INTEGER         , INTENT(IN)    :: order
    TYPE(C_FUNPTR)  , INTENT(IN)    :: op
    TYPE(C_PTR)     , INTENT(INOUT) :: op_data ! Declare INOUT to bypass gfortran 4.8.5 issue
    INTEGER         , INTENT(OUT)   :: return_value
    INTEGER       , INTENT(OUT)     :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id

    INTEGER(HID_T) :: lapl_id_default
    CHARACTER(LEN=LEN_TRIM(group_name)+1,KIND=C_CHAR) :: c_name
    INTEGER(C_INT)  :: return_value_c

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Lvisit_by_name(loc_id, group_name, idx_type, order, op, op_data, lapl_id) &
            BIND(C, NAME='H5Lvisit_by_name2')
         IMPORT :: C_CHAR, C_INT, C_PTR, C_FUNPTR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: group_name
         INTEGER(C_INT), VALUE :: idx_type
         INTEGER(C_INT), VALUE :: order
         TYPE(C_FUNPTR), VALUE :: op
         TYPE(C_PTR)   , VALUE :: op_data
         INTEGER(HID_T), VALUE :: lapl_id
       END FUNCTION H5Lvisit_by_name
    END INTERFACE

    c_name = TRIM(group_name)//C_NULL_CHAR

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    return_value_c = INT(H5Lvisit_by_name(loc_id, c_name, INT(idx_type, C_INT), INT(order, C_INT), op, op_data, lapl_id_default))
    return_value = INT(return_value_c)

    IF(return_value.GE.0)THEN
       hdferr = 0
    ELSE
       hdferr = -1
    END IF

  END SUBROUTINE H5Lvisit_by_name_f

END MODULE H5L
