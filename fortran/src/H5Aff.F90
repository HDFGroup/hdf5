!> @defgroup FH5A Fortran Attribute (H5A) Interface
!!
!! @see H5A, C-API
!!
!! @see @ref H5A_UG, User Guide
!!

!> @ingroup FH5A
!!
!! @brief This module contains Fortran interfaces for H5A functions.
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
!
!  (A) C_LOC and character strings according to the Fortran 2003 standard:
!
!  15.1.2.5 C_LOC(X)
!
!  Argument. X shall either
!
!  (1) have interoperable type and type parameters and be
!    (a) a variable that has the TARGET attribute and is interoperable,
!    (b) an allocated allocatable variable that has the TARGET attribute
!        and is not an array of zero size, or
!    (c) an associated scalar pointer, or
!  (2) be a nonpolymorphic scalar, have no length type parameters, and be
!    (a) a nonallocatable, nonpointer variable that has the TARGET attribute,
!    (b) an allocated allocatable variable that has the TARGET attribute, or
!    (c) an associated pointer.
!
!  - When X is a character, for interoperability the standard is:
!
!  15.2.1 Interoperability of intrinsic types
!
!  ...if the type is character, interoperability also requires that the length type parameter
!  be omitted or be specified by an initialization expression whose value is one.
!
!  THEREFORE compilers that have not extended the standard  require
!
!  CHARACTER(LEN=1), TARGET :: chr
!  or
!  CHARACTER, TARGET :: chr
!
!  (B)
!       _____ __  __ _____   ____  _____ _______       _   _ _______
!      |_   _|  \/  |  __ \ / __ \|  __ \__   __|/\   | \ | |__   __|
! ****   | | | \  / | |__) | |  | | |__) | | |  /  \  |  \| |  | |    ****
! ****   | | | |\/| |  ___/| |  | |  _  /  | | / /\ \ | . ` |  | |    ****
! ****  _| |_| |  | | |    | |__| | | \ \  | |/ ____ \| |\  |  | |    ****
!      |_____|_|  |_|_|     \____/|_|  \_\ |_/_/    \_\_| \_|  |_|
!
!  If you add a new H5A function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!

#include <H5config_f.inc>

MODULE H5A

  USE H5GLOBAL
  IMPLICIT NONE

  PRIVATE h5awrite_char_scalar, h5awrite_ptr
  PRIVATE h5aread_char_scalar, h5aread_ptr


#ifndef H5_DOXYGEN
  INTERFACE h5awrite_f
     MODULE PROCEDURE h5awrite_char_scalar
     ! This is the preferred way to call h5awrite
     ! by passing an address
     MODULE PROCEDURE h5awrite_ptr
  END INTERFACE

  INTERFACE h5aread_f
     MODULE PROCEDURE h5aread_char_scalar
     ! This is the preferred way to call h5aread
     ! by passing an address
     MODULE PROCEDURE h5aread_ptr
  END INTERFACE

!  Interface for the function used to pass the C pointer of the buffer
!  to the C H5Awrite routine
  INTERFACE
     INTEGER FUNCTION h5awrite_f_c(attr_id, memtype_id, buf) BIND(C, NAME='h5awrite_f_c')
       IMPORT :: c_ptr
       IMPORT :: HID_T
       IMPLICIT NONE
       INTEGER(HID_T), INTENT(IN) :: attr_id
       INTEGER(HID_T), INTENT(IN) :: memtype_id
       TYPE(C_PTR), VALUE :: buf
     END FUNCTION h5awrite_f_c
  END INTERFACE

!  Interface for the function used to pass the C pointer of the buffer
!  to the C H5Aread routine
  INTERFACE
     INTEGER FUNCTION h5aread_f_c(attr_id, memtype_id, buf) BIND(C, NAME='h5aread_f_c')
       IMPORT :: c_ptr
       IMPORT :: HID_T
       IMPLICIT NONE
       INTEGER(HID_T), INTENT(IN) :: attr_id
       INTEGER(HID_T), INTENT(IN) :: memtype_id
       TYPE(C_PTR), VALUE :: buf
     END FUNCTION h5aread_f_c
  END INTERFACE

  INTERFACE
     INTEGER(HID_T) FUNCTION H5Aopen(obj_id, attr_name, aapl_id_default) &
          BIND(C,NAME='H5Aopen')
       IMPORT :: C_CHAR
       IMPORT :: HID_T
       IMPLICIT NONE
       INTEGER(HID_T), VALUE :: obj_id
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: attr_name
       INTEGER(HID_T), VALUE :: aapl_id_default
     END FUNCTION H5Aopen
  END INTERFACE

#endif

CONTAINS

!>
!! \ingroup FH5A
!!
!! \brief Creates a dataset as an attribute of a group, dataset, or named datatype.
!!
!! \param loc_id   Identifier of an object (group, dataset, or named datatype) attribute is attached to
!! \param name     Attribute name
!! \param type_id  Attribute datatype identifier
!! \param space_id Attribute dataspace identifier
!! \param attr_id  Attribute identifier
!! \param hdferr   \fortran_error
!! \param acpl_id  Attribute creation property list identifier
!! \param aapl_id  Attribute access property list identifier
!!
!! See C API: @ref H5Acreate2()
!!
  SUBROUTINE h5acreate_f(loc_id, name, type_id, space_id, attr_id, &
       hdferr, acpl_id, aapl_id )
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HID_T), INTENT(OUT) :: attr_id
    INTEGER, INTENT(OUT) :: hdferr

    INTEGER(HID_T), INTENT(IN), OPTIONAL :: acpl_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: aapl_id

    INTEGER(HID_T) :: acpl_id_default
    INTEGER(HID_T) :: aapl_id_default
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name
    INTERFACE
       INTEGER(HID_T) FUNCTION H5Acreate2(loc_id, name, type_id, &
            space_id, acpl_id_default, aapl_id_default) BIND(C,NAME='H5Acreate2')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER(HID_T), INTENT(IN), VALUE :: type_id
         INTEGER(HID_T), INTENT(IN), VALUE :: space_id
         INTEGER(HID_T), INTENT(IN), VALUE :: acpl_id_default
         INTEGER(HID_T), INTENT(IN), VALUE :: aapl_id_default
       END FUNCTION H5Acreate2
    END INTERFACE

    c_name = TRIM(name)//C_NULL_CHAR

    acpl_id_default = H5P_DEFAULT_F
    aapl_id_default = H5P_DEFAULT_F
    IF (PRESENT(acpl_id)) acpl_id_default = acpl_id
    IF (PRESENT(aapl_id)) aapl_id_default = aapl_id

    attr_id = h5acreate2(loc_id, c_name, type_id, space_id, &
         acpl_id_default, aapl_id_default)

    hdferr = 0
    IF(attr_id.LT.0) hdferr = -1

  END SUBROUTINE h5acreate_f

!>
!! \ingroup FH5A
!!
!! \brief Asynchronously creates a dataset as an attribute of a group, dataset, or named datatype.
!!
!! \param loc_id   Identifier of an object (group, dataset, or named datatype) attribute is attached to
!! \param name     Attribute name
!! \param type_id  Attribute datatype identifier
!! \param space_id Attribute dataspace identifier
!! \param attr_id  Attribute identifier
!! \param es_id    \fortran_es_id
!! \param hdferr   \fortran_error
!! \param acpl_id  Attribute creation property list identifier
!! \param aapl_id  Attribute access property list identifier
!! \param file     \fortran_file
!! \param func     \fortran_func
!! \param line     \fortran_line
!!
!! See C API: @ref H5Acreate_async()
!!
  SUBROUTINE h5acreate_async_f(loc_id, name, type_id, space_id, attr_id, es_id, &
       hdferr, acpl_id, aapl_id, file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(HID_T), INTENT(IN) :: type_id
    INTEGER(HID_T), INTENT(IN) :: space_id
    INTEGER(HID_T), INTENT(OUT) :: attr_id
    INTEGER(HID_T), INTENT(IN) :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T)  , INTENT(IN), OPTIONAL :: acpl_id
    INTEGER(HID_T)  , INTENT(IN), OPTIONAL :: aapl_id
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    INTEGER(HID_T) :: acpl_id_default
    INTEGER(HID_T) :: aapl_id_default
    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Acreate_async(file, func, line, loc_id, name, type_id, &
            space_id, acpl_id_default, aapl_id_default, es_id) BIND(C,NAME='H5Acreate_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
         INTEGER(HID_T), VALUE :: type_id
         INTEGER(HID_T), VALUE :: space_id
         INTEGER(HID_T), VALUE :: acpl_id_default
         INTEGER(HID_T), VALUE :: aapl_id_default
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Acreate_async
    END INTERFACE

    acpl_id_default = H5P_DEFAULT_F
    aapl_id_default = H5P_DEFAULT_F
    IF (PRESENT(acpl_id)) acpl_id_default = acpl_id
    IF (PRESENT(aapl_id)) aapl_id_default = aapl_id
    IF (PRESENT(file)) file_default = file
    IF (PRESENT(func)) func_default = func
    IF (PRESENT(line)) line_default = INT(line, C_INT)

    c_name = TRIM(name)//C_NULL_CHAR

    attr_id = h5acreate_async(file_default, func_default, line_default, &
         loc_id, c_name, type_id, space_id, &
         acpl_id_default, aapl_id_default, es_id)

    hdferr = 0
    IF(attr_id.LT.0) hdferr = -1

  END SUBROUTINE h5acreate_async_f

!>
!! \ingroup FH5A
!!
!! \brief Opens an attribute specified by name.
!!
!! \param obj_id  Identifier of a group, dataset, or named datatype attribute to be attached to
!! \param name    Attribute name
!! \param attr_id Attribute identifier
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Aopen_name()
!!
  SUBROUTINE H5Aopen_name_f(obj_id, name, attr_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(HID_T), INTENT(OUT) :: attr_id
    INTEGER, INTENT(OUT) :: hdferr

    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name

    c_name = TRIM(name)//C_NULL_CHAR
    attr_id = H5Aopen(obj_id, c_name, H5P_DEFAULT_F)

    hdferr = 0
    IF(attr_id.LT.0) hdferr = -1

  END SUBROUTINE H5Aopen_name_f

#ifndef H5_NO_DEPRECATED_SYMBOLS
!>
!! \ingroup FH5A
!!
!! \brief Opens the attribute specified by its index.
!!
!! \deprecation_note{H5Aopen_by_idx_f()}
!!
!! \param obj_id  Identifier of a group, dataset, or named datatype an attribute to be attached to
!! \param index   Index of the attribute to open (zero-based)
!! \param attr_id Attribute identifier
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Aopen_idx()
!!
  SUBROUTINE h5aopen_idx_f(obj_id, index, attr_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    INTEGER, INTENT(IN) :: index
    INTEGER(HID_T), INTENT(OUT) :: attr_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER(HID_T) FUNCTION H5Aopen_idx(obj_id, index) BIND(C,NAME='H5Aopen_idx')
         IMPORT :: HID_T
         IMPORT :: C_INT
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: obj_id
         INTEGER(C_INT), VALUE :: index
       END FUNCTION H5Aopen_idx
    END INTERFACE

    attr_id = H5Aopen_idx(obj_id, INT(index, C_INT))

    hdferr = 0
    IF(attr_id.LT.0) hdferr = -1

  END SUBROUTINE h5aopen_idx_f
#endif

!>
!! \ingroup FH5A
!!
!! \brief Gets a copy of the dataspace for an attribute.
!!
!! \param attr_id  Attribute identifier
!! \param space_id Attribite dataspace identifier
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Aget_space()
!!
  SUBROUTINE h5aget_space_f(attr_id, space_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id
    INTEGER(HID_T), INTENT(OUT) :: space_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER(HID_T) FUNCTION H5Aget_space(attr_id) BIND(C,NAME='H5Aget_space')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: attr_id
       END FUNCTION H5Aget_space
    END INTERFACE

    space_id = H5Aget_space(attr_id)

    hdferr = 0
    IF(space_id.LT.0) hdferr = -1

  END SUBROUTINE h5aget_space_f
!>
!! \ingroup FH5A
!!
!! \brief Gets an attribute datatype.
!!
!! \param attr_id Attribute identifier
!! \param type_id Attribute datatype identifier
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Aget_type()
!!
  SUBROUTINE h5aget_type_f(attr_id, type_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id
    INTEGER(HID_T), INTENT(OUT) :: type_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER(HID_T) FUNCTION H5Aget_type(attr_id) BIND(C,NAME='H5Aget_type')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: attr_id
       END FUNCTION H5Aget_type
    END INTERFACE

    type_id = H5Aget_type(attr_id)

    hdferr = 0
    IF(type_id.LT.0) hdferr = -1

  END SUBROUTINE h5aget_type_f
!>
!! \ingroup FH5A
!!
!! \brief Gets an attribute name.
!!
!! \param attr_id Attribute identifier
!! \param size    Size of a buffer to read name in
!! \param buf     Buffer to read name in
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Aget_name()
!!
  SUBROUTINE h5aget_name_f(attr_id, size, buf, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id
    INTEGER(SIZE_T), INTENT(IN) :: size
    CHARACTER(LEN=*), INTENT(INOUT) :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER FUNCTION h5aget_name_c(attr_id, size, buf) &
            BIND(C,NAME='h5aget_name_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(SIZE_T), INTENT(IN) :: size
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: buf
       END FUNCTION h5aget_name_c
    END INTERFACE

    hdferr = h5aget_name_c(attr_id, size, buf)
  END SUBROUTINE h5aget_name_f
!>
!! \ingroup FH5A
!!
!! \brief Gets an attribute name, by attribute index position.
!!
!! \param loc_id   Location of object to which attribute is attached
!! \param obj_name Name of object to which attribute is attached, relative to location
!! \param idx_type Type of index; Possible values are:
!!                 \li H5_INDEX_UNKNOWN_F = -1  - Unknown index type
!!                 \li H5_INDEX_NAME_F          - Index on names
!!                 \li H5_INDEX_CRT_ORDER_F     - Index on creation order
!!                 \li H5_INDEX_N_F             - Number of indices defined
!!
!! \param order    Index traversal order in which to iterate over index; Possible values are:
!!                 \li H5_ITER_UNKNOWN_F   - Unknown order
!!                 \li H5_ITER_INC_F       - Increasing order
!!                 \li H5_ITER_DEC_F       - Decreasing order
!!                 \li H5_ITER_NATIVE_F    - No particular order, whatever is fastest
!!                 \li H5_ITER_N_F         - Number of iteration orders
!! \param n        Attribute&apos;s position in index
!! \param name     Attribute name
!! \param hdferr   \fortran_error
!!
!! \param size     Size, in bytes, of attribute name
!! \param lapl_id  Link access property list
!!
!! See C API: @ref H5Aget_name_by_idx()
!!
  SUBROUTINE h5aget_name_by_idx_f(loc_id, obj_name, idx_type, order, &
       n, name, hdferr, size, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: obj_name
    INTEGER, INTENT(IN) :: idx_type
    INTEGER, INTENT(IN) :: order
    INTEGER(HSIZE_T), INTENT(IN) :: n
    CHARACTER(LEN=*), INTENT(OUT) :: name
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id
    INTEGER(SIZE_T), INTENT(OUT), OPTIONAL :: size
    INTEGER(HID_T) :: lapl_id_default
    INTEGER(SIZE_T) :: obj_namelen
    INTEGER(SIZE_T) :: size_default

    INTERFACE
       INTEGER FUNCTION h5aget_name_by_idx_c(loc_id, obj_name, obj_namelen, idx_type, order, &
            n, name, size_default, lapl_id_default) BIND(C,NAME='h5aget_name_by_idx_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: obj_name
         INTEGER, INTENT(IN) :: idx_type
         INTEGER, INTENT(IN) :: order
         INTEGER(HSIZE_T), INTENT(IN) :: n
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: name
         INTEGER(SIZE_T) :: size_default
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(SIZE_T) :: obj_namelen
       END FUNCTION h5aget_name_by_idx_c
    END INTERFACE

    obj_namelen = LEN(obj_name)
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    size_default = LEN(name)

    hdferr = h5aget_name_by_idx_c(loc_id, obj_name, obj_namelen, idx_type, order, &
         n, name, size_default, lapl_id_default)

    IF(PRESENT(size)) size = size_default


  END SUBROUTINE h5aget_name_by_idx_f
!>
!! \ingroup FH5A
!!
!! \brief Determines the number of attributes attached to an object.
!!
!! \param obj_id   Object (group, dataset, or named datatype) identifier
!! \param attr_num Number of attributes attached to the object
!! \param hdferr   \fortran_error
!!
!! See C API: @ref H5Aget_num_attrs()
!!
  SUBROUTINE h5aget_num_attrs_f(obj_id, attr_num, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    INTEGER, INTENT(OUT) :: attr_num
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION h5aget_num_attrs_c(obj_id, attr_num) BIND(C,name='h5aget_num_attrs_c')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: obj_id
         INTEGER, INTENT(OUT) :: attr_num
       END FUNCTION h5aget_num_attrs_c
    END INTERFACE

    hdferr = h5aget_num_attrs_c(obj_id, attr_num)
  END SUBROUTINE h5aget_num_attrs_f

!>
!! \ingroup FH5A
!!
!! \brief Deletes an attribute of an object (group, dataset or named datatype)
!!
!! \param obj_id Object identifier
!! \param name   Attribute name
!!
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Adelete()
!!
  SUBROUTINE h5adelete_f(obj_id, name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(SIZE_T) :: namelen

    INTERFACE
       INTEGER FUNCTION H5Adelete_c(obj_id, name, namelen) BIND(C,NAME='h5adelete_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: obj_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER(SIZE_T) :: namelen
       END FUNCTION H5Adelete_c
    END INTERFACE

    namelen = LEN(name)
    hdferr = H5Adelete_c(obj_id, name, namelen)
  END SUBROUTINE h5adelete_f

!>
!! \ingroup FH5A
!!
!! \brief Closes the specified attribute.
!!
!! \param attr_id Attribute identifier
!! \param hdferr \fortran_error
!!
!! See C API: @ref H5Aclose()
!!
  SUBROUTINE h5aclose_f(attr_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER FUNCTION H5Aclose(attr_id) BIND(C, NAME='H5Aclose')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: attr_id
       END FUNCTION H5Aclose
    END INTERFACE

    hdferr = INT(H5Aclose(attr_id))
  END SUBROUTINE h5aclose_f

!>
!! \ingroup FH5A
!!
!! \brief Asynchronously closes the specified attribute.
!!
!! \param attr_id Attribute identifier
!! \param es_id  \fortran_es_id
!! \param hdferr \fortran_error
!! \param file   \fortran_file
!! \param func   \fortran_func
!! \param line   \fortran_line
!!
!! See C API: @ref H5Aclose_async()
!!
  SUBROUTINE h5aclose_async_f(attr_id, es_id, hdferr, file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id
    INTEGER(HID_T), INTENT(IN) :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER FUNCTION H5Aclose_async(file, func, line, attr_id, es_id) BIND(C, NAME='H5Aclose_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: attr_id
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Aclose_async
    END INTERFACE

    IF (PRESENT(file)) file_default = file
    IF (PRESENT(func)) func_default = func
    IF (PRESENT(line)) line_default = INT(line, C_INT)

    hdferr = H5Aclose_async(file_default, func_default, line_default, attr_id, es_id)

  END SUBROUTINE h5aclose_async_f

!>
!! \ingroup FH5A
!!
!! \brief Returns the amount of storage required for an attribute.
!!
!! \param attr_id Attribute identifier
!! \param size    Attribute storage size
!! \param hdferr  \fortran_error
!!
!! See C API: @ref H5Aget_storage_size()
!!
  SUBROUTINE h5aget_storage_size_f(attr_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id
    INTEGER(HSIZE_T), INTENT(OUT) :: size
    INTEGER, INTENT(OUT) :: hdferr

    INTERFACE
       INTEGER(HSIZE_T) FUNCTION H5Aget_storage_size(attr_id) BIND(C,NAME='H5Aget_storage_size')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T),  INTENT(IN), VALUE :: attr_id
       END FUNCTION H5Aget_storage_size
    END INTERFACE

    size = H5Aget_storage_size(attr_id)

    hdferr = 0
    IF(size.LT.0) hdferr = -1

  END SUBROUTINE h5aget_storage_size_f

!>
!! \ingroup FH5A
!!
!! \brief Gets an attribute creation property list identifier
!!
!! \param attr_id          Identifier of the attribute
!! \param creation_prop_id Identifier for the attribute&apos;s creation property
!! \param hdferr           \fortran_error
!!
!! See C API: @ref H5Aget_create_plist()
!!
  SUBROUTINE h5aget_create_plist_f(attr_id, creation_prop_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id
    INTEGER(HID_T), INTENT(OUT) :: creation_prop_id
    INTEGER, INTENT(OUT) :: hdferr
    INTERFACE
       INTEGER(HID_T) FUNCTION H5Aget_create_plist(attr_id) BIND(C,NAME='H5Aget_create_plist')
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN), VALUE :: attr_id
       END FUNCTION H5Aget_create_plist
    END INTERFACE

    creation_prop_id = H5Aget_create_plist(attr_id)

    hdferr = 0
    IF(creation_prop_id.LT.0) hdferr = -1

  END SUBROUTINE h5aget_create_plist_f

!>
!! \ingroup FH5A
!!
!! \brief Renames an attribute
!!
!! \param loc_id        Location or object identifier; may be dataset or group or named datatype
!! \param obj_name      Name of object, relative to location, whose attribute is to be renamed
!! \param old_attr_name Prior attribute name
!! \param new_attr_name New attribute name
!! \param hdferr        \fortran_error
!! \param lapl_id       Link access property list identifier
!!
!! See C API: @ref H5Arename_by_name()
!!
  SUBROUTINE h5arename_by_name_f(loc_id, obj_name, old_attr_name, new_attr_name, &
        hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: obj_name
    CHARACTER(LEN=*), INTENT(IN) :: old_attr_name
    CHARACTER(LEN=*), INTENT(IN) :: new_attr_name
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id

    INTEGER(HID_T) :: lapl_id_default
    CHARACTER(LEN=LEN_TRIM(obj_name) +1,KIND=C_CHAR) :: c_obj_name
    CHARACTER(LEN=LEN_TRIM(old_attr_name)+1,KIND=C_CHAR) :: c_old_attr_name
    CHARACTER(LEN=LEN_TRIM(new_attr_name)+1,KIND=C_CHAR) :: c_new_attr_name

    INTERFACE
       INTEGER FUNCTION H5Arename_by_name(loc_id, obj_name, &
            old_attr_name, new_attr_name, lapl_id_default) &
            BIND(C,NAME='H5Arename_by_name')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: obj_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: old_attr_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: new_attr_name
         INTEGER(HID_T), VALUE :: lapl_id_default

       END FUNCTION H5Arename_by_name
    END INTERFACE

    c_obj_name      = TRIM(obj_name)//C_NULL_CHAR
    c_old_attr_name = TRIM(old_attr_name)//C_NULL_CHAR
    c_new_attr_name = TRIM(new_attr_name)//C_NULL_CHAR

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default=lapl_id

    hdferr = 0
    hdferr = H5Arename_by_name(loc_id, c_obj_name, c_old_attr_name, c_new_attr_name, lapl_id_default)

  END SUBROUTINE h5arename_by_name_f
!>
!! \ingroup FH5A
!!
!! \brief Asynchronously renames an attribute
!!
!! \param loc_id        Location or object identifier; may be dataset or group or named datatype
!! \param obj_name      Name of object, relative to location, whose attribute is to be renamed
!! \param old_attr_name Prior attribute name
!! \param new_attr_name New attribute name
!! \param es_id         \fortran_es_id
!! \param hdferr        \fortran_error
!! \param lapl_id       Link access property list identifier
!! \param file          \fortran_file
!! \param func          \fortran_func
!! \param line          \fortran_line
!!
!! See C API: @ref H5Arename_by_name()
!!
  SUBROUTINE h5arename_by_name_async_f(loc_id, obj_name, old_attr_name, new_attr_name, es_id, &
       hdferr, lapl_id, file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: obj_name
    CHARACTER(LEN=*), INTENT(IN) :: old_attr_name
    CHARACTER(LEN=*), INTENT(IN) :: new_attr_name
    INTEGER(HID_T), INTENT(IN)    :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    INTEGER(HID_T) :: lapl_id_default
    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    CHARACTER(LEN=LEN_TRIM(obj_name)     +1,KIND=C_CHAR) :: c_obj_name
    CHARACTER(LEN=LEN_TRIM(old_attr_name)+1,KIND=C_CHAR) :: c_old_attr_name
    CHARACTER(LEN=LEN_TRIM(new_attr_name)+1,KIND=C_CHAR) :: c_new_attr_name

    INTERFACE
       INTEGER FUNCTION H5Arename_by_name_async(file, func, line, loc_id, obj_name, &
            old_attr_name, new_attr_name, lapl_id_default, es_id) &
            BIND(C,NAME='H5Arename_by_name_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: obj_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: old_attr_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: new_attr_name
         INTEGER(HID_T), VALUE :: lapl_id_default
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Arename_by_name_async
    END INTERFACE

    c_obj_name      = TRIM(obj_name)//C_NULL_CHAR
    c_old_attr_name = TRIM(old_attr_name)//C_NULL_CHAR
    c_new_attr_name = TRIM(new_attr_name)//C_NULL_CHAR

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default=lapl_id

    IF (PRESENT(file)) file_default = file
    IF (PRESENT(func)) func_default = func
    IF (PRESENT(line)) line_default = INT(line, C_INT)

    hdferr = 0
    hdferr = H5Arename_by_name_async(file_default, func_default, line_default, &
         loc_id, c_obj_name, c_old_attr_name, c_new_attr_name, lapl_id_default, es_id)

  END SUBROUTINE h5arename_by_name_async_f

!>
!! \ingroup FH5A
!!
!! \brief Opens an attribute for an object specified by object
!!       identifier and attribute name
!!
!! \param obj_id    Identifier for object to which attribute is attached
!! \param attr_name Name of attribute to open
!! \param attr_id   Attribute identifier
!! \param hdferr    \fortran_error
!! \param aapl_id   Attribute access property list
!!
!! See C API: @ref H5Aopen()
!!
  SUBROUTINE h5aopen_f(obj_id, attr_name, attr_id, hdferr, aapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    CHARACTER(LEN=*), INTENT(IN) :: attr_name
    INTEGER(HID_T), INTENT(OUT) :: attr_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: aapl_id

    INTEGER(HID_T) :: aapl_id_default
    CHARACTER(LEN=LEN_TRIM(attr_name)+1,KIND=C_CHAR) :: c_attr_name

    c_attr_name = TRIM(attr_name)//C_NULL_CHAR

    aapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(aapl_id)) aapl_id_default = aapl_id

    attr_id = INT(H5Aopen(obj_id, c_attr_name, aapl_id_default), HID_T)

    hdferr = 0
    IF(attr_id.LT.0) hdferr = -1

  END SUBROUTINE h5aopen_f

!>
!! \ingroup FH5A
!!
!! \brief Asynchronously opens an attribute for an object specified by object identifier and attribute name.
!!
!! \param obj_id    Identifier for object to which attribute is attached
!! \param attr_name Name of attribute to open
!! \param attr_id   Attribute identifier
!! \param es_id     \fortran_es_id
!! \param hdferr    \fortran_error
!! \param aapl_id   Attribute access property list
!! \param file      \fortran_file
!! \param func      \fortran_func
!! \param line      \fortran_line
!!
!! See C API: @ref H5Aopen_async()
!!
  SUBROUTINE h5aopen_async_f(obj_id, attr_name, attr_id, es_id, hdferr, aapl_id, file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id
    CHARACTER(LEN=*), INTENT(IN) :: attr_name
    INTEGER(HID_T), INTENT(OUT) :: attr_id
    INTEGER(HID_T), INTENT(IN) :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: aapl_id
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER         , INTENT(IN) , OPTIONAL :: line

    INTEGER(HID_T) :: aapl_id_default
    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0
    CHARACTER(LEN=LEN_TRIM(attr_name)+1,KIND=C_CHAR) :: c_attr_name

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Aopen_async(file, func, line, &
            obj_id, attr_name, aapl_id_default, es_id) BIND(C,NAME='H5Aopen_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: obj_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: attr_name
         INTEGER(HID_T), VALUE :: aapl_id_default
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Aopen_async
    END INTERFACE

    c_attr_name = TRIM(attr_name)//C_NULL_CHAR

    aapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(aapl_id)) aapl_id_default = aapl_id
    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    attr_id = INT(H5Aopen_async(file_default, func_default, line_default, &
         obj_id, c_attr_name, aapl_id_default, es_id), HID_T)

    hdferr = 0
    IF(attr_id.LT.0) hdferr = -1

  END SUBROUTINE h5aopen_async_f

!>
!! \ingroup FH5A
!!
!! \brief Deletes an attribute from an object according to index order
!!
!! \param loc_id   Location or object identifier; may be dataset or group or named datatype
!! \param obj_name Name of object, relative to location, from which attribute is to be removed
!! \param idx_type Type of index; Possible values are:
!!                 \li H5_INDEX_UNKNOWN_F = -1  - Unknown index type
!!                 \li H5_INDEX_NAME_F          - Index on names
!!                 \li H5_INDEX_CRT_ORDER_F     - Index on creation order
!!                 \li H5_INDEX_N_F             - Number of indices defined
!!
!! \param order    Order in which to iterate over index; Possible values are:
!!                 \li H5_ITER_UNKNOWN_F   - Unknown order
!!                 \li H5_ITER_INC_F       - Increasing order
!!                 \li H5_ITER_DEC_F       - Decreasing order
!!                 \li H5_ITER_NATIVE_F    - No particular order, whatever is fastest
!!                 \li H5_ITER_N_F         - Number of iteration orders
!!
!! \param n        Offset within index
!! \param hdferr   \fortran_error
!! \param lapl_id  Link access property list
!!
!! See C API: @ref H5Adelete_by_idx()
!!
  SUBROUTINE h5adelete_by_idx_f(loc_id, obj_name, idx_type, order, n, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: obj_name
    INTEGER, INTENT(IN) :: idx_type
    INTEGER, INTENT(IN) :: order
    INTEGER(HSIZE_T), INTENT(IN) :: n
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id
    INTEGER(SIZE_T) :: obj_namelen
    INTEGER(HID_T) :: lapl_id_default

    INTERFACE
       INTEGER FUNCTION H5Adelete_by_idx_c(loc_id, obj_name, obj_namelen, idx_type, order, n, lapl_id_default) &
            BIND(C,NAME='h5adelete_by_idx_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: obj_name
         INTEGER, INTENT(IN) :: idx_type
         INTEGER, INTENT(IN) :: order
         INTEGER(HSIZE_T), INTENT(IN) :: n
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(SIZE_T) :: obj_namelen
       END FUNCTION H5Adelete_by_idx_c
    END INTERFACE

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    obj_namelen = LEN(obj_name)
    hdferr = H5Adelete_by_idx_c(loc_id, obj_name, obj_namelen, idx_type, order, n, lapl_id_default)

  END SUBROUTINE h5adelete_by_idx_f

!>
!! \ingroup FH5A
!!
!! \brief Removes an attribute from a specified location
!!
!! \param loc_id    Identifier for object to which attribute is attached
!! \param obj_name  Name of attribute to open
!! \param attr_name Attribute access property list
!! \param lapl_id   Link access property list
!! \param hdferr    \fortran_error
!!
!! See C API: @ref H5Adelete_by_name()
!!
  SUBROUTINE H5Adelete_by_name_f(loc_id, obj_name, attr_name, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: obj_name
    CHARACTER(LEN=*), INTENT(IN) :: attr_name
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id
    INTEGER(SIZE_T) :: attr_namelen
    INTEGER(SIZE_T) :: obj_namelen

    INTEGER(HID_T) :: lapl_id_default

    INTERFACE
       INTEGER FUNCTION H5Adelete_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, lapl_id_default) &
            BIND(C,NAME='h5adelete_by_name_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: obj_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: attr_name
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(SIZE_T) :: attr_namelen
         INTEGER(SIZE_T) :: obj_namelen
       END FUNCTION H5Adelete_by_name_c
    END INTERFACE

    obj_namelen = LEN(obj_name)
    attr_namelen = LEN(attr_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = H5Adelete_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, lapl_id_default)

  END SUBROUTINE h5adelete_by_name_f

!>
!! \ingroup FH5A
!!
!! \brief Opens an existing attribute that is attached to an object specified by location and name.
!!
!! \param loc_id   Location of object to which attribute is attached.
!! \param obj_name Name of object to which attribute is attached, relative to location.
!! \param idx_type Type of index; Possible values are:
!!                 \li H5_INDEX_UNKNOWN_F = -1  - Unknown index type
!!                 \li H5_INDEX_NAME_F          - Index on names
!!                 \li H5_INDEX_CRT_ORDER_F     - Index on creation order
!!                 \li H5_INDEX_N_F             - Number of indices defined
!!
!! \param order    Order in which to iterate over index; Possible values are:
!!                 \li H5_ITER_UNKNOWN_F   - Unknown order
!!                 \li H5_ITER_INC_F       - Increasing order
!!                 \li H5_ITER_DEC_F       - Decreasing order
!!                 \li H5_ITER_NATIVE_F    - No particular order, whatever is fastest
!!                 \li H5_ITER_N_F         - Number of iteration orders
!! \param n        Attribute&apos;s position in index.
!! \param attr_id  Attribute identifier.
!! \param hdferr   \fortran_error
!! \param aapl_id  Attribute access property list.
!! \param lapl_id  Link access property list.
!!
!! See C API: @ref H5Aopen_by_idx()
!!
  SUBROUTINE h5aopen_by_idx_f(loc_id, obj_name, idx_type, order, n, attr_id, hdferr, aapl_id, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: obj_name
    INTEGER, INTENT(IN) :: idx_type
    INTEGER, INTENT(IN) :: order

    INTEGER(HSIZE_T), INTENT(IN) :: n

    INTEGER(HID_T), INTENT(OUT) :: attr_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: aapl_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id
    INTEGER(HID_T) :: aapl_id_default
    INTEGER(HID_T) :: lapl_id_default
    CHARACTER(LEN=LEN_TRIM(obj_name)+1,KIND=C_CHAR) :: c_obj_name

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Aopen_by_idx(loc_id, obj_name, idx_type, order, n, &
            aapl_id_default, lapl_id_default) BIND(C,NAME='H5Aopen_by_idx')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: obj_name
         INTEGER(C_INT), VALUE :: idx_type
         INTEGER(C_INT), VALUE :: order
         INTEGER(HSIZE_T), VALUE :: n
         INTEGER(HID_T), VALUE :: aapl_id_default
         INTEGER(HID_T), VALUE :: lapl_id_default
       END FUNCTION H5Aopen_by_idx
    END INTERFACE

    c_obj_name  = TRIM(obj_name)//C_NULL_CHAR

    aapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(aapl_id)) aapl_id_default = aapl_id
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    attr_id = INT(H5Aopen_by_idx(loc_id, c_obj_name, INT(idx_type, C_INT), INT(order, C_INT), n, &
         aapl_id_default, lapl_id_default), HID_T)

    hdferr = 0
    IF(attr_id.LT.0) hdferr = -1
    
  END SUBROUTINE h5aopen_by_idx_f

!>
!! \ingroup FH5A
!!
!! \brief Asynchronously opens an existing attribute that is attached to an object specified by location and name.
!!
!! \param loc_id   Location of object to which attribute is attached.
!! \param obj_name Name of object to which attribute is attached, relative to location.
!! \param idx_type Type of index; Possible values are:
!!                 \li H5_INDEX_UNKNOWN_F = -1  - Unknown index type
!!                 \li H5_INDEX_NAME_F          - Index on names
!!                 \li H5_INDEX_CRT_ORDER_F     - Index on creation order
!!                 \li H5_INDEX_N_F             - Number of indices defined
!!
!! \param order    Order in which to iterate over index; Possible values are:
!!                 \li H5_ITER_UNKNOWN_F   - Unknown order
!!                 \li H5_ITER_INC_F       - Increasing order
!!                 \li H5_ITER_DEC_F       - Decreasing order
!!                 \li H5_ITER_NATIVE_F    - No particular order, whatever is fastest
!!                 \li H5_ITER_N_F         - Number of iteration orders
!! \param n        Attribute&apos;s position in index.
!! \param attr_id  Attribute identifier.
!! \param es_id    \fortran_es_id
!! \param hdferr   \fortran_error
!! \param aapl_id  Attribute access property list.
!! \param lapl_id  Link access property list.
!! \param file     \fortran_file
!! \param func     \fortran_func
!! \param line     \fortran_line
!!
!! See C API: @ref H5Aopen_by_idx_async()
!!
  SUBROUTINE h5aopen_by_idx_async_f(loc_id, obj_name, idx_type, order, n, attr_id, es_id, hdferr, &
       aapl_id, lapl_id, file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: obj_name
    INTEGER, INTENT(IN) :: idx_type
    INTEGER, INTENT(IN) :: order
    INTEGER(HSIZE_T), INTENT(IN) :: n
    INTEGER(HID_T), INTENT(OUT) :: attr_id
    INTEGER(HID_T), INTENT(IN) :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: aapl_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER         , INTENT(IN) , OPTIONAL :: line

    INTEGER(HID_T) :: aapl_id_default
    INTEGER(HID_T) :: lapl_id_default
    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0
    CHARACTER(LEN=LEN_TRIM(obj_name)+1,KIND=C_CHAR) :: c_obj_name

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Aopen_by_idx_async(file, func, line, &
            loc_id, obj_name, idx_type, order, n, &
            aapl_id_default, lapl_id_default, es_id) BIND(C,NAME='H5Aopen_by_idx_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: obj_name
         INTEGER(C_INT), VALUE :: idx_type
         INTEGER(C_INT), VALUE :: order
         INTEGER(HSIZE_T), VALUE :: n
         INTEGER(HID_T), VALUE :: aapl_id_default
         INTEGER(HID_T), VALUE :: lapl_id_default
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Aopen_by_idx_async
    END INTERFACE

    c_obj_name  = TRIM(obj_name)//C_NULL_CHAR

    aapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(aapl_id)) aapl_id_default = aapl_id
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id
    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    attr_id = INT(H5Aopen_by_idx_async(file_default, func_default, line_default, &
         loc_id, c_obj_name, INT(idx_type, C_INT), INT(order, C_INT), n, &
         aapl_id_default, lapl_id_default, es_id), HID_T)

    hdferr = 0
    IF(attr_id.LT.0) hdferr = -1

  END SUBROUTINE h5aopen_by_idx_async_f

!>
!! \ingroup FH5A
!!
!! \brief Retrieves attribute information, by attribute identifier.
!!
!! \param attr_id        Attribute identifier.
!!                       NOTE: In C it is defined as a structure: H5A_info_t.
!! \param f_corder_valid Indicates whether the creation order data is valid for this attribute.
!! \param corder         Is a positive integer containing the creation order of the attribute.
!! \param cset           Indicates the character set used for the attribute&apos;s name.
!! \param data_size      Indicates the size, in the number of characters, of the attribute.
!! \param hdferr         \fortran_error
!!
!! See C API: @ref H5Aget_info()
!!
  SUBROUTINE h5aget_info_f(attr_id, f_corder_valid, corder, cset, data_size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id
    LOGICAL, INTENT(OUT) :: f_corder_valid
    INTEGER, INTENT(OUT) :: corder
    INTEGER, INTENT(OUT) :: cset
    INTEGER(HSIZE_T), INTENT(OUT) :: data_size
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: corder_valid

    INTERFACE
       INTEGER FUNCTION H5Aget_info_c(attr_id, corder_valid, corder, cset, data_size) BIND(C,NAME='h5aget_info_c')
         IMPORT :: HID_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: attr_id

         INTEGER, INTENT(OUT) :: corder_valid
         INTEGER, INTENT(OUT) :: corder
         INTEGER, INTENT(OUT) :: cset
         INTEGER(HSIZE_T), INTENT(OUT) :: data_size
       END FUNCTION H5Aget_info_c
    END INTERFACE

    hdferr = H5Aget_info_c(attr_id, corder_valid, corder, cset, data_size)

    f_corder_valid =.FALSE.
    IF (corder_valid .EQ. 1) f_corder_valid =.TRUE.


  END SUBROUTINE h5aget_info_f

!>
!! \ingroup FH5A
!!
!! \brief Retrieves attribute information by attribute index position
!!
!! \param loc_id         Location of object to which attribute is attached
!! \param obj_name       Name of object to which attribute is attached, relative to location
!! \param idx_type       Type of index
!! \param order          Index traversal order
!! \param n              Attribute&apos;s position in index
!! \param f_corder_valid Indicates whether the creation order data is valid for this attribute
!! \param corder         Is a positive integer containing the creation order of the attribute
!! \param cset           Indicates the character set used for the attribute&apos;s name
!! \param data_size      Indicates the size, in the number of characters, of the attribute
!! \param hdferr         \fortran_error
!! \param lapl_id        Link access property list
!!
!! See C API: @ref H5Aget_info_by_idx()
!!
  SUBROUTINE h5aget_info_by_idx_f(loc_id, obj_name, idx_type, order, n, &
       f_corder_valid, corder, cset, data_size, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: obj_name
    INTEGER, INTENT(IN) :: idx_type
                                              !    H5_INDEX_N_F           - Number of indices defined
    INTEGER, INTENT(IN) :: order
                                              !    H5_ITER_NATIVE_F   - No particular order, whatever is fastest

    INTEGER(HSIZE_T), INTENT(IN) :: n


    LOGICAL, INTENT(OUT) :: f_corder_valid
    INTEGER, INTENT(OUT) :: corder
    INTEGER, INTENT(OUT) :: cset
    INTEGER(HSIZE_T), INTENT(OUT) :: data_size
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id
    INTEGER :: corder_valid
    INTEGER(SIZE_T)  :: obj_namelen
    INTEGER(HID_T) :: lapl_id_default

    INTERFACE
       INTEGER FUNCTION h5aget_info_by_idx_c(loc_id, obj_name, obj_namelen, idx_type, order, n, lapl_id_default, &
            corder_valid, corder, cset, data_size) BIND(C,NAME='h5aget_info_by_idx_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: obj_name
         INTEGER, INTENT(IN) :: idx_type
         INTEGER, INTENT(IN) :: order
         INTEGER(HSIZE_T), INTENT(IN) :: n
         INTEGER(HID_T) :: lapl_id_default
         INTEGER, INTENT(OUT) :: corder_valid
         INTEGER, INTENT(OUT) :: corder
         INTEGER, INTENT(OUT) :: cset
         INTEGER(HSIZE_T), INTENT(OUT) :: data_size

         INTEGER(SIZE_T)  :: obj_namelen
       END FUNCTION H5Aget_info_by_idx_c
    END INTERFACE

    obj_namelen = LEN(obj_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(present(lapl_id)) lapl_id_default = lapl_id

    hdferr = H5Aget_info_by_idx_c(loc_id, obj_name, obj_namelen, idx_type, order, n, lapl_id_default, &
            corder_valid, corder, cset, data_size)

    f_corder_valid =.FALSE.
    IF (corder_valid .EQ. 1) f_corder_valid =.TRUE.

  END SUBROUTINE h5aget_info_by_idx_f

!>
!! \ingroup FH5A
!!
!! \brief Retrieves attribute information, by attribute name
!!
!! \param loc_id         Location of object to which attribute is attached
!! \param obj_name       Name of object to which attribute is attached, relative to location
!! \param attr_name      Attribute name
!! \param f_corder_valid Indicates whether the creation order data is valid for this attribute
!! \param corder         Is a positive integer containing the creation order of the attribute
!! \param cset           Indicates the character set used for the attribute&apos;s name
!! \param data_size      Indicates the size, in the number of characters, of the attribute
!! \param hdferr         \fortran_error
!! \param lapl_id        Link access property list
!!
!! See C API: @ref H5Aget_info_by_name()
!!
  SUBROUTINE h5aget_info_by_name_f(loc_id, obj_name, attr_name, &
       f_corder_valid, corder, cset, data_size, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: obj_name
    CHARACTER(LEN=*), INTENT(IN) :: attr_name


    LOGICAL, INTENT(OUT) :: f_corder_valid
    INTEGER, INTENT(OUT) :: corder
    INTEGER, INTENT(OUT) :: cset
    INTEGER(HSIZE_T), INTENT(OUT) :: data_size
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id
    INTEGER :: corder_valid
    INTEGER(SIZE_T)  :: obj_namelen
    INTEGER(SIZE_T)  :: attr_namelen
    INTEGER(HID_T) :: lapl_id_default

    INTERFACE
       INTEGER FUNCTION H5Aget_info_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, lapl_id_default, &
            corder_valid, corder, cset, data_size) BIND(C,NAME='h5aget_info_by_name_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: obj_name
         INTEGER(SIZE_T), INTENT(IN) :: obj_namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: attr_name
         INTEGER(SIZE_T), INTENT(IN) :: attr_namelen
         INTEGER(HID_T) :: lapl_id_default
         INTEGER, INTENT(OUT) :: corder_valid
         INTEGER, INTENT(OUT) :: corder
         INTEGER, INTENT(OUT) :: cset
         INTEGER(HSIZE_T), INTENT(OUT) :: data_size

       END FUNCTION H5Aget_info_by_name_c
    END INTERFACE

    obj_namelen = LEN(obj_name)
    attr_namelen = LEN(attr_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = H5Aget_info_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, lapl_id_default, &
            corder_valid, corder, cset, data_size)

    f_corder_valid =.FALSE.
    IF (corder_valid .EQ. 1) f_corder_valid =.TRUE.

  END SUBROUTINE h5aget_info_by_name_f

!>
!! \ingroup FH5A
!!
!! \brief Creates an attribute attached to a specified object
!!
!! \param loc_id    Location or object identifier; may be dataset or group
!! \param obj_name  Name, relative to loc_id, of object that attribute is to be attached to
!! \param attr_name Attribute name
!! \param type_id   Attribute datatype identifier
!! \param space_id  Attribute dataspace identifier
!! \param attr      An attribute identifier
!! \param hdferr    \fortran_error
!! \param acpl_id   Attribute creation property list identifier (Currently not used.)
!! \param aapl_id   Attribute access property list identifier (Currently not used.)
!! \param lapl_id   Link access property list
!!
!! See C API: @ref H5Acreate_by_name()
!!
  SUBROUTINE h5acreate_by_name_f(loc_id, obj_name, attr_name, type_id, space_id, attr, hdferr, &
       acpl_id, aapl_id, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T),   INTENT(IN)  :: loc_id
    CHARACTER(LEN=*), INTENT(IN)  :: obj_name
    CHARACTER(LEN=*), INTENT(IN)  :: attr_name
    INTEGER(HID_T),   INTENT(IN)  :: type_id
    INTEGER(HID_T),   INTENT(IN)  :: space_id
    INTEGER(HID_T),   INTENT(OUT) :: attr
    INTEGER,          INTENT(OUT) :: hdferr

    INTEGER(HID_T),   INTENT(IN), OPTIONAL :: acpl_id
    INTEGER(HID_T),   INTENT(IN), OPTIONAL :: aapl_id
    INTEGER(HID_T),   INTENT(IN), OPTIONAL :: lapl_id

    INTEGER(HID_T) :: acpl_id_default
    INTEGER(HID_T) :: aapl_id_default
    INTEGER(HID_T) :: lapl_id_default
    CHARACTER(LEN=LEN_TRIM(obj_name)+1,KIND=C_CHAR) :: c_obj_name
    CHARACTER(LEN=LEN_TRIM(attr_name)+1,KIND=C_CHAR) :: c_attr_name

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Acreate_by_name(loc_id, obj_name, attr_name, &
            type_id, space_id, acpl_id_default, aapl_id_default, lapl_id_default) &
            BIND(C,NAME='H5Acreate_by_name')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: obj_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: attr_name
         INTEGER(HID_T), VALUE :: type_id
         INTEGER(HID_T), VALUE :: space_id
         INTEGER(HID_T), VALUE :: acpl_id_default
         INTEGER(HID_T), VALUE :: aapl_id_default
         INTEGER(HID_T), VALUE :: lapl_id_default
       END FUNCTION H5Acreate_by_name
    END INTERFACE

    c_obj_name  = TRIM(obj_name)//C_NULL_CHAR
    c_attr_name = TRIM(attr_name)//C_NULL_CHAR

    acpl_id_default = H5P_DEFAULT_F
    aapl_id_default = H5P_DEFAULT_F
    lapl_id_default = H5P_DEFAULT_F

    IF(PRESENT(acpl_id)) acpl_id_default = acpl_id
    IF(PRESENT(aapl_id)) aapl_id_default = aapl_id
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    attr = INT(H5Acreate_by_name(loc_id, c_obj_name, c_attr_name, type_id, space_id, &
         acpl_id_default, aapl_id_default, lapl_id_default), HID_T)

    hdferr = 0
    IF(attr.LT.0) hdferr = -1

  END SUBROUTINE h5acreate_by_name_f

!>
!! \ingroup FH5A
!!
!! \brief Asynchronously creates an attribute attached to a specified object
!!
!! \param loc_id    Location or object identifier; may be dataset or group
!! \param obj_name  Name, relative to loc_id, of object that attribute is to be attached to
!! \param attr_name Attribute name
!! \param type_id   Attribute datatype identifier
!! \param space_id  Attribute dataspace identifier
!! \param attr      An attribute identifier
!! \param es_id     \fortran_es_id
!! \param hdferr    \fortran_error
!! \param acpl_id   Attribute creation property list identifier (Currently not used.)
!! \param aapl_id   Attribute access property list identifier (Currently not used.)
!! \param lapl_id   Link access property list
!! \param file      \fortran_file
!! \param func      \fortran_func
!! \param line      \fortran_line
!!
!! See C API: @ref H5Acreate_by_name_async()
!!
  SUBROUTINE h5acreate_by_name_async_f(loc_id, obj_name, attr_name, type_id, space_id, attr, es_id, hdferr, &
       acpl_id, aapl_id, lapl_id, file, func, line)

    IMPLICIT NONE
    INTEGER(HID_T),   INTENT(IN)  :: loc_id
    CHARACTER(LEN=*), INTENT(IN)  :: obj_name
    CHARACTER(LEN=*), INTENT(IN)  :: attr_name
    INTEGER(HID_T),   INTENT(IN)  :: type_id
    INTEGER(HID_T),   INTENT(IN)  :: space_id
    INTEGER(HID_T),   INTENT(OUT) :: attr
    INTEGER(HID_T),   INTENT(IN)  :: es_id
    INTEGER,          INTENT(OUT) :: hdferr

    INTEGER(HID_T),   INTENT(IN), OPTIONAL :: acpl_id
    INTEGER(HID_T),   INTENT(IN), OPTIONAL :: aapl_id
    INTEGER(HID_T),   INTENT(IN), OPTIONAL :: lapl_id
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    INTEGER(HID_T) :: acpl_id_default
    INTEGER(HID_T) :: aapl_id_default
    INTEGER(HID_T) :: lapl_id_default
    CHARACTER(LEN=LEN_TRIM(obj_name)+1,KIND=C_CHAR) :: c_obj_name
    CHARACTER(LEN=LEN_TRIM(attr_name)+1,KIND=C_CHAR) :: c_attr_name

    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Acreate_by_name_async(file, func, line, loc_id, obj_name, attr_name, &
            type_id, space_id, acpl_id_default, aapl_id_default, lapl_id_default, es_id) &
            BIND(C,NAME='H5Acreate_by_name_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: obj_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: attr_name
         INTEGER(HID_T), VALUE :: type_id
         INTEGER(HID_T), VALUE :: space_id
         INTEGER(HID_T), VALUE :: acpl_id_default
         INTEGER(HID_T), VALUE :: aapl_id_default
         INTEGER(HID_T), VALUE :: lapl_id_default
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Acreate_by_name_async
    END INTERFACE

    c_obj_name  = TRIM(obj_name)//C_NULL_CHAR
    c_attr_name = TRIM(attr_name)//C_NULL_CHAR

    acpl_id_default = H5P_DEFAULT_F
    aapl_id_default = H5P_DEFAULT_F
    lapl_id_default = H5P_DEFAULT_F

    IF(PRESENT(acpl_id)) acpl_id_default = acpl_id
    IF(PRESENT(aapl_id)) aapl_id_default = aapl_id
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id
    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    attr = H5Acreate_by_name_async(file_default, func_default, line_default, &
         loc_id, c_obj_name, c_attr_name, &
         type_id, space_id, acpl_id_default, aapl_id_default, lapl_id_default, es_id)

    hdferr = 0
    IF(attr.LT.0) hdferr = -1

  END SUBROUTINE h5acreate_by_name_async_f

!>
!! \ingroup FH5A
!!
!! \brief Determines whether an attribute with a given name exists on an object
!!
!! \param obj_id      Object identifier
!! \param attr_name   Attribute name
!! \param attr_exists Attribute exists status
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Aexists()
!!
  SUBROUTINE h5aexists_f(obj_id, attr_name, attr_exists, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T),   INTENT(IN) :: obj_id
    CHARACTER(LEN=*), INTENT(IN) :: attr_name
    LOGICAL, INTENT(OUT) :: attr_exists
    INTEGER, INTENT(OUT) :: hdferr

    CHARACTER(LEN=LEN_TRIM(attr_name)+1,KIND=C_CHAR) :: c_attr_name
    INTEGER(C_INT) :: attr_exists_c

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Aexists(obj_id, attr_name) BIND(C,NAME='H5Aexists')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: obj_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: attr_name
       END FUNCTION H5Aexists
    END INTERFACE

    c_attr_name = TRIM(attr_name)//C_NULL_CHAR

    attr_exists_c = H5Aexists(obj_id, c_attr_name)

    attr_exists = .FALSE.
    IF(attr_exists_c.GT.0) attr_exists = .TRUE.

    hdferr = 0
    IF(attr_exists_c.LT.0) hdferr = -1

  END SUBROUTINE h5aexists_f

!>
!! \ingroup FH5A
!!
!! \brief Asynchronously determines whether an attribute with a given name exists on an object
!!
!! \param obj_id      Object identifier
!! \param attr_name   Attribute name
!! \param attr_exists Pointer to attribute exists status. It should be declared INTEGER(C_INT) and initialized
!!                    to zero (false) for portability. It will return one when true. LOGICAL(C_BOOL) is also
!!                    acceptable but may encounter atypical anomalies. It should be initialized to false when used.
!! \param es_id       \fortran_es_id
!! \param hdferr      \fortran_error
!! \param file        \fortran_file
!! \param func        \fortran_func
!! \param line        \fortran_line
!!
!! See C API: @ref H5Aexists_async()
!!
  SUBROUTINE h5aexists_async_f(obj_id, attr_name, attr_exists, es_id, hdferr, file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)  :: obj_id
    CHARACTER(LEN=*), INTENT(IN)  :: attr_name
    TYPE(C_PTR)     , INTENT(IN)  :: attr_exists
    INTEGER(HID_T)  , INTENT(IN)  :: es_id
    INTEGER         , INTENT(OUT) :: hdferr
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    CHARACTER(LEN=LEN_TRIM(attr_name)+1,KIND=C_CHAR) :: c_attr_name
    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Aexists_async(file, func, line, &
            obj_id, attr_name, exists, es_id) BIND(C,NAME='H5Aexists_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: obj_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: attr_name
         TYPE(C_PTR)   , VALUE :: exists
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Aexists_async
    END INTERFACE

    c_attr_name = TRIM(attr_name)//C_NULL_CHAR
    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    hdferr = INT(H5Aexists_async(file_default, func_default, line_default, obj_id, c_attr_name, attr_exists, es_id))

  END SUBROUTINE h5aexists_async_f

!>
!! \ingroup FH5A
!!
!! \brief Determines whether an attribute with a given name exists on an object
!!
!! \param loc_id      Location identifier
!! \param obj_name    Object name either relative to loc_id, absolute from the file&apos;s root group, or &apos;. &apos;(a dot)
!! \param attr_name   Attribute name
!! \param attr_exists Attribute exists status
!! \param hdferr      \fortran_error
!! \param lapl_id     Link access property list identifier
!!
!! See C API: @ref H5Aexists_by_name()
!!
  SUBROUTINE h5aexists_by_name_f(loc_id, obj_name, attr_name, attr_exists, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: obj_name
    CHARACTER(LEN=*), INTENT(IN) :: attr_name
    LOGICAL, INTENT(OUT) :: attr_exists
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id

    INTEGER(C_INT) :: attr_exists_c
    INTEGER(HID_T) :: lapl_id_default
    CHARACTER(LEN=LEN_TRIM(obj_name)+1,KIND=C_CHAR) :: c_obj_name
    CHARACTER(LEN=LEN_TRIM(attr_name)+1,KIND=C_CHAR) :: c_attr_name

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Aexists_by_name(loc_id, obj_name, attr_name, lapl_id_default) &
            BIND(C,NAME='H5Aexists_by_name')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: obj_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: attr_name
         INTEGER(HID_T), VALUE :: lapl_id_default
       END FUNCTION H5Aexists_by_name
    END INTERFACE

    c_obj_name  = TRIM(obj_name)//C_NULL_CHAR
    c_attr_name = TRIM(attr_name)//C_NULL_CHAR

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    attr_exists_c = H5Aexists_by_name(loc_id, c_obj_name, c_attr_name, lapl_id_default)

    attr_exists = .FALSE.
    IF(attr_exists_c.GT.0) attr_exists = .TRUE.

    hdferr = 0
    IF(attr_exists_c.LT.0) hdferr = -1

  END SUBROUTINE h5aexists_by_name_f

!>
!! \ingroup FH5A
!!
!! \brief Asynchronously determines whether an attribute with a given name exists on an object
!!
!! \param loc_id      Location identifier
!! \param obj_name    Object name either relative to loc_id, absolute from the file&apos;s root group, or &apos;. &apos;(a dot)
!! \param attr_name   Attribute name
!! \param attr_exists Pointer to attribute exists status, must be of type LOGICAL(C_BOOL) and initialize to .FALSE.
!! \param es_id       \fortran_es_id
!! \param hdferr      \fortran_error
!! \param lapl_id     Link access property list identifier
!! \param file        \fortran_file
!! \param func        \fortran_func
!! \param line        \fortran_line
!!
!! See C API: @ref H5Aexists_by_name_async()
!!
  SUBROUTINE h5aexists_by_name_async_f(loc_id, obj_name, attr_name, attr_exists, es_id, hdferr, lapl_id, file, func, line)
    IMPLICIT NONE
    INTEGER  (HID_T), INTENT(IN)            :: loc_id
    CHARACTER(LEN=*), INTENT(IN)            :: obj_name
    CHARACTER(LEN=*), INTENT(IN)            :: attr_name
    TYPE(C_PTR)     , INTENT(IN)            :: attr_exists
    INTEGER  (HID_T), INTENT(IN)            :: es_id
    INTEGER         , INTENT(OUT)           :: hdferr
    INTEGER  (HID_T), INTENT(IN) , OPTIONAL :: lapl_id
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER         , INTENT(IN) , OPTIONAL :: line

    INTEGER(HID_T) :: lapl_id_default
    CHARACTER(LEN=LEN_TRIM(obj_name)+1,KIND=C_CHAR) :: c_obj_name
    CHARACTER(LEN=LEN_TRIM(attr_name)+1,KIND=C_CHAR) :: c_attr_name
    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER(C_INT) FUNCTION H5Aexists_by_name_async(file, func, line, &
            loc_id, obj_name, attr_name, exists, lapl_id_default, es_id) &
            BIND(C,NAME='H5Aexists_by_name_async')
         IMPORT :: C_CHAR, C_PTR, C_INT
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: obj_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: attr_name
         TYPE(C_PTR)   , VALUE :: exists
         INTEGER(HID_T), VALUE :: lapl_id_default
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Aexists_by_name_async
    END INTERFACE

    c_obj_name  = TRIM(obj_name)//C_NULL_CHAR
    c_attr_name = TRIM(attr_name)//C_NULL_CHAR

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id
    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    hdferr = INT(H5Aexists_by_name_async(file_default, func_default, line_default, &
         loc_id, c_obj_name, c_attr_name, attr_exists, lapl_id_default, es_id))

  END SUBROUTINE h5aexists_by_name_async_f

!>
!! \ingroup FH5A
!!
!! \brief Opens an attribute for an object by object name and attribute name.
!!
!! \param loc_id    Location from which to find object to which attribute is attached
!! \param obj_name  Object name either relative to loc_id, absolute from the file&apos;s root group, or &apos;.&apos; (a dot)
!! \param attr_name Attribute name
!! \param attr_id   Attribute identifier
!! \param hdferr    \fortran_error
!! \param aapl_id   Attribute access property list (Currently unused; should be passed in as H5P_DEFAULT.)
!! \param lapl_id   Link access property list identifier
!!
!! See C API: @ref H5Aopen_by_name()
!!
  SUBROUTINE h5aopen_by_name_f(loc_id, obj_name, attr_name, attr_id, hdferr, aapl_id, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: obj_name
    CHARACTER(LEN=*), INTENT(IN) :: attr_name
    INTEGER(HID_T), INTENT(OUT) :: attr_id
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: aapl_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id

    INTEGER(HID_T) :: aapl_id_default
    INTEGER(HID_T) :: lapl_id_default
    CHARACTER(LEN=LEN_TRIM(obj_name)+1,KIND=C_CHAR) :: c_obj_name
    CHARACTER(LEN=LEN_TRIM(attr_name)+1,KIND=C_CHAR) :: c_attr_name

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Aopen_by_name(loc_id, obj_name, attr_name, aapl_id_default, lapl_id_default) &
            BIND(C,NAME='H5Aopen_by_name')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: obj_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: attr_name
         INTEGER(HID_T), VALUE :: aapl_id_default
         INTEGER(HID_T), VALUE :: lapl_id_default
       END FUNCTION H5Aopen_by_name
    END INTERFACE

    c_obj_name  = TRIM(obj_name)//C_NULL_CHAR
    c_attr_name = TRIM(attr_name)//C_NULL_CHAR

    aapl_id_default = H5P_DEFAULT_F
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(aapl_id)) aapl_id_default = aapl_id
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    attr_id = INT(H5Aopen_by_name(loc_id, c_obj_name, c_attr_name, aapl_id_default, lapl_id_default), HID_T)

    hdferr = 0
    IF(attr_id.LT.0) hdferr = -1

  END SUBROUTINE h5aopen_by_name_f

!>
!! \ingroup FH5A
!!
!! \brief Asynchronously opens an attribute for an object by object name and attribute name.
!!
!! \param loc_id    Location from which to find object to which attribute is attached
!! \param obj_name  Object name either relative to loc_id, absolute from the file&apos;s root group, or &apos;.&apos; (a dot)
!! \param attr_name Attribute name
!! \param attr_id   Attribute identifier
!! \param es_id     \fortran_es_id
!! \param hdferr    \fortran_error
!! \param aapl_id   Attribute access property list (Currently unused; should be passed in as H5P_DEFAULT.)
!! \param lapl_id   Link access property list identifier
!! \param file      \fortran_file
!! \param func      \fortran_func
!! \param line      \fortran_line
!!
!! See C API: @ref H5Aopen_by_name_async()
!!
  SUBROUTINE h5aopen_by_name_async_f(loc_id, obj_name, attr_name, attr_id, es_id, hdferr, &
       aapl_id, lapl_id, file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: obj_name
    CHARACTER(LEN=*), INTENT(IN) :: attr_name
    INTEGER(HID_T), INTENT(OUT) :: attr_id
    INTEGER(HID_T), INTENT(IN) :: es_id

    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: aapl_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: lapl_id
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    INTEGER(HID_T) :: aapl_id_default
    INTEGER(HID_T) :: lapl_id_default
    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0
    CHARACTER(LEN=LEN_TRIM(obj_name)+1,KIND=C_CHAR) :: c_obj_name
    CHARACTER(LEN=LEN_TRIM(attr_name)+1,KIND=C_CHAR) :: c_attr_name

    INTERFACE
       INTEGER(HID_T) FUNCTION H5Aopen_by_name_async(file, func, line, loc_id, obj_name, attr_name, &
            aapl_id_default, lapl_id_default, es_id) BIND(C,NAME='H5Aopen_by_name_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: obj_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*) :: attr_name
         INTEGER(HID_T), VALUE :: aapl_id_default
         INTEGER(HID_T), VALUE :: lapl_id_default
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Aopen_by_name_async
    END INTERFACE

    c_obj_name  = TRIM(obj_name)//C_NULL_CHAR
    c_attr_name = TRIM(attr_name)//C_NULL_CHAR

    aapl_id_default = H5P_DEFAULT_F
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(aapl_id)) aapl_id_default = aapl_id
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id
    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    attr_id = INT(H5Aopen_by_name_async(file_default, func_default, line_default, &
         loc_id, c_obj_name, c_attr_name, aapl_id_default, lapl_id_default, es_id), HID_T)

    hdferr = 0
    IF(attr_id.LT.0) hdferr = -1

  END SUBROUTINE h5aopen_by_name_async_f

!>
!! \ingroup FH5A
!!
!! \brief Renames an attribute
!!
!! \param loc_id        Location or object identifier; may be dataset or group
!! \param old_attr_name Prior attribute name
!! \param new_attr_name New attribute name
!! \param hdferr        \fortran_error
!!
!! See C API: @ref H5Arename()
!!
  SUBROUTINE h5arename_f(loc_id, old_attr_name, new_attr_name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: old_attr_name
    CHARACTER(LEN=*), INTENT(IN) :: new_attr_name
    INTEGER, INTENT(OUT) :: hdferr

    CHARACTER(LEN=LEN_TRIM(old_attr_name)+1,KIND=C_CHAR) :: c_old_attr_name
    CHARACTER(LEN=LEN_TRIM(new_attr_name)+1,KIND=C_CHAR) :: c_new_attr_name

    INTERFACE
       INTEGER FUNCTION H5Arename(loc_id, old_attr_name, new_attr_name) &
            BIND(C,NAME='H5Arename')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         IMPLICIT NONE
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: old_attr_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: new_attr_name
       END FUNCTION H5Arename
    END INTERFACE

    c_old_attr_name = TRIM(old_attr_name)//C_NULL_CHAR
    c_new_attr_name = TRIM(new_attr_name)//C_NULL_CHAR

    hdferr = H5Arename(loc_id, c_old_attr_name, c_new_attr_name)

  END SUBROUTINE h5arename_f

!>
!! \ingroup FH5A
!!
!! \brief Asynchronously renames an attribute
!!
!! \param loc_id        Location or object identifier; may be dataset or group
!! \param old_attr_name Prior attribute name
!! \param new_attr_name New attribute name
!! \param es_id         \fortran_es_id
!! \param hdferr        \fortran_error
!! \param file          \fortran_file
!! \param func          \fortran_func
!! \param line          \fortran_line
!!
!! See C API: @ref H5Arename_async()
!!
  SUBROUTINE h5arename_async_f(loc_id, old_attr_name, new_attr_name, es_id, hdferr, &
       file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: old_attr_name
    CHARACTER(LEN=*), INTENT(IN) :: new_attr_name
    INTEGER(HID_T), INTENT(IN) :: es_id
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    CHARACTER(LEN=LEN_TRIM(old_attr_name)+1,KIND=C_CHAR) :: c_old_attr_name
    CHARACTER(LEN=LEN_TRIM(new_attr_name)+1,KIND=C_CHAR) :: c_new_attr_name

    INTERFACE
       INTEGER FUNCTION H5Arename_async(file, func, line, loc_id, old_attr_name, new_attr_name, es_id) &
            BIND(C,NAME='H5Arename_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: old_attr_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: new_attr_name
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Arename_async
    END INTERFACE

    c_old_attr_name = TRIM(old_attr_name)//C_NULL_CHAR
    c_new_attr_name = TRIM(new_attr_name)//C_NULL_CHAR

    IF(PRESENT(file)) file_default = file
    IF(PRESENT(func)) func_default = func
    IF(PRESENT(line)) line_default = INT(line, C_INT)

    hdferr = H5Arename_async(file_default, func_default, line_default, &
         loc_id, c_old_attr_name, c_new_attr_name, es_id)

  END SUBROUTINE h5arename_async_f

!>
!! \ingroup FH5A
!!
!! \brief Asynchronously reads an attribute.
!!
!! \param attr_id    Identifier of an attribute to read.
!! \param memtype_id Identifier of the attribute datatype (in memory).
!! \param buf	     Buffer for data to be read.
!! \param es_id      \fortran_es_id
!! \param hdferr     \fortran_error
!! \param file       \fortran_file
!! \param func       \fortran_func
!! \param line       \fortran_line
!!
!! See C API: @ref H5Aread_async()
!!

  SUBROUTINE h5aread_async_f(attr_id, memtype_id, buf, es_id, hdferr, file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: attr_id
    INTEGER(HID_T), INTENT(IN)  :: memtype_id
    TYPE(C_PTR)   , INTENT(IN)  :: buf
    INTEGER(HID_T), INTENT(IN)  :: es_id
    INTEGER       , INTENT(OUT) :: hdferr
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: file
    TYPE(C_PTR), OPTIONAL, INTENT(IN) :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER FUNCTION H5Aread_async(file, func, line, attr_id, memtype_id, buf, es_id) &
            BIND(C,NAME='H5Aread_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: attr_id
         INTEGER(HID_T), VALUE :: memtype_id
         TYPE(C_PTR)   , VALUE :: buf
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Aread_async
    END INTERFACE

    IF (PRESENT(file)) file_default = file
    IF (PRESENT(func)) func_default = func
    IF (PRESENT(line)) line_default = INT(line, C_INT)

    hdferr = H5Aread_async(file_default, func_default, line_default, attr_id, memtype_id, buf, es_id)

  END SUBROUTINE h5aread_async_f

!>
!! \ingroup FH5A
!!
!! \brief Asynchronously writes an attribute.
!!
!! \param attr_id    Identifier of an attribute to read.
!! \param memtype_id Identifier of the attribute datatype (in memory).
!! \param buf	     Data to be written.
!! \param es_id      \fortran_es_id
!! \param hdferr     \fortran_error
!! \param file       \fortran_file
!! \param func       \fortran_func
!! \param line       \fortran_line
!!
!! See C API: @ref H5Awrite_async()
!!

  SUBROUTINE h5awrite_async_f(attr_id, memtype_id, buf, es_id, hdferr, file, func, line)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN)  :: attr_id
    INTEGER(HID_T), INTENT(IN)  :: memtype_id
    TYPE(C_PTR)   , INTENT(IN)  :: buf
    INTEGER(HID_T), INTENT(IN)  :: es_id
    INTEGER       , INTENT(OUT) :: hdferr
    TYPE(C_PTR), OPTIONAL :: file
    TYPE(C_PTR), OPTIONAL :: func
    INTEGER    , INTENT(IN), OPTIONAL :: line

    TYPE(C_PTR) :: file_default = C_NULL_PTR
    TYPE(C_PTR) :: func_default = C_NULL_PTR
    INTEGER(KIND=C_INT) :: line_default = 0

    INTERFACE
       INTEGER FUNCTION H5Awrite_async(file, func, line, attr_id, memtype_id, buf, es_id) &
            BIND(C,NAME='H5Awrite_async')
         IMPORT :: C_CHAR, C_INT, C_PTR
         IMPORT :: HID_T
         IMPLICIT NONE
         TYPE(C_PTR), VALUE :: file
         TYPE(C_PTR), VALUE :: func
         INTEGER(C_INT), VALUE :: line
         INTEGER(HID_T), VALUE :: attr_id
         INTEGER(HID_T), VALUE :: memtype_id
         TYPE(C_PTR)   , VALUE :: buf
         INTEGER(HID_T), VALUE :: es_id
       END FUNCTION H5Awrite_async
    END INTERFACE

    IF (PRESENT(file)) file_default = file
    IF (PRESENT(func)) func_default = func
    IF (PRESENT(line)) line_default = INT(line, C_INT)

    hdferr = H5Awrite_async(file_default, func_default, line_default, attr_id, memtype_id, buf, es_id)

  END SUBROUTINE h5awrite_async_f

#ifdef H5_DOXYGEN


!>
!! \ingroup FH5A
!!
!! \brief Writes data to an attribute.
!!
!! \attention  \fortran_approved
!!
!! \param attr_id     Identifier of an attribute to write.
!! \param memtype_id  Identifier of the attribute datatype (in memory).
!! \param buf	      Data to be written.
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Awrite()
!!
  SUBROUTINE h5awrite_f(attr_id, memtype_id, buf, hdferr)
    INTEGER(HID_T)  , INTENT(IN)  :: attr_id
    INTEGER(HID_T)  , INTENT(IN)  :: memtype_id
    TYPE(C_PTR)     , INTENT(IN)  :: buf
    INTEGER         , INTENT(OUT) :: hdferr
  END SUBROUTINE h5awrite_f

!>
!! \ingroup FH5A
!!
!! \brief Writes data to an attribute.
!!
!! \attention  \fortran_obsolete
!!
!! \param attr_id     Identifier of an attribute to write.
!! \param memtype_id  Identifier of the attribute datatype (in memory).
!! \param buf         Data buffer; may be a scalar or an array.
!! \param dims        Array to hold corresponding dimension sizes of data buffer buf;
!!                    dim(k) has value of the k-th dimension of buffer buf; values are ignored if buf is a scalar.
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Awrite()
!!
  SUBROUTINE h5awrite_f(attr_id, memtype_id, buf, dims, hdferr)
    INTEGER(HID_T)  , INTENT(IN)               :: attr_id
    INTEGER(HID_T)  , INTENT(IN)               :: memtype_id
    TYPE(TYPE)      , INTENT(IN)               :: buf
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER         , INTENT(OUT)              :: hdferr
  END SUBROUTINE h5awrite_f
!>
!! \ingroup FH5A
!!
!! \brief Reads an attribute.
!!
!! \attention  \fortran_approved
!!
!! \param attr_id     Identifier of an attribute to read.
!! \param memtype_id  Identifier of the attribute datatype (in memory).
!! \param buf	      Buffer for data to be read.
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Aread()
!!
  SUBROUTINE h5aread_f(attr_id, memtype_id, buf, hdferr)
    INTEGER(HID_T), INTENT(IN)  :: attr_id
    INTEGER(HID_T), INTENT(IN)  :: memtype_id
    TYPE(C_PTR)   , INTENT(IN)  :: buf
    INTEGER       , INTENT(OUT) :: hdferr
  END SUBROUTINE h5aread_f

!>
!! \ingroup FH5A
!!
!! \brief Reads an attribute.
!!
!! \attention  \fortran_obsolete
!!
!! \param attr_id     Identifier of an attribute to read.
!! \param memtype_id  Identifier of the attribute datatype (in memory).
!! \param buf         Buffer for data to be read.
!! \param dims        Array to hold corresponding dimension sizes of data buffer buf;
!!                    dim(k) has value of the k-th dimension of buffer buf; values are ignored if buf is a scalar.
!! \param hdferr      \fortran_error
!!
!! See C API: @ref H5Aread()
!!
  SUBROUTINE h5aread_f(attr_id, memtype_id, buf, dims, hdferr)
    INTEGER(HID_T)  , INTENT(IN)               :: attr_id
    INTEGER(HID_T)  , INTENT(IN)               :: memtype_id
    TYPE(TYPE)      , INTENT(INOUT)            :: buf
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER         , INTENT(OUT)              :: hdferr
  END SUBROUTINE h5aread_f

#else

  SUBROUTINE h5awrite_char_scalar(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id
    INTEGER(HID_T), INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN) :: buf
    INTEGER, INTENT(OUT) :: hdferr

    CALL H5Awrite_char_scalar_fix(attr_id, memtype_id, buf, LEN(buf), dims, hdferr)

  END SUBROUTINE h5awrite_char_scalar

  SUBROUTINE h5awrite_char_scalar_fix(attr_id, memtype_id, buf, buf_len, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id
    INTEGER(HID_T), INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER, INTENT(IN)  :: buf_len
    CHARACTER(LEN=buf_len), INTENT(IN), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1:1))

    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5awrite_char_scalar_fix

  SUBROUTINE h5awrite_ptr(attr_id, memtype_id, buf, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id
    INTEGER(HID_T), INTENT(IN) :: memtype_id
    TYPE(C_PTR), INTENT(IN), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr

    hdferr = H5Awrite_f_c(attr_id, memtype_id, buf)

  END SUBROUTINE h5awrite_ptr

  SUBROUTINE h5aread_char_scalar(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id
    INTEGER(HID_T), INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT) :: buf
    INTEGER, INTENT(OUT) :: hdferr

    CALL H5Aread_char_scalar_fix(attr_id, memtype_id, buf, LEN(buf), hdferr)

  END SUBROUTINE H5Aread_char_scalar

  SUBROUTINE H5Aread_char_scalar_fix(attr_id, memtype_id, buf, buf_len, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id
    INTEGER(HID_T), INTENT(IN) :: memtype_id
    INTEGER, INTENT(IN)  :: buf_len
    CHARACTER(LEN=buf_len), INTENT(INOUT), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1:1))

    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE h5aread_char_scalar_fix

  SUBROUTINE h5aread_ptr(attr_id, memtype_id, buf, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id
    INTEGER(HID_T), INTENT(IN) :: memtype_id
    TYPE(C_PTR), INTENT(INOUT) :: buf
    INTEGER, INTENT(OUT) :: hdferr

    hdferr = H5Aread_f_c(attr_id, memtype_id, buf)

  END SUBROUTINE h5aread_ptr

#endif

END MODULE H5A


