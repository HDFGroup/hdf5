!****h* ROBODoc/H5A
!
! NAME
!  MODULE H5A
!
! PURPOSE
!  This file contains Fortran interfaces for H5A functions. It includes
!  all the functions that are independent on whether the Fortran 2003 functions
!  are enabled or disabled.
!
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
!   distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
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
!*****

#include <H5config_f.inc>

MODULE H5A

  USE, INTRINSIC :: ISO_C_BINDING

  USE H5GLOBAL

  PRIVATE h5awrite_char_scalar, h5awrite_ptr
  PRIVATE h5aread_char_scalar, h5aread_ptr

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
     INTEGER FUNCTION h5awrite_f_c(attr_id, mem_type_id, buf) BIND(C, NAME='h5awrite_f_c')
       IMPORT :: c_ptr
       IMPORT :: HID_T
       IMPLICIT NONE
       INTEGER(HID_T), INTENT(IN) :: attr_id
       INTEGER(HID_T), INTENT(IN) :: mem_type_id
       TYPE(C_PTR), VALUE :: buf
     END FUNCTION h5awrite_f_c
  END INTERFACE

!  Interface for the function used to pass the C pointer of the buffer
!  to the C H5Aread routine
  INTERFACE
     INTEGER FUNCTION h5aread_f_c(attr_id, mem_type_id, buf) BIND(C, NAME='h5aread_f_c')
       IMPORT :: c_ptr
       IMPORT :: HID_T
       IMPLICIT NONE
       INTEGER(HID_T), INTENT(IN) :: attr_id
       INTEGER(HID_T), INTENT(IN) :: mem_type_id
       TYPE(C_PTR), VALUE :: buf
     END FUNCTION h5aread_f_c
  END INTERFACE

CONTAINS

!
!****s* H5A/h5acreate_f
!
! NAME
!  h5acreate_f
!
! PURPOSE
!  Creates a dataset as an attribute of a group, dataset, or named datatype
!
! INPUTS
!  loc_id 	 - identifier of an object (group, dataset,
!                  or named datatype) attribute is attached to
!  name 	 - attribute name
!  type_id 	 - attribute datatype identifier
!  space_id 	 - attribute dataspace identifier
!
! OUTPUTS
!  attr_id 	 - attribute identifier
!  hdferr 	 - Returns 0 if successful and -1 if fails
! OPTIONAL PARAMETERS
!  acpl_id 	 - Attribute creation property list identifier
!  appl_id 	 - Attribute access property list identifier
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces are added for
!  called C functions (it is needed for Windows
!  port).  February 27, 2001
!
!
! SOURCE
  SUBROUTINE h5acreate_f(loc_id, name, type_id, space_id, attr_id, &
       hdferr, acpl_id, aapl_id )
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id   ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: name   ! Attribute name
    INTEGER(HID_T), INTENT(IN) :: type_id  ! Attribute datatype identifier
    INTEGER(HID_T), INTENT(IN) :: space_id ! Attribute dataspace identifier
    INTEGER(HID_T), INTENT(OUT) :: attr_id ! Attribute identifier
    INTEGER, INTENT(OUT) :: hdferr         ! Error code:
                                           ! 0 on success and -1 on failure
!*****
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: acpl_id ! Attribute creation property list identifier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: aapl_id ! Attribute access property list identifier

    INTEGER(HID_T) :: acpl_id_default
    INTEGER(HID_T) :: aapl_id_default
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name
    INTERFACE
       INTEGER(HID_T) FUNCTION H5Acreate2(loc_id, name, type_id, &
            space_id, acpl_id_default, aapl_id_default) BIND(C,NAME='H5Acreate2')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN), VALUE :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER(HID_T), INTENT(IN), VALUE :: type_id
         INTEGER(HID_T), INTENT(IN), VALUE :: space_id
         INTEGER(HID_T), INTENT(IN), VALUE :: acpl_id_default
         INTEGER(HID_T), INTENT(IN), VALUE :: aapl_id_default
       END FUNCTION H5Acreate2
    END INTERFACE

    acpl_id_default = H5P_DEFAULT_F
    aapl_id_default = H5P_DEFAULT_F
    IF (PRESENT(acpl_id)) acpl_id_default = acpl_id
    IF (PRESENT(aapl_id)) aapl_id_default = aapl_id

    c_name = TRIM(name)//C_NULL_CHAR
    attr_id = h5acreate2(loc_id, c_name, type_id, space_id, &
         acpl_id_default, aapl_id_default)

    hdferr = 0
    IF(attr_id.LT.0) hdferr = -1

  END SUBROUTINE h5acreate_f

!
!****s* H5A/h5aopen_name_f
!
! NAME
!  h5aopen_name_f
!
! PURPOSE
!  Opens an attribute specified by name.
!
! INPUTS
!  obj_id 	 - identifier of a group, dataset, or named
!                  datatype atttribute to be attached to
!  name 	 - attribute name
! OUTPUTS
!  attr_id 	 - attribute identifier
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces are added for
!  called C functions (it is needed for Windows
!  port).  February 27, 2001
!
! SOURCE
  SUBROUTINE H5Aopen_name_f(obj_id, name, attr_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id    ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: name    ! Attribute name
    INTEGER(HID_T), INTENT(OUT) :: attr_id  ! Attribute identifier
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
!*****
    CHARACTER(LEN=LEN_TRIM(name)+1,KIND=C_CHAR) :: c_name

! H5Aopen_name is deprecated
    INTERFACE
       INTEGER(HID_T) FUNCTION H5Aopen(obj_id, name, aapl_id) BIND(C,NAME='H5Aopen')
         IMPORT :: C_CHAR
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN), VALUE :: obj_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER(HID_T), INTENT(IN), VALUE :: aapl_id
       END FUNCTION H5Aopen
    END INTERFACE

    c_name = TRIM(name)//C_NULL_CHAR
    attr_id = H5Aopen(obj_id, c_name, H5P_DEFAULT_F)

    hdferr = 0
    IF(attr_id.LT.0) hdferr = -1

  END SUBROUTINE H5Aopen_name_f
!
!****s* H5A/H5Aopen_idx_f
!
! NAME
!  H5Aopen_idx_f
!
! PURPOSE
!  Opens the attribute specified by its index.
!
! INPUTS
!  obj_id 	 - identifier of a group, dataset, or named
!                  datatype an attribute to be attached to
!  index 	 - index of the attribute to open (zero-based)
! OUTPUTS
!  attr_id 	 - attribute identifier
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces are added for
!  called C functions (it is needed for Windows
!  port).  February 27, 2001
!
! SOURCE
  SUBROUTINE H5Aopen_idx_f(obj_id, index, attr_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id    ! Object identifier
    INTEGER, INTENT(IN) :: index            ! Attribute index
    INTEGER(HID_T), INTENT(OUT) :: attr_id  ! Attribute identifier
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
!*****
!   H5Aopen_idx is deprecated in favor of the function H5Aopen_by_idx.
    INTERFACE
       INTEGER(HID_T) FUNCTION H5Aopen_by_idx(obj_id, index) BIND(C,NAME='H5Aopen_by_idx')
         IMPORT :: HID_T
         IMPORT :: C_INT
         INTEGER(HID_T), INTENT(IN) :: obj_id
         INTEGER(C_INT), INTENT(IN) :: index
       END FUNCTION H5Aopen_by_idx
    END INTERFACE

    attr_id = H5Aopen_by_idx(obj_id, INT(index, C_INT))

    hdferr = 0
    IF(attr_id.LT.0) hdferr = -1

  END SUBROUTINE H5Aopen_idx_f
!
!****s* H5A/H5Aget_space_f
!
! NAME
!  H5Aget_space_f
!
! PURPOSE
!  Gets a copy of the dataspace for an attribute.
!
! INPUTS
!  attr_id 	 - attribute identifier
!
! OUTPUTS
!  space_id 	 - attribite dataspace identifier
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces are added for
!  called C functions (it is needed for Windows
!  port).  February 27, 2001
!
!
! SOURCE
  SUBROUTINE H5Aget_space_f(attr_id, space_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(OUT) :: space_id ! Attribute dataspace identifier
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
!*****
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

  END SUBROUTINE H5Aget_space_f
!
!****s* H5A/H5Aget_type_f
!
! NAME
!  H5Aget_type_f
!
! PURPOSE
!  Gets an attribute datatype.
!
! INPUTS
!  attr_id 	 - attribute identifier
! OUTPUTS
!  type_id 	 - attribute datatype identifier
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces are added for
!  called C functions (it is needed for Windows
!  port).  February 27, 2001
!
! SOURCE
  SUBROUTINE H5Aget_type_f(attr_id, type_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier
    INTEGER(HID_T), INTENT(OUT) :: type_id ! Attribute datatype identifier
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
!*****
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

  END SUBROUTINE H5Aget_type_f
!
!****s* H5A/H5Aget_name_f
!
! NAME
!  H5Aget_name_f
!
! PURPOSE
!  Gets an attribute name.
!
! INPUTS
!  attr_id 	 - attribute identifier
!  size 	 - size of a buffer to read name in
! OUTPUTS
!  buf 	         - buffer to read name in
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces are added for
!  called C functions (it is needed for Windows
!  port).  February 27, 2001
!
!
! SOURCE
  SUBROUTINE h5aget_name_f(attr_id, size, buf, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier
    INTEGER(SIZE_T), INTENT(IN) :: size    ! Buffer size
    CHARACTER(LEN=*), INTENT(INOUT) :: buf ! Buffer to hold attribute name
    INTEGER, INTENT(OUT) :: hdferr ! Error code:
                                   ! name length is successful, -1 if fail
!*****
    INTERFACE
       INTEGER FUNCTION h5aget_name_c(attr_id, size, buf) &
            BIND(C,NAME='h5aget_name_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(SIZE_T), INTENT(IN) :: size
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(OUT) :: buf
       END FUNCTION h5aget_name_c
    END INTERFACE

    hdferr = h5aget_name_c(attr_id, size, buf)
  END SUBROUTINE h5aget_name_f
!
!****s* H5A/H5Aget_name_by_idx_f
!
! NAME
!  H5Aget_name_by_idx_f
!
! PURPOSE
!  Gets an attribute name, by attribute index position.
!
! INPUTS
!  loc_id 	 - Location of object to which attribute is attached
!  obj_name 	 - Name of object to which attribute is attached, relative to location
!  idx_type 	 - Type of index; Possible values are:
!                   H5_INDEX_UNKNOWN_F = -1  - Unknown index type
!                   H5_INDEX_NAME_F 	     - Index on names
!                   H5_INDEX_CRT_ORDER_F     - Index on creation order
!                   H5_INDEX_N_F 	     - Number of indices defined
!
!  order 	 - Order in which to iterate over index; Possible values are:
!                   H5_ITER_UNKNOWN_F 	 - Unknown order
!                   H5_ITER_INC_F 	 - Increasing order
!                   H5_ITER_DEC_F 	 - Decreasing order
!                   H5_ITER_NATIVE_F 	 - No particular order, whatever is fastest
!                   H5_ITER_N_F 	 - Number of iteration orders
!  order 	 - Index traversal order
!  n 	         - Attribute’s position in index
!
! OUTPUTS
!  name 	 - Attribute name
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! OPTIONAL PARAMETERS
!  lapl_id 	 - Link access property list
!  size 	 - Size, in bytes, of attribute name
!
! AUTHOR
!  M. Scot Breitenfeld
!  January, 2008
!
! SOURCE
  SUBROUTINE h5aget_name_by_idx_f(loc_id, obj_name, idx_type, order, &
       n, name, hdferr, size, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id      ! Identifer for object to which attribute is attached
    CHARACTER(LEN=*), INTENT(IN) :: obj_name  ! Name of object, relative to location,
                                              !  from which attribute is to be removed *TEST* check NULL
    INTEGER, INTENT(IN) :: idx_type ! Type of index; Possible values are:
                                    !    H5_INDEX_UNKNOWN_F   - Unknown index type
                                    !    H5_INDEX_NAME_F       - Index on names
                                    !    H5_INDEX_CRT_ORDER_F  - Index on creation order
                                    !    H5_INDEX_N_F 	      - Number of indices defined

    INTEGER, INTENT(IN) :: order    ! Order in which to iterate over index; Possible values are:
                                    !    H5_ITER_UNKNOWN_F   - Unknown order
                                    !    H5_ITER_INC_F      - Increasing order
                                    !    H5_ITER_DEC_F       - Decreasing order
                                    !    H5_ITER_NATIVE_F    - No particular order, whatever is fastest
                                    !    H5_ITER_N_F 	    - Number of iteration orders
    INTEGER(HSIZE_T), INTENT(IN) :: n !  Attribute’s position in index
    CHARACTER(LEN=*), INTENT(OUT) :: name ! Attribute name
    INTEGER, INTENT(OUT) :: hdferr    ! Error code:
                                      ! Returns attribute name size,
                                      ! -1 if fail
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id ! Link access property list
    INTEGER(SIZE_T), OPTIONAL, INTENT(OUT) :: size  ! Indicates the size, in the number of characters, 
                                                    ! of the attribute
!*****
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
!
!****s* H5A/H5Aget_num_attrs_f
!
! NAME
!  H5Aget_num_attrs_f
!
! PURPOSE
!  Determines the number of attributes attached to an object.
!
! INPUTS
!  obj_id 	 - object (group, dataset, or named datatype)
!  identifier
! OUTPUTS
!  attr_num 	 - number of attributes attached to the object
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces are added for
!  called C functions (it is needed for Windows
!  port).  February 27, 2001
!
! SOURCE
  SUBROUTINE h5aget_num_attrs_f(obj_id, attr_num, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id  ! Object identifier
    INTEGER, INTENT(OUT) :: attr_num      ! Number of attributes of the object
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****

    INTERFACE
       INTEGER FUNCTION h5aget_num_attrs_c(obj_id, attr_num) BIND(C,name='h5aget_num_attrs_c')
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN) :: obj_id
         INTEGER, INTENT(OUT) :: attr_num
       END FUNCTION h5aget_num_attrs_c
    END INTERFACE

    hdferr = h5aget_num_attrs_c(obj_id, attr_num)
  END SUBROUTINE h5aget_num_attrs_f

!
!****s* H5A/H5Adelete_f
!
! NAME
!  H5Adelete_f
!
! PURPOSE
!  Deletes an attribute of an object (group, dataset or
!  named datatype)
!
! INPUTS
!  obj_id 	 - object identifier
!  name 	 - attribute name
! OUTPUTS
!
!  hdferr 	 - Returns 0 if successful and -1 if fails
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces are added for
!  called C functions (it is needed for Windows
!  port).  February 27, 2001
!
! SOURCE
  SUBROUTINE H5Adelete_f(obj_id, name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id  ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Attribute name
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTEGER(SIZE_T) :: namelen

    INTERFACE
       INTEGER FUNCTION H5Adelete_c(obj_id, name, namelen) BIND(C,NAME='h5adelete_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         INTEGER(HID_T), INTENT(IN) :: obj_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER(SIZE_T) :: namelen
       END FUNCTION H5Adelete_c
    END INTERFACE

    namelen = LEN(name)
    hdferr = H5Adelete_c(obj_id, name, namelen)
  END SUBROUTINE H5Adelete_f

!
!****s* H5A/H5Aclose_f
!
! NAME
!  H5Aclose_f
!
! PURPOSE
!  Closes the specified attribute.
!
! INPUTS
!  attr_id  - attribute identifier
! OUTPUTS
!
!  hdferr   - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces are added for
!  called C functions (it is needed for Windows
!  port).  February 27, 2001
! SOURCE

  SUBROUTINE H5Aclose_f(attr_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
!*****

    INTERFACE
       INTEGER FUNCTION H5Aclose(attr_id) BIND(C, NAME='H5Aclose')
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN), VALUE :: attr_id
       END FUNCTION H5Aclose
    END INTERFACE

    hdferr = INT(H5Aclose(attr_id))
  END SUBROUTINE H5Aclose_f

!
!****s* H5A/H5Aget_storage_size_f
!
! NAME
!  H5Aget_storage_size_f
!
! PURPOSE
!  Returns the amount of storage required for an attribute.
!
! INPUTS
!  attr_id 	 - attribute identifier
! OUTPUTS
!  size 	 - attribute storage size
!  hdferr 	 - Returns 0 if successful and -1 if fails
! AUTHOR
!  M. Scot Breitenfeld
!  January, 2008
!
! SOURCE
  SUBROUTINE H5Aget_storage_size_f(attr_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier
    INTEGER(HSIZE_T), INTENT(OUT) :: size  ! Attribute storage requirement
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
!*****

    INTERFACE
       INTEGER(HSIZE_T) FUNCTION H5Aget_storage_size(attr_id) BIND(C,NAME='H5Aget_storage_size')
         IMPORT :: HID_T, HSIZE_T
         INTEGER(HID_T),  INTENT(IN), VALUE :: attr_id
       END FUNCTION H5Aget_storage_size
    END INTERFACE

    size = H5Aget_storage_size(attr_id)

    hdferr = 0
    IF(size.LT.0) hdferr = -1

  END SUBROUTINE H5Aget_storage_size_f

!
!****s* H5A/H5Aget_create_plist_f
!
! NAME
!  H5Aget_create_plist_f
!
! PURPOSE
!  Gets an attribute creation property list identifier
!
! INPUTS
!  attr_id 	    - Identifier of the attribute
! OUTPUTS
!  creation_prop_id - Identifier for the attribute’s creation property
!  hdferr 	    - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  M. Scot Breitenfeld
!  January, 2008
!
! SOURCE
  SUBROUTINE H5Aget_create_plist_f(attr_id, creation_prop_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id  ! Identifier of the attribute
    INTEGER(HID_T), INTENT(OUT) :: creation_prop_id   ! Identifier for the attribute’s creation property
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****
    INTERFACE
       INTEGER(HID_T) FUNCTION H5Aget_create_plist(attr_id) BIND(C,NAME='H5Aget_create_plist')
         IMPORT :: HID_T
         INTEGER(HID_T), INTENT(IN), VALUE :: attr_id
       END FUNCTION H5Aget_create_plist
    END INTERFACE

    creation_prop_id = H5Aget_create_plist(attr_id)

    hdferr = 0
    IF(creation_prop_id.LT.0) hdferr = -1

  END SUBROUTINE H5Aget_create_plist_f

!
!****s* H5A/H5Arename_by_name_f
!
! NAME
!  H5Arename_by_name_f
!
! PURPOSE
!  Renames an attribute
!
! INPUTS
!  loc_id 	 - Location or object identifier; may be dataset or group
!  obj_name 	 - Name of object, relative to location,
!                  whose attribute is to be renamed
!  old_attr_name - Prior attribute name
!  new_attr_name - New attribute name
!  lapl_id 	 - Link access property list identifier
!
! OUTPUTS
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  M. Scot Breitenfeld
!  January, 2008
!
! SOURCE
  SUBROUTINE H5Arename_by_name_f(loc_id, obj_name, old_attr_name, new_attr_name, &
        hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id    ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: obj_name  ! Name of object, relative to location,
                                              !  whose attribute is to be renamed
    CHARACTER(LEN=*), INTENT(IN) :: old_attr_name ! Prior attribute name
    CHARACTER(LEN=*), INTENT(IN) :: new_attr_name ! New attribute name

    INTEGER, INTENT(OUT) :: hdferr       ! Error code:
                                         ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id ! Link access property list identifier
!*****
    INTEGER(HID_T) :: lapl_id_default
    INTEGER(SIZE_T) :: obj_namelen
    INTEGER(SIZE_T) :: old_attr_namelen
    INTEGER(SIZE_T) :: new_attr_namelen

    INTERFACE
       INTEGER FUNCTION H5Arename_by_name_c(loc_id, obj_name, obj_namelen, &
            old_attr_name, old_attr_namelen, new_attr_name, new_attr_namelen, &
            lapl_id_default) BIND(C,NAME='h5arename_by_name_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: obj_name
         INTEGER(SIZE_T) :: obj_namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: old_attr_name
         INTEGER(SIZE_T) :: old_attr_namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: new_attr_name
         INTEGER(SIZE_T) :: new_attr_namelen
         INTEGER(HID_T) :: lapl_id_default

       END FUNCTION H5Arename_by_name_c
    END INTERFACE

    obj_namelen = LEN(obj_name)
    old_attr_namelen = LEN(old_attr_name)
    new_attr_namelen = LEN(new_attr_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default=lapl_id

    hdferr = H5Arename_by_name_c(loc_id, obj_name, obj_namelen, &
         old_attr_name, old_attr_namelen, new_attr_name, new_attr_namelen, &
         lapl_id_default)

  END SUBROUTINE H5Arename_by_name_f

!
!****s* H5A/H5Aopen_f
!
! NAME
!  H5Aopen_f
!
! PURPOSE
!  Opens an attribute for an object specified by object
!  identifier and attribute name
!
! INPUTS
!  obj_id 	 - Identifer for object to which attribute is attached
!  attr_name 	 - Name of attribute to open
! OUTPUTS
!  attr_id 	 - attribute identifier

! OPTIONAL PARAMETERS
!  aapl_id 	 - Attribute access property list
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  M. Scot Breitenfeld
!  January, 2008
!
! SOURCE
  SUBROUTINE H5Aopen_f(obj_id, attr_name, attr_id, hdferr, aapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id      ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: attr_name ! Attribute name
    INTEGER(HID_T), INTENT(OUT) :: attr_id    ! Attribute identifier
    INTEGER, INTENT(OUT) :: hdferr            ! Error code
                                              !   Success:  0
                                              !   Failure: -1
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: aapl_id     ! Attribute access property list
!*****
    INTEGER(HID_T) :: aapl_id_default

    INTEGER(SIZE_T) :: attr_namelen

    INTERFACE
       INTEGER FUNCTION H5Aopen_c(obj_id, attr_name, attr_namelen, aapl_id_default, attr_id) &
            BIND(C,NAME='h5aopen_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         INTEGER(HID_T), INTENT(IN) :: obj_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: attr_name
         INTEGER(HID_T) :: aapl_id_default
         INTEGER(SIZE_T) :: attr_namelen
         INTEGER(HID_T), INTENT(OUT) :: attr_id
       END FUNCTION H5Aopen_c
    END INTERFACE

    attr_namelen = LEN(attr_name)

    aapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(aapl_id)) aapl_id_default = aapl_id

    hdferr = H5Aopen_c(obj_id, attr_name, attr_namelen, aapl_id_default, attr_id)

  END SUBROUTINE H5Aopen_f

!
!****s* H5A/H5Adelete_by_idx_f
!
! NAME
!  H5Adelete_by_idx_f
!
! PURPOSE
!  Deletes an attribute from an object according to index order
!
! INPUTS
!  loc_id 	 - Location or object identifier; may be dataset or group
!  obj_name 	 - Name of object, relative to location, from which attribute is to be removed
!  idx_type 	 - Type of index; Possible values are:
!                   H5_INDEX_UNKNOWN_F = -1  - Unknown index type
!                   H5_INDEX_NAME_F 	     - Index on names
!                   H5_INDEX_CRT_ORDER_F     - Index on creation order
!                   H5_INDEX_N_F 	     - Number of indices defined
!
!  order 	 - Order in which to iterate over index; Possible values are:
!                   H5_ITER_UNKNOWN_F 	 - Unknown order
!                   H5_ITER_INC_F 	 - Increasing order
!                   H5_ITER_DEC_F 	 - Decreasing order
!                   H5_ITER_NATIVE_F 	 - No particular order, whatever is fastest
!                   H5_ITER_N_F 	 - Number of iteration orders
!
!  n 	         - Offset within index
! OUTPUTS
!  hdferr 	 - Returns 0 if successful and -1 if fails
! OPTIONAL PARAMETERS
!  lapl_id 	 - Link access property list
!
! AUTHOR
!  M. Scot Breitenfeld
!  January, 2008
!
! SOURCE
  SUBROUTINE H5Adelete_by_idx_f(loc_id, obj_name, idx_type, order, n, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id      ! Identifer for object to which attribute is attached
    CHARACTER(LEN=*), INTENT(IN) :: obj_name  ! Name of object, relative to location,
                                              !  from which attribute is to be removed
    INTEGER, INTENT(IN) :: idx_type           ! Type of index; Possible values are:
                                              !    H5_INDEX_UNKNOWN_F   - Unknown index type
                                              !    H5_INDEX_NAME_F      - Index on names
                                              !    H5_INDEX_CRT_ORDER_F - Index on creation order
                                              !    H5_INDEX_N_F	      - Number of indices defined

    INTEGER, INTENT(IN) :: order              ! Order in which to iterate over index; Possible values are:
                                              !    H5_ITER_UNKNOWN_F  - Unknown order
                                              !    H5_ITER_INC_F      - Increasing order
                                              !    H5_ITER_DEC_F      - Decreasing order
                                              !    H5_ITER_NATIVE_F   - No particular order, whatever is fastest
                                              !    H5_ITER_N_F	    - Number of iteration orders
    INTEGER(HSIZE_T), INTENT(IN) :: n         ! Offset within index
    INTEGER, INTENT(OUT) :: hdferr         ! Error code:
                                           ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id ! Link access property list
!*****
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

  END SUBROUTINE H5Adelete_by_idx_f

!
!****s* H5A/H5Adelete_by_name_f
!
! NAME
!  H5Adelete_by_name_f
!
! PURPOSE
!  Removes an attribute from a specified location
!
! INPUTS
!  loc_id 	 - Identifer for object to which attribute is attached
!  obj_name 	 - Name of attribute to open
!  attr_name 	 - Attribute access property list
!  lapl_id 	 - Link access property list
! OUTPUTS
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  M. Scot Breitenfeld
!  January, 2008
!
! SOURCE
  SUBROUTINE H5Adelete_by_name_f(loc_id, obj_name, attr_name, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id      ! Identifer for object to which attribute is attached
    CHARACTER(LEN=*), INTENT(IN) :: obj_name  ! Name of object, relative to location,
                                              !  from which attribute is to be removed
    CHARACTER(LEN=*), INTENT(IN) :: attr_name ! Name of attribute to delete
    INTEGER, INTENT(OUT) :: hdferr            ! Error code:
                                              ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id ! Link access property list
!*****
    INTEGER(SIZE_T) :: attr_namelen
    INTEGER(SIZE_T) :: obj_namelen

    INTEGER(HID_T) :: lapl_id_default

    INTERFACE
       INTEGER FUNCTION H5Adelete_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, lapl_id_default) &
            BIND(C,NAME='h5adelete_by_name_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
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

  END SUBROUTINE H5Adelete_by_name_f

!
!****s* H5A/H5Aopen_by_idx_f
!
! NAME
!  H5Aopen_by_idx_f
!
! PURPOSE
!  Opens an existing attribute that is attached to an object specified by location and name
!
! INPUTS
!  loc_id 	 - Location of object to which attribute is attached
!  obj_name 	 - Name of object to which attribute is attached, relative to location
!  idx_type 	 - Type of index
!  order 	 - Index traversal order
!  n 	         - Attribute’s position in index
! OUTPUTS
!  hdferr 	 - Returns 0 if successful and -1 if fails
! OPTIONAL PARAMETERS
!  aapl_id 	 - Attribute access property list
!  lapl_id 	 - Link access property list
!
! AUTHOR
!  M. Scot Breitenfeld
!  January, 2008
!
! SOURCE
  SUBROUTINE H5Aopen_by_idx_f(loc_id, obj_name, idx_type, order, n, attr_id, hdferr, aapl_id, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id      ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: obj_name  ! Name of object to which attribute is attached
    INTEGER, INTENT(IN) :: idx_type           ! Type of index; Possible values are:
                                              !    H5_INDEX_UNKNOWN_F   - Unknown index type
                                              !    H5_INDEX_NAME_F      - Index on names
                                              !    H5_INDEX_CRT_ORDER_F - Index on creation order
                                              !    H5_INDEX_N_F	      - Number of indices defined
    INTEGER, INTENT(IN) :: order              ! Order in which to iterate over index; Possible values are:
                                              !    H5_ITER_UNKNOWN_F  - Unknown order
                                              !    H5_ITER_INC_F      - Increasing order
                                              !    H5_ITER_DEC_F      - Decreasing order
                                              !    H5_ITER_NATIVE_F   - No particular order, whatever is fastest

    INTEGER(HSIZE_T), INTENT(IN) :: n       ! Attribute’s position in index

    INTEGER(HID_T), INTENT(OUT) :: attr_id  ! Attribute identifier
    INTEGER, INTENT(OUT) :: hdferr          ! Error code:
                                            ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: aapl_id  ! Attribute access property list
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id  ! Link access property list
!*****
    INTEGER(SIZE_T) :: obj_namelen
    INTEGER(HID_T) :: aapl_id_default
    INTEGER(HID_T) :: lapl_id_default

    INTERFACE
       INTEGER FUNCTION H5Aopen_by_idx_c(loc_id, obj_name, obj_namelen, idx_type, order, n, &
            aapl_id_default, lapl_id_default, attr_id) BIND(C,NAME='h5aopen_by_idx_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: obj_name
         INTEGER, INTENT(IN) :: idx_type
         INTEGER, INTENT(IN) :: order
         INTEGER(HSIZE_T), INTENT(IN) :: n
         INTEGER(HID_T) :: aapl_id_default
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(SIZE_T) :: obj_namelen
         INTEGER(HID_T), INTENT(OUT) :: attr_id  ! Attribute identifier
       END FUNCTION H5Aopen_by_idx_c
    END INTERFACE

    obj_namelen = LEN(obj_name)

    aapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(aapl_id)) aapl_id_default = aapl_id
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = H5Aopen_by_idx_c(loc_id, obj_name, obj_namelen, idx_type, order, n, &
         aapl_id_default, lapl_id_default, attr_id)

  END SUBROUTINE H5Aopen_by_idx_f

!
!****s* H5A/H5Aget_info_f
!
! NAME
!  H5Aget_info_f
!
! PURPOSE
!  Retrieves attribute information, by attribute identifier
!
! INPUTS
!  attr_id 	 - attribute identifier
!
! OUTPUTS
!  NOTE: In C it is defined as a structure: H5A_info_t
!
!  corder_valid  - indicates whether the creation order data is valid for this attribute
!  corder 	 - is a positive integer containing the creation order of the attribute
!  cset 	 - indicates the character set used for the attribute’s name
!  data_size 	 - indicates the size, in the number of characters, of the attribute
!  hdferr 	 - Returns 0 if successful and -1 if fails
! AUTHOR
!  M. Scot Breitenfeld
!  January, 2008
! SOURCE
  SUBROUTINE H5Aget_info_f(attr_id, f_corder_valid, corder, cset, data_size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier

    LOGICAL, INTENT(OUT) :: f_corder_valid ! Indicates whether the creation order data is valid for this attribute
    INTEGER, INTENT(OUT) :: corder ! Is a positive integer containing the creation order of the attribute
    INTEGER, INTENT(OUT) :: cset   ! Indicates the character set used for the attribute’s name
    INTEGER(HSIZE_T), INTENT(OUT) :: data_size ! Indicates the size, in the number of characters, of the attribute
    INTEGER, INTENT(OUT) :: hdferr       ! Error code:
                                         ! 0 on success and -1 on failure
!*****
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


  END SUBROUTINE H5Aget_info_f

!
!****s* H5A/H5Aget_info_by_idx_f
!
! NAME
!  H5Aget_info_by_idx_f
!
! PURPOSE
!  Retrieves attribute information, by attribute index position
!
! INPUTS
!  loc_id 	 - Location of object to which attribute is attached
!  obj_name 	 - Name of object to which attribute is attached, relative to location
!  idx_type 	 - Type of index
!  order 	 - Index traversal order
!  n 	         - Attribute’s position in index
!
! OUTPUTS  NOTE: In C it is defined as a structure: H5A_info_t
!  corder_valid  - indicates whether the creation order data is valid for this attribute
!  corder 	 - is a positive integer containing the creation order of the attribute
!  cset 	 - indicates the character set used for the attribute’s name
!  data_size 	 - indicates the size, in the number of characters, of the attribute
!  hdferr 	 - Returns 0 if successful and -1 if fails
! OPTIONAL PARAMETERS
!  lapl_id 	 - Link access property list
!
! AUTHOR
!  M. Scot Breitenfeld
!  January, 2008
!
! SOURCE
  SUBROUTINE H5Aget_info_by_idx_f(loc_id, obj_name, idx_type, order, n, &
       f_corder_valid, corder, cset, data_size, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id      ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: obj_name  ! Name of object to which attribute is attached
    INTEGER, INTENT(IN) :: idx_type           ! Type of index; Possible values are:
                                              !    H5_INDEX_UNKNOWN_F   - Unknown index type
                                              !    H5_INDEX_NAME_F      - Index on names
                                              !    H5_INDEX_CRT_ORDER_F - Index on creation order
                                              !    H5_INDEX_N_F	      - Number of indices defined
    INTEGER, INTENT(IN) :: order              ! Order in which to iterate over index; Possible values are:
                                              !    H5_ITER_UNKNOWN_F  - Unknown order
                                              !    H5_ITER_INC_F      - Increasing order
                                              !    H5_ITER_DEC_F      - Decreasing order
                                              !    H5_ITER_NATIVE_F   - No particular order, whatever is fastest

    INTEGER(HSIZE_T), INTENT(IN) :: n         ! Attribute’s position in index


    LOGICAL, INTENT(OUT) :: f_corder_valid ! Indicates whether the creation order data is valid for this attribute
    INTEGER, INTENT(OUT) :: corder ! Is a positive integer containing the creation order of the attribute
    INTEGER, INTENT(OUT) :: cset   ! Indicates the character set used for the attribute’s name
    INTEGER(HSIZE_T), INTENT(OUT) :: data_size ! Indicates the size, in the number of characters, of the attribute
    INTEGER, INTENT(OUT) :: hdferr       ! Error code:
                                         ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id  ! Link access property list
!*****
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

  END SUBROUTINE H5Aget_info_by_idx_f

!
!****s* H5A/H5Aget_info_by_name_f
!
! NAME
!  H5Aget_info_by_name_f
!
! PURPOSE
!  Retrieves attribute information, by attribute name
!
! INPUTS
!  loc_id 	 - Location of object to which attribute is attached
!  obj_name 	 - Name of object to which attribute is attached, relative to location
!  attr_name 	 - Attribute name
!
! OUTPUTS  NOTE: In C it is defined as a structure: H5A_info_t
!  corder_valid  - indicates whether the creation order data is valid for this attribute
!  corder 	 - is a positive integer containing the creation order of the attribute
!  cset 	 - indicates the character set used for the attribute’s name
!  data_size 	 - indicates the size, in the number of characters, of the attribute
!  hdferr 	 - Returns 0 if successful and -1 if fails
! OPTIONAL PARAMETERS
!  lapl_id 	 - Link access property list
!
! AUTHOR
!  M. Scot Breitenfeld
!  January, 2008
!
! SOURCE
  SUBROUTINE H5Aget_info_by_name_f(loc_id, obj_name, attr_name, &
       f_corder_valid, corder, cset, data_size, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id    ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: obj_name ! Name of object to which attribute is attached
    CHARACTER(LEN=*), INTENT(IN) :: attr_name ! Attribute name


    LOGICAL, INTENT(OUT) :: f_corder_valid ! Indicates whether the creation order data is valid for this attribute
    INTEGER, INTENT(OUT) :: corder ! Is a positive integer containing the creation order of the attribute
    INTEGER, INTENT(OUT) :: cset ! Indicates the character set used for the attribute’s name
    INTEGER(HSIZE_T), INTENT(OUT) :: data_size   ! Indicates the size, in the number of characters, of the attribute
    INTEGER, INTENT(OUT) :: hdferr         ! Error code:
                                           ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id  ! Link access property list
!*****
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

  END SUBROUTINE H5Aget_info_by_name_f

!
!****s* H5A/H5Acreate_by_name_f
!
! NAME
!  H5Acreate_by_name_f
!
! PURPOSE
!  Creates an attribute attached to a specified object
!
! INPUTS
!  loc_id 	 - Location or object identifier; may be dataset or group
!  obj_name 	 - Name, relative to loc_id, of object that attribute is to be attached to
!  attr_name 	 - Attribute name
!  type_id 	 - Attribute datatype identifier
!  space_id 	 - Attribute dataspace identifier
!
! OUTPUTS
!  attr 	 - an attribute identifier
!  hdferr 	 - Returns 0 if successful and -1 if fails
! OPTIONAL PARAMETERS
!  acpl_id 	 - Attribute creation property list identifier (Currently not used.)
!  aapl_id 	 - Attribute access property list identifier (Currently not used.)
!  lapl_id 	 - Link access property list
!
! AUTHOR
!  M. Scot Breitenfeld
!  February, 2008
! SOURCE
  SUBROUTINE H5Acreate_by_name_f(loc_id, obj_name, attr_name, type_id, space_id, attr, hdferr, &
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
!*****
    INTEGER(SIZE_T)  :: obj_namelen
    INTEGER(SIZE_T)  :: attr_namelen

    INTEGER(HID_T) :: acpl_id_default
    INTEGER(HID_T) :: aapl_id_default
    INTEGER(HID_T) :: lapl_id_default

    INTERFACE
       INTEGER FUNCTION H5Acreate_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, &
            type_id, space_id, acpl_id_default, aapl_id_default, lapl_id_default, attr) &
            BIND(C,NAME='h5acreate_by_name_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: obj_name
         INTEGER(SIZE_T), INTENT(IN) :: obj_namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: attr_name
         INTEGER(SIZE_T), INTENT(IN) :: attr_namelen
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(HID_T) :: acpl_id_default
         INTEGER(HID_T) :: aapl_id_default
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(HID_T), INTENT(OUT) :: attr

       END FUNCTION H5Acreate_by_name_c
    END INTERFACE

    obj_namelen = LEN(obj_name)
    attr_namelen = LEN(attr_name)

    acpl_id_default = H5P_DEFAULT_F
    aapl_id_default = H5P_DEFAULT_F
    lapl_id_default = H5P_DEFAULT_F

    IF(PRESENT(acpl_id)) acpl_id_default = acpl_id
    IF(PRESENT(aapl_id)) aapl_id_default = aapl_id
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = H5Acreate_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, &
            type_id, space_id, acpl_id_default, aapl_id_default, lapl_id_default, attr)
  END SUBROUTINE H5Acreate_by_name_f

!
!****s* H5A/H5Aexists_f
!
! NAME
!  H5Aexists_f
!
! PURPOSE
!  Determines whether an attribute with a given name exists on an object
!
! INPUTS
!  obj_id 	 - Object identifier
!  attr_name 	 - Attribute name
!
! OUTPUTS
!  attr_exists 	 - attribute exists status
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  M. Scot Breitenfeld
!  February, 2008
!
! SOURCE
  SUBROUTINE H5Aexists_f(obj_id, attr_name, attr_exists, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id      ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: attr_name ! Attribute name
    LOGICAL, INTENT(OUT) :: attr_exists  ! .TRUE. if exists, .FALSE. otherwise
    INTEGER, INTENT(OUT) :: hdferr       ! Error code:
                                         ! 0 on success and -1 on failure
!*****
    INTEGER(HID_T) :: attr_exists_c
    INTEGER(SIZE_T) :: attr_namelen

    INTERFACE
       INTEGER FUNCTION H5Aexists_c(obj_id, attr_name, attr_namelen, attr_exists_c) BIND(C,NAME='h5aexists_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: obj_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: attr_name
         INTEGER(SIZE_T) :: attr_namelen
         INTEGER(HID_T) :: attr_exists_c
       END FUNCTION H5Aexists_c
    END INTERFACE

    attr_namelen = LEN(attr_name)

    hdferr = H5Aexists_c(obj_id, attr_name, attr_namelen, attr_exists_c)

    attr_exists = .FALSE.
    IF(attr_exists_c.GT.0) attr_exists = .TRUE.

  END SUBROUTINE H5Aexists_f

!
!****s* H5A/H5Aexists_by_name_f
!
! NAME
!  H5Aexists_by_name_f
!
! PURPOSE
!  Determines whether an attribute with a given name exists on an object
!
! INPUTS
!  loc_id 	 - Location identifier
!  obj_name 	 - Object name either relative to loc_id, absolute from the file’s root group, or '.' (a dot)
!  attr_name 	 - Attribute name
!
! OUTPUTS
!  attr_exists 	 - attribute exists status
!  hdferr 	 - Returns 0 if successful and -1 if fails
! OPTIONAL PARAMETERS
!  lapl_id 	 - Link access property list identifier
!
! AUTHOR
!  M. Scot Breitenfeld
!  February, 2008
!
! SOURCE
  SUBROUTINE H5Aexists_by_name_f(loc_id, obj_name, attr_name, attr_exists, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id     ! Location identifier
    CHARACTER(LEN=*), INTENT(IN) :: obj_name ! Object name either relative to loc_id,
                                             ! absolute from the file’s root group, or '.'
    CHARACTER(LEN=*), INTENT(IN) :: attr_name ! Attribute name
    LOGICAL, INTENT(OUT) :: attr_exists ! .TRUE. if exists, .FALSE. otherwise
    INTEGER, INTENT(OUT) :: hdferr      ! Error code:
                                        ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id ! Link access property list identifier
!*****
    INTEGER :: attr_exists_c
    INTEGER(SIZE_T)  :: obj_namelen
    INTEGER(SIZE_T)  :: attr_namelen

    INTEGER(HID_T) :: lapl_id_default

    INTERFACE
       INTEGER FUNCTION H5Aexists_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, &
            lapl_id_default, attr_exists_c) BIND(C,NAME='h5aexists_by_name_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: obj_name
         INTEGER(SIZE_T), INTENT(IN) :: obj_namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: attr_name
         INTEGER(SIZE_T), INTENT(IN) :: attr_namelen
         INTEGER(HID_T), INTENT(IN) :: lapl_id_default
         INTEGER, INTENT(OUT) :: attr_exists_c
       END FUNCTION H5Aexists_by_name_c
    END INTERFACE

    attr_namelen = LEN(attr_name)
    obj_namelen = LEN(obj_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = H5Aexists_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, lapl_id_default, attr_exists_c)

    attr_exists = .FALSE.
    IF(attr_exists_c.GT.0) attr_exists = .TRUE.

  END SUBROUTINE H5Aexists_by_name_f
!
!****s* H5A/H5Aopen_by_name_f
!
! NAME
!  H5Aopen_by_name_f
!
! PURPOSE
!  Opens an attribute for an object by object name and attribute name.
!
! INPUTS
!  loc_id 	 - Location from which to find object to which attribute is attached
!  obj_name 	 - Object name either relative to loc_id, absolute from the file’s root group, or '.' (a dot)
!  attr_name 	 - Attribute name
!
! OUTPUTS
!  attr_id 	 - attribute identifier
!  hdferr 	 - Returns 0 if successful and -1 if fails
! OPTIONAL PARAMETERS
!  aapl_id 	 - Attribute access property list (Currently unused; should be passed in as H5P_DEFAULT.)
!  lapl_id 	 - Link access property list identifier
!
! AUTHOR
!  M. Scot Breitenfeld
!  February, 2008
! SOURCE
  SUBROUTINE H5Aopen_by_name_f(loc_id, obj_name, attr_name, attr_id, hdferr, aapl_id, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id    ! Location identifier
    CHARACTER(LEN=*), INTENT(IN) :: obj_name ! Object name either relative to loc_id,
                                             ! absolute from the file’s root group, or '.'
    CHARACTER(LEN=*), INTENT(IN) :: attr_name ! Attribute name
    INTEGER(HID_T), INTENT(OUT) :: attr_id ! Attribute identifier
    INTEGER, INTENT(OUT) :: hdferr         ! Error code:
                                           ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: aapl_id ! Attribute access property list
                                                    ! (Currently unused; should be passed in as H5P_DEFAULT_F)
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id ! Link access property list identifier
!*****
    INTEGER(HID_T) :: aapl_id_default
    INTEGER(HID_T) :: lapl_id_default

    INTEGER(SIZE_T) :: obj_namelen
    INTEGER(SIZE_T) :: attr_namelen

    INTERFACE
       INTEGER FUNCTION H5Aopen_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, &
            aapl_id_default, lapl_id_default, attr_id) BIND(C,NAME='h5aopen_by_name_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: obj_name
         INTEGER(SIZE_T), INTENT(IN) :: obj_namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: attr_name
         INTEGER(SIZE_T), INTENT(IN) :: attr_namelen
         INTEGER(HID_T) :: aapl_id_default
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(HID_T), INTENT(OUT) :: attr_id
       END FUNCTION H5Aopen_by_name_c
    END INTERFACE

    attr_namelen = LEN(attr_name)
    obj_namelen = LEN(obj_name)

    aapl_id_default = H5P_DEFAULT_F
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(aapl_id)) aapl_id_default = aapl_id
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = H5Aopen_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, &
         aapl_id_default, lapl_id_default, attr_id)

  END SUBROUTINE H5Aopen_by_name_f

!
!****s* H5A/H5Arename_f
!
! NAME
!  H5Arename_f
!
! PURPOSE
!  Renames an attribute
!
! INPUTS
!  loc_id 	 - Location or object identifier; may be dataset or group
!  old_attr_name - Prior attribute name
!  new_attr_name - New attribute name
!
! OUTPUTS
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  M. Scot Breitenfeld
!  January, 2008
!
! HISTORY
!  N/A
!
!

! SOURCE
  SUBROUTINE H5Arename_f(loc_id, old_attr_name, new_attr_name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id    ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: old_attr_name ! Prior attribute name
    CHARACTER(LEN=*), INTENT(IN) :: new_attr_name ! New attribute name
    INTEGER, INTENT(OUT) :: hdferr       ! Error code:
                                         ! 0 on success and -1 on failure
!*****
    INTEGER(SIZE_T) :: old_attr_namelen
    INTEGER(SIZE_T) :: new_attr_namelen

    INTERFACE
       INTEGER FUNCTION H5Arename_c(loc_id, &
            old_attr_name, old_attr_namelen, new_attr_name, new_attr_namelen) BIND(C,NAME='h5arename_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: old_attr_name
         INTEGER(SIZE_T) :: old_attr_namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: new_attr_name
         INTEGER(SIZE_T) :: new_attr_namelen
       END FUNCTION H5Arename_c
    END INTERFACE

    old_attr_namelen = LEN(old_attr_name)
    new_attr_namelen = LEN(new_attr_name)

    hdferr = H5Arename_c(loc_id, &
         old_attr_name, old_attr_namelen, new_attr_name, new_attr_namelen)

  END SUBROUTINE H5Arename_f

  SUBROUTINE H5Awrite_char_scalar(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id               ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id            ! Attribute datatype
                                                        !  identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims  ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(IN) :: buf                 ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr                      ! Error code

    CALL H5Awrite_char_scalar_fix(attr_id, memtype_id, buf, LEN(buf), dims, hdferr)

  END SUBROUTINE H5Awrite_char_scalar

  SUBROUTINE H5Awrite_char_scalar_fix(attr_id, memtype_id, buf, buf_len, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id               ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id            ! Attribute datatype
                                                        !  identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims  ! Array to story buf dimension sizes
    INTEGER, INTENT(IN)  :: buf_len
    CHARACTER(LEN=buf_len), INTENT(IN), TARGET :: buf   ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr                      ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1:1))

    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE H5Awrite_char_scalar_fix


!****s* H5A (F03)/H5Awrite_f_F03
!
! NAME
!  H5Awrite_f_F03
!
! PURPOSE
!  Writes an attribute.
!
! Inputs:
!  attr_id     - Attribute identifier
!  memtype_id  - Attribute datatype identifier  (in memory)
!  buf 	       - Data buffer; may be a scalar or an array
!
! Outputs:
!  hdferr      - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces are added for
!  called C functions (it is needed for Windows
!  port).  February 27, 2001
!
! NOTES
!  This function is overloaded to write INTEGER,
!  REAL, REAL(KIND=C_DOUBLE) and CHARACTER buffers
!  up to 7 dimensions.
!
! Fortran2003 Interface:
!!  SUBROUTINE H5Awrite_f(attr_id, memtype_id, buf, hdferr) 
!!    INTEGER(HID_T)  , INTENT(IN)  :: attr_id
!!    INTEGER(HID_T)  , INTENT(IN)  :: memtype_id
!!    TYPE(C_PTR)     , INTENT(IN)  :: buf
!!    INTEGER         , INTENT(OUT) :: hdferr
!*****

  SUBROUTINE H5Awrite_ptr(attr_id, mem_type_id, buf, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id     ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    TYPE(C_PTR), INTENT(IN), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr            ! Error code

    hdferr = H5Awrite_f_c(attr_id, mem_type_id, buf)

  END SUBROUTINE H5Awrite_ptr

  SUBROUTINE H5Aread_char_scalar(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims ! Array to story buf dimension sizes
    CHARACTER(LEN=*), INTENT(INOUT) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr         ! Error code

    CALL H5Aread_char_scalar_fix(attr_id, memtype_id, buf, LEN(buf), hdferr)

  END SUBROUTINE H5Aread_char_scalar

  SUBROUTINE H5Aread_char_scalar_fix(attr_id, memtype_id, buf, buf_len, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                             ! identifier  (in memory)
    INTEGER, INTENT(IN)  :: buf_len
    CHARACTER(LEN=buf_len), INTENT(INOUT), TARGET :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
    TYPE(C_PTR) :: f_ptr

    f_ptr = C_LOC(buf(1:1))

    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)

  END SUBROUTINE H5Aread_char_scalar_fix

!****s* H5A (F03)/H5Aread_f_F03
!
! NAME
!  H5Aread_f_F03
!
! PURPOSE
!  Reads an attribute.
!
! Inputs:
!  attr_id     - Attribute identifier
!  memtype_id  - Attribute datatype identifier  (in memory)
!
! Outputs:
!  buf 	       - Data buffer; may be a scalar or an array
!  hdferr      - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces are added for
!  called C functions (it is needed for Windows
!  port).  February 27, 2001
!
!  dims parameter was added to make code portable;
!  Aprile 4, 2001
!
!  Changed buf intent to INOUT to be consistant
!  with how the C functions handles it. The pg
!  compiler will return 0 if a buf value is not set.
!  February, 2008
!
! NOTES
!  This function is overloaded to write INTEGER,
!  REAL, REAL(KIND=C_DOUBLE) and CHARACTER buffers
!  up to 7 dimensions.
! Fortran2003 Interface:
!!  SUBROUTINE H5Aread_f(attr_id, memtype_id, buf, hdferr) 
!!    INTEGER(HID_T)  , INTENT(IN)    :: attr_id
!!    INTEGER(HID_T)  , INTENT(IN)    :: memtype_id
!!    TYPE(C_PTR)     , INTENT(INOUT) :: buf
!!    INTEGER         , INTENT(OUT)   :: hdferr
!*****

  SUBROUTINE H5Aread_ptr(attr_id, mem_type_id, buf, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id     ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    TYPE(C_PTR), INTENT(INOUT), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr            ! Error code

    hdferr = H5Aread_f_c(attr_id, mem_type_id, buf)

  END SUBROUTINE H5Aread_ptr

END MODULE H5A


