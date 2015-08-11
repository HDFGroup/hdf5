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
!   the files COPYING and Copyright.html.  COPYING can be found at the root   *
!   of the source code distribution tree; Copyright.html can be found at the  *
!   root level of an installed copy of the electronic HDF5 document set and   *
!   is linked from the top-level documents page.  It can also be found at     *
!   http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
!   access to either file, you may request a copy from help@hdfgroup.org.     *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! NOTES
!                         *** IMPORTANT ***
!  If you add a new H5A function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!
!*****

MODULE H5A

  USE H5GLOBAL
!
!  On Windows there are no big (integer*8) integers, so overloading
!  for bug #670 does not work. I have to use DEC compilation directives to make
!  Windows DEC Visual Fortran and OSF compilers happy and do right things.
!  05/01/02 EP
!

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
    INTEGER(SIZE_T) :: namelen
    INTERFACE
       INTEGER FUNCTION h5acreate_c(loc_id, name, namelen, type_id, &
            space_id, acpl_id_default, aapl_id_default, attr_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ACREATE_C'::h5acreate_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER(SIZE_T) :: namelen
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(HID_T) :: acpl_id_default
         INTEGER(HID_T) :: aapl_id_default
         INTEGER(HID_T), INTENT(OUT) :: attr_id
       END FUNCTION h5acreate_c
    END INTERFACE

    acpl_id_default = H5P_DEFAULT_F
    aapl_id_default = H5P_DEFAULT_F
    namelen = LEN(name)
    IF (PRESENT(acpl_id)) acpl_id_default = acpl_id
    IF (PRESENT(aapl_id)) aapl_id_default = aapl_id

    hdferr = h5acreate_c(loc_id, name, namelen, type_id, space_id, &
         acpl_id_default, aapl_id_default, attr_id)

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
  SUBROUTINE h5aopen_name_f(obj_id, name, attr_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id    ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: name    ! Attribute name
    INTEGER(HID_T), INTENT(OUT) :: attr_id  ! Attribute identifier
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
!*****
    INTEGER(SIZE_T) :: namelen

    INTERFACE
       INTEGER FUNCTION h5aopen_name_c(obj_id, name, namelen, attr_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AOPEN_NAME_C'::h5aopen_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: obj_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER(SIZE_T) :: namelen
         INTEGER(HID_T), INTENT(OUT) :: attr_id
       END FUNCTION h5aopen_name_c
    END INTERFACE

    namelen = LEN(name)
    hdferr = h5aopen_name_c(obj_id, name, namelen, attr_id)
  END SUBROUTINE h5aopen_name_f
!
!****s* H5A/h5aopen_idx_f
!
! NAME
!  h5aopen_idx_f
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
  SUBROUTINE h5aopen_idx_f(obj_id, index, attr_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id    ! Object identifier
    INTEGER, INTENT(IN) :: index            ! Attribute index
    INTEGER(HID_T), INTENT(OUT) :: attr_id  ! Attribute identifier
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
!*****

    INTERFACE
       INTEGER FUNCTION h5aopen_idx_c(obj_id, index, attr_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AOPEN_IDX_C'::h5aopen_idx_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: obj_id
         INTEGER, INTENT(IN) :: index
         INTEGER(HID_T), INTENT(OUT) :: attr_id
       END FUNCTION h5aopen_idx_c
    END INTERFACE

    hdferr = h5aopen_idx_c(obj_id, index, attr_id)
  END SUBROUTINE h5aopen_idx_f
!
!****s* H5A/h5aget_space_f
!
! NAME
!  h5aget_space_f
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
  SUBROUTINE h5aget_space_f(attr_id, space_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(OUT) :: space_id ! Attribute dataspace identifier
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5aget_space_c(attr_id, space_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AGET_SPACE_C'::h5aget_space_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(OUT) :: space_id
       END FUNCTION h5aget_space_c
    END INTERFACE
    
    hdferr = h5aget_space_c(attr_id, space_id)
  END SUBROUTINE h5aget_space_f
!
!****s* H5A/h5aget_type_f
!
! NAME
!  h5aget_type_f
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
  SUBROUTINE h5aget_type_f(attr_id, type_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier
    INTEGER(HID_T), INTENT(OUT) :: type_id ! Attribute datatype identifier
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5aget_type_c(attr_id, type_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AGET_TYPE_C'::h5aget_type_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(OUT) :: type_id
       END FUNCTION h5aget_type_c
    END INTERFACE
    
    hdferr = h5aget_type_c(attr_id, type_id)
  END SUBROUTINE h5aget_type_f
!
!****s* H5A/h5aget_name_f
!
! NAME
!  h5aget_name_f
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
       INTEGER FUNCTION h5aget_name_c(attr_id, size, buf)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AGET_NAME_C'::h5aget_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(SIZE_T), INTENT(IN) :: size
         CHARACTER(LEN=*), INTENT(OUT) :: buf
       END FUNCTION h5aget_name_c
    END INTERFACE

    hdferr = h5aget_name_c(attr_id, size, buf)
  END SUBROUTINE h5aget_name_f

!
!****s* H5A/h5aget_name_by_idx_f
!
! NAME
!  h5aget_name_by_idx_f
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
            n, name, size_default, lapl_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AGET_NAME_BY_IDX_C'::h5aget_name_by_idx_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: obj_name, name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         INTEGER, INTENT(IN) :: idx_type
         INTEGER, INTENT(IN) :: order
         INTEGER(HSIZE_T), INTENT(IN) :: n

         CHARACTER(LEN=*), INTENT(OUT) :: name
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
!****s* H5A/h5aget_num_attrs_f
!
! NAME
!  h5aget_num_attrs_f
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
       INTEGER FUNCTION h5aget_num_attrs_c(obj_id, attr_num)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AGET_NUM_ATTRS_C'::h5aget_num_attrs_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: obj_id
         INTEGER, INTENT(OUT) :: attr_num
       END FUNCTION h5aget_num_attrs_c
    END INTERFACE

    hdferr = h5aget_num_attrs_c(obj_id, attr_num)
  END SUBROUTINE h5aget_num_attrs_f

!
!****s* H5A/h5adelete_f
!
! NAME
!  h5adelete_f
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
  SUBROUTINE h5adelete_f(obj_id, name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id  ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Attribute name
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTEGER(SIZE_T) :: namelen

    INTERFACE
       INTEGER FUNCTION h5adelete_c(obj_id, name, namelen)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ADELETE_C'::h5adelete_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: obj_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER(SIZE_T) :: namelen
       END FUNCTION h5adelete_c
    END INTERFACE

    namelen = LEN(name)
    hdferr = h5adelete_c(obj_id, name, namelen)
  END SUBROUTINE h5adelete_f

!
!****s* H5A/h5aclose_f
!
! NAME
!  h5aclose_f
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
  SUBROUTINE h5aclose_f(attr_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
!*****

    INTERFACE
       INTEGER FUNCTION h5aclose_c(attr_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ACLOSE_C'::h5aclose_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: attr_id
       END FUNCTION h5aclose_c
    END INTERFACE

    hdferr = h5aclose_c(attr_id)
  END SUBROUTINE h5aclose_f

!
!****s* H5A/h5aget_storage_size_f
!
! NAME
!  h5aget_storage_size_f
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
  SUBROUTINE h5aget_storage_size_f(attr_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier
    INTEGER(HSIZE_T), INTENT(OUT) :: size  ! Attribute storage requirement
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
!*****

    INTERFACE
       INTEGER FUNCTION h5aget_storage_size_c(attr_id, size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AGET_STORAGE_SIZE_C'::h5aget_storage_size_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HSIZE_T), INTENT(OUT) :: size
       END FUNCTION h5aget_storage_size_c
    END INTERFACE

    hdferr = h5aget_storage_size_c(attr_id, size)
  END SUBROUTINE h5aget_storage_size_f

!
!****s* H5A/h5aget_create_plist_f
!
! NAME
!  h5aget_create_plist_f
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
  SUBROUTINE h5aget_create_plist_f(attr_id, creation_prop_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id  ! Identifier of the attribute
    INTEGER(HID_T), INTENT(OUT) :: creation_prop_id   ! Identifier for the attribute’s creation property
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
                                         ! 0 on success and -1 on failure
!*****

    INTERFACE
       INTEGER FUNCTION h5aget_create_plist_c(attr_id, creation_prop_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AGET_CREATE_PLIST_C'::h5aget_create_plist_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(OUT) :: creation_prop_id
       END FUNCTION h5aget_create_plist_c
    END INTERFACE

    hdferr = h5aget_create_plist_c(attr_id, creation_prop_id)
  END SUBROUTINE h5aget_create_plist_f

!
!****s* H5A/h5arename_by_name_f
!
! NAME
!  h5arename_by_name_f
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
  SUBROUTINE h5arename_by_name_f(loc_id, obj_name, old_attr_name, new_attr_name, &
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
       INTEGER FUNCTION h5arename_by_name_c(loc_id, obj_name, obj_namelen, &
            old_attr_name, old_attr_namelen, new_attr_name, new_attr_namelen, &
            lapl_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ARENAME_BY_NAME_C'::h5arename_by_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: obj_name, old_attr_name, new_attr_name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         INTEGER(SIZE_T) :: obj_namelen
         CHARACTER(LEN=*), INTENT(IN) :: old_attr_name
         INTEGER(SIZE_T) :: old_attr_namelen
         CHARACTER(LEN=*), INTENT(IN) :: new_attr_name
         INTEGER(SIZE_T) :: new_attr_namelen
         INTEGER(HID_T) :: lapl_id_default

       END FUNCTION h5arename_by_name_c
    END INTERFACE

    obj_namelen = LEN(obj_name)
    old_attr_namelen = LEN(old_attr_name)
    new_attr_namelen = LEN(new_attr_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default=lapl_id

    hdferr = h5arename_by_name_c(loc_id, obj_name, obj_namelen, &
         old_attr_name, old_attr_namelen, new_attr_name, new_attr_namelen, &
         lapl_id_default)

  END SUBROUTINE h5arename_by_name_f

!
!****s* H5A/h5aopen_f
!
! NAME
!  h5aopen_f
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
  SUBROUTINE h5aopen_f(obj_id, attr_name, attr_id, hdferr, aapl_id)
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
       INTEGER FUNCTION h5aopen_c(obj_id, attr_name, attr_namelen, aapl_id_default, attr_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AOPEN_C'::h5aopen_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: attr_name
         INTEGER(HID_T), INTENT(IN) :: obj_id
         CHARACTER(LEN=*), INTENT(IN) :: attr_name
         INTEGER(HID_T) :: aapl_id_default
         INTEGER(SIZE_T) :: attr_namelen
         INTEGER(HID_T), INTENT(OUT) :: attr_id
       END FUNCTION h5aopen_c
    END INTERFACE

    attr_namelen = LEN(attr_name)

    aapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(aapl_id)) aapl_id_default = aapl_id

    hdferr = h5aopen_c(obj_id, attr_name, attr_namelen, aapl_id_default, attr_id)

  END SUBROUTINE h5aopen_f

!
!****s* H5A/h5adelete_by_idx_f
!
! NAME
!  h5adelete_by_idx_f
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
  SUBROUTINE h5adelete_by_idx_f(loc_id, obj_name, idx_type, order, n, hdferr, lapl_id)
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
       INTEGER FUNCTION h5adelete_by_idx_c(loc_id, obj_name, obj_namelen, idx_type, order, n, lapl_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ADELETE_BY_IDX_C'::h5adelete_by_idx_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: obj_name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         INTEGER, INTENT(IN) :: idx_type
         INTEGER, INTENT(IN) :: order
         INTEGER(HSIZE_T), INTENT(IN) :: n
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(SIZE_T) :: obj_namelen
       END FUNCTION h5adelete_by_idx_c
    END INTERFACE

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    obj_namelen = LEN(obj_name)
    hdferr = h5adelete_by_idx_c(loc_id, obj_name, obj_namelen, idx_type, order, n, lapl_id_default)

  END SUBROUTINE h5adelete_by_idx_f

!
!****s* H5A/h5adelete_by_name_f
!
! NAME
!  h5adelete_by_name_f
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
  SUBROUTINE h5adelete_by_name_f(loc_id, obj_name, attr_name, hdferr, lapl_id)
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
       INTEGER FUNCTION h5adelete_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, lapl_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ADELETE_BY_NAME_C'::h5adelete_by_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: obj_name, attr_name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         CHARACTER(LEN=*), INTENT(IN) :: attr_name
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(SIZE_T) :: attr_namelen
         INTEGER(SIZE_T) :: obj_namelen
       END FUNCTION h5adelete_by_name_c
    END INTERFACE

    obj_namelen = LEN(obj_name)
    attr_namelen = LEN(attr_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5adelete_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, lapl_id_default)

  END SUBROUTINE h5adelete_by_name_f

!
!****s* H5A/h5aopen_by_idx_f
!
! NAME
!  h5aopen_by_idx_f
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
  SUBROUTINE h5aopen_by_idx_f(loc_id, obj_name, idx_type, order, n, attr_id, hdferr, aapl_id, lapl_id)
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
       INTEGER FUNCTION h5aopen_by_idx_c(loc_id, obj_name, obj_namelen, idx_type, order, n, &
            aapl_id_default, lapl_id_default, attr_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AOPEN_BY_IDX_C'::h5aopen_by_idx_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: obj_name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         INTEGER, INTENT(IN) :: idx_type
         INTEGER, INTENT(IN) :: order
         INTEGER(HSIZE_T), INTENT(IN) :: n
         INTEGER(HID_T) :: aapl_id_default
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(SIZE_T) :: obj_namelen
         INTEGER(HID_T), INTENT(OUT) :: attr_id  ! Attribute identifier
       END FUNCTION h5aopen_by_idx_c
    END INTERFACE

    obj_namelen = LEN(obj_name)

    aapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(aapl_id)) aapl_id_default = aapl_id
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5aopen_by_idx_c(loc_id, obj_name, obj_namelen, idx_type, order, n, &
         aapl_id_default, lapl_id_default, attr_id)

  END SUBROUTINE h5aopen_by_idx_f

!
!****s* H5A/h5aget_info_f
!
! NAME
!  h5aget_info_f
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
  SUBROUTINE h5aget_info_f(attr_id, f_corder_valid, corder, cset, data_size, hdferr)
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
       INTEGER FUNCTION h5aget_info_c(attr_id, corder_valid, corder, cset, data_size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AGET_INFO_C'::h5aget_info_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: attr_id

         INTEGER, INTENT(OUT) :: corder_valid
         INTEGER, INTENT(OUT) :: corder
         INTEGER, INTENT(OUT) :: cset
         INTEGER(HSIZE_T), INTENT(OUT) :: data_size
       END FUNCTION h5aget_info_c
    END INTERFACE

    hdferr = h5aget_info_c(attr_id, corder_valid, corder, cset, data_size)

    f_corder_valid =.FALSE.
    IF (corder_valid .EQ. 1) f_corder_valid =.TRUE.


  END SUBROUTINE h5aget_info_f

!
!****s* H5A/h5aget_info_by_idx_f
!
! NAME
!  h5aget_info_by_idx_f
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
  SUBROUTINE h5aget_info_by_idx_f(loc_id, obj_name, idx_type, order, n, &
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
            corder_valid, corder, cset, data_size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AGET_INFO_BY_IDX_C'::h5aget_info_by_idx_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: obj_name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         INTEGER, INTENT(IN) :: idx_type
         INTEGER, INTENT(IN) :: order
         INTEGER(HSIZE_T), INTENT(IN) :: n
         INTEGER(HID_T) :: lapl_id_default
         INTEGER, INTENT(OUT) :: corder_valid
         INTEGER, INTENT(OUT) :: corder
         INTEGER, INTENT(OUT) :: cset
         INTEGER(HSIZE_T), INTENT(OUT) :: data_size

         INTEGER(SIZE_T)  :: obj_namelen
       END FUNCTION h5aget_info_by_idx_c
    END INTERFACE

    obj_namelen = LEN(obj_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(present(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5aget_info_by_idx_c(loc_id, obj_name, obj_namelen, idx_type, order, n, lapl_id_default, &
            corder_valid, corder, cset, data_size)

    f_corder_valid =.FALSE.
    IF (corder_valid .EQ. 1) f_corder_valid =.TRUE.

  END SUBROUTINE h5aget_info_by_idx_f

!
!****s* H5A/h5aget_info_by_name_f
!
! NAME
!  h5aget_info_by_name_f
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
  SUBROUTINE h5aget_info_by_name_f(loc_id, obj_name, attr_name, &
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
       INTEGER FUNCTION h5aget_info_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, lapl_id_default, &
            corder_valid, corder, cset, data_size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AGET_INFO_BY_NAME_C'::h5aget_info_by_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: obj_name, attr_name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         INTEGER(SIZE_T), INTENT(IN) :: obj_namelen
         CHARACTER(LEN=*), INTENT(IN) :: attr_name
         INTEGER(SIZE_T), INTENT(IN) :: attr_namelen
         INTEGER(HID_T) :: lapl_id_default
         INTEGER, INTENT(OUT) :: corder_valid
         INTEGER, INTENT(OUT) :: corder
         INTEGER, INTENT(OUT) :: cset
         INTEGER(HSIZE_T), INTENT(OUT) :: data_size

       END FUNCTION h5aget_info_by_name_c
    END INTERFACE

    obj_namelen = LEN(obj_name)
    attr_namelen = LEN(attr_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5aget_info_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, lapl_id_default, &
            corder_valid, corder, cset, data_size)

    f_corder_valid =.FALSE.
    IF (corder_valid .EQ. 1) f_corder_valid =.TRUE.

  END SUBROUTINE h5aget_info_by_name_f

!
!****s* H5A/h5acreate_by_name_f
!
! NAME
!  h5acreate_by_name_f
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
!*****
    INTEGER(SIZE_T)  :: obj_namelen
    INTEGER(SIZE_T)  :: attr_namelen

    INTEGER(HID_T) :: acpl_id_default
    INTEGER(HID_T) :: aapl_id_default
    INTEGER(HID_T) :: lapl_id_default

    INTERFACE
       INTEGER FUNCTION h5acreate_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, &
            type_id, space_id, acpl_id_default, aapl_id_default, lapl_id_default, attr)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ACREATE_BY_NAME_C'::h5acreate_by_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: obj_name, attr_name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         INTEGER(SIZE_T), INTENT(IN) :: obj_namelen
         CHARACTER(LEN=*), INTENT(IN) :: attr_name
         INTEGER(SIZE_T), INTENT(IN) :: attr_namelen
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(HID_T) :: acpl_id_default
         INTEGER(HID_T) :: aapl_id_default
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(HID_T), INTENT(OUT) :: attr

       END FUNCTION h5acreate_by_name_c
    END INTERFACE

    obj_namelen = LEN(obj_name)
    attr_namelen = LEN(attr_name)

    acpl_id_default = H5P_DEFAULT_F
    aapl_id_default = H5P_DEFAULT_F
    lapl_id_default = H5P_DEFAULT_F

    IF(PRESENT(acpl_id)) acpl_id_default = acpl_id
    IF(PRESENT(aapl_id)) aapl_id_default = aapl_id
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5acreate_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, &
            type_id, space_id, acpl_id_default, aapl_id_default, lapl_id_default, attr)
  END SUBROUTINE h5acreate_by_name_f

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
  SUBROUTINE h5aexists_f(obj_id, attr_name, attr_exists, hdferr)
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
       INTEGER FUNCTION h5aexists_c(obj_id, attr_name, attr_namelen, attr_exists_c)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AEXISTS_C'::h5aexists_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: attr_name
         INTEGER(HID_T), INTENT(IN) :: obj_id
         CHARACTER(LEN=*), INTENT(IN) :: attr_name
         INTEGER(SIZE_T) :: attr_namelen
         INTEGER(HID_T) :: attr_exists_c
       END FUNCTION h5aexists_c
    END INTERFACE

    attr_namelen = LEN(attr_name)

    hdferr = h5aexists_c(obj_id, attr_name, attr_namelen, attr_exists_c)

    attr_exists = .FALSE.
    IF(attr_exists_c.GT.0) attr_exists = .TRUE.

  END SUBROUTINE h5aexists_f

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
  SUBROUTINE h5aexists_by_name_f(loc_id, obj_name, attr_name, attr_exists, hdferr, lapl_id)
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
       INTEGER FUNCTION h5aexists_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, lapl_id_default, attr_exists_c)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AEXISTS_BY_NAME_C'::h5aexists_by_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: obj_name, attr_name 
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         INTEGER(SIZE_T), INTENT(IN) :: obj_namelen
         CHARACTER(LEN=*), INTENT(IN) :: attr_name
         INTEGER(SIZE_T), INTENT(IN) :: attr_namelen
         INTEGER(HID_T), INTENT(IN) :: lapl_id_default
         INTEGER, INTENT(OUT) :: attr_exists_c
       END FUNCTION h5aexists_by_name_c
    END INTERFACE

    attr_namelen = LEN(attr_name)
    obj_namelen = LEN(obj_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5aexists_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, lapl_id_default, attr_exists_c)

    attr_exists = .FALSE.
    IF(attr_exists_c.GT.0) attr_exists = .TRUE.

  END SUBROUTINE h5aexists_by_name_f
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
  SUBROUTINE h5aopen_by_name_f(loc_id, obj_name, attr_name, attr_id, hdferr, aapl_id, lapl_id)
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
       INTEGER FUNCTION h5aopen_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, &
            aapl_id_default, lapl_id_default, attr_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AOPEN_BY_NAME_C'::h5aopen_by_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: obj_name, attr_name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         INTEGER(SIZE_T), INTENT(IN) :: obj_namelen
         CHARACTER(LEN=*), INTENT(IN) :: attr_name
         INTEGER(SIZE_T), INTENT(IN) :: attr_namelen
         INTEGER(HID_T) :: aapl_id_default
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(HID_T), INTENT(OUT) :: attr_id
       END FUNCTION h5aopen_by_name_c
    END INTERFACE

    attr_namelen = LEN(attr_name)
    obj_namelen = LEN(obj_name)

    aapl_id_default = H5P_DEFAULT_F
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(aapl_id)) aapl_id_default = aapl_id
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5aopen_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, &
         aapl_id_default, lapl_id_default, attr_id)

  END SUBROUTINE h5aopen_by_name_f

!
!****s* H5A/h5arename_f
!
! NAME
!  h5arename_f
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
  SUBROUTINE h5arename_f(loc_id, old_attr_name, new_attr_name, hdferr)
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
       INTEGER FUNCTION h5arename_c(loc_id, &
            old_attr_name, old_attr_namelen, new_attr_name, new_attr_namelen)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ARENAME_C'::h5arename_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: old_attr_name, new_attr_name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: old_attr_name
         INTEGER(SIZE_T) :: old_attr_namelen
         CHARACTER(LEN=*), INTENT(IN) :: new_attr_name
         INTEGER(SIZE_T) :: new_attr_namelen

       END FUNCTION h5arename_c
    END INTERFACE

    old_attr_namelen = LEN(old_attr_name)
    new_attr_namelen = LEN(new_attr_name)

    hdferr = h5arename_c(loc_id, &
         old_attr_name, old_attr_namelen, new_attr_name, new_attr_namelen)

  END SUBROUTINE h5arename_f

END MODULE H5A


