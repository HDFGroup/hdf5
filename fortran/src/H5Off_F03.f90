!****h* ROBODoc/H5O (F03)
!
! NAME
!  H5O_PROVISIONAL
!
! PURPOSE
!  This file contains Fortran 90 and Fortran 2003 interfaces for H5O functions.
!  It contains the same functions as H5Off_F90.f90 but includes the
!  Fortran 2003 functions and the interface listings. This file will be compiled
!  instead of H5Off_F90.f90 if Fortran 2003 functions are enabled.
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
!  If you add a new H5P function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!
!*****

MODULE H5O_PROVISIONAL

  USE H5GLOBAL
  USE, INTRINSIC :: ISO_C_BINDING

  IMPLICIT NONE

  enum, bind(c)
     enumerator :: H5O_TYPE_UNKNOWN_F = -1
     enumerator :: H5O_TYPE_GROUP_F, H5O_TYPE_DATASET_F, H5O_TYPE_NAMED_DATATYPE_F, H5O_TYPE_NTYPES_F
  end enum

!****t* H5T (F03)/h5o_info_t
!
! Fortran2003 Derived Type:
!
  TYPE, BIND(C) :: space_t
     INTEGER(hsize_t) :: total ! Total space for storing object header in file
     INTEGER(hsize_t) :: meta  ! Space within header for object header metadata information
     INTEGER(hsize_t) :: mesg  ! Space within header for actual message information
     INTEGER(hsize_t) :: free  ! Free space within object header
  END TYPE space_t

  TYPE, BIND(C) :: mesg_t
     INTEGER(c_int64_t) :: present ! Flags to indicate presence of message type in header 
     INTEGER(c_int64_t) :: shared  ! Flags to indicate message type is shared in header
  END TYPE mesg_t
  
  TYPE, BIND(C) :: hdr_t
     INTEGER :: version ! Version number of header format in file
     INTEGER :: nmesgs  ! Number of object header messages
     INTEGER :: nchunks ! Number of object header chunks
     INTEGER :: flags   ! Object header status flags
     TYPE(space_t)  :: space   
     TYPE(mesg_t)   :: mesg
  END TYPE hdr_t

  ! Extra metadata storage for obj & attributes
  TYPE, BIND(C) :: H5_ih_info_t
     INTEGER(hsize_t) :: index_size ! btree and/or list
     INTEGER(hsize_t) :: heap_size
  END TYPE H5_ih_info_t

  TYPE, BIND(C) :: meta_size_t
     TYPE(H5_ih_info_t) :: obj  ! v1/v2 B-tree & local/fractal heap for groups, B-tree for chunked datasets
     TYPE(H5_ih_info_t) :: attr ! v2 B-tree & heap for attributes
  ENDTYPE meta_size_t
  
  TYPE, BIND(C) :: h5o_info_t
     INTEGER(C_LONG)  :: fileno     ! File number that object is located in
     INTEGER(haddr_t) :: addr       ! Object address in file  
     INTEGER(C_INT)   :: type       ! Basic object type (group, dataset, etc.) 
     INTEGER          :: rc         ! Reference count of object

     INTEGER, DIMENSION(8) :: atime ! Access time         !    -- NOTE --
     INTEGER, DIMENSION(8) :: mtime ! Modification time   ! Returns an integer array    
     INTEGER, DIMENSION(8) :: ctime ! Change time         ! as specified in the Fortran 
     INTEGER, DIMENSION(8) :: btime ! Birth time          ! intrinsic DATE_AND_TIME(VALUES)

     INTEGER(hsize_t) :: num_attrs  ! # of attributes attached to object

     TYPE(hdr_t) :: hdr

     TYPE(meta_size_t) :: meta_size
  END TYPE h5o_info_t

!*****

CONTAINS

!****s* H5O (F03)/h5ovisit_f_F03
!
! NAME
!  h5ovisit_f
!
! PURPOSE
!  Recursively visits all objects starting from a specified object.
!
! Inputs:
!  object_id  - Identifier of the object at which the recursive iteration begins.
!  index_type - Type of index; valid values include:
!                H5_INDEX_NAME_F
!                H5_INDEX_CRT_ORDER_F
!  order      - Order in which index is traversed; valid values include:
!                H5_ITER_DEC_F
!                H5_ITER_INC_F
!                H5_ITER_NATIVE_F
!  op 	      - Callback function passing data regarding the group to the calling application
!  op_data    - User-defined pointer to data required by the application for its processing of the group
!
! Outputs:
!  return_value - returns the return value of the first operator that returns a positive value, or 
!                 zero if all members were processed with no operator returning non-zero.
!  hdferr       - Returns 0 if successful and -1 if fails
! AUTHOR
!  M. Scot Breitenfeld
!  November 19, 2008
!
! Fortran2003 Interface:
  SUBROUTINE h5ovisit_f(object_id, index_type, order, op, op_data, return_value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: object_id
    INTEGER, INTENT(IN) :: index_type 
    INTEGER, INTENT(IN) :: order

    TYPE(C_FUNPTR):: op
    TYPE(C_PTR)   :: op_data
    INTEGER, INTENT(OUT) :: return_value
    INTEGER, INTENT(OUT) :: hdferr
!*****

    INTERFACE
       INTEGER FUNCTION h5ovisit_c(object_id, index_type, order, op, op_data)
         USE, INTRINSIC :: ISO_C_BINDING
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OVISIT_C'::h5ovisit_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: object_id
         INTEGER, INTENT(IN) :: index_type
         INTEGER, INTENT(IN) :: order
         TYPE(C_FUNPTR), VALUE :: op
         TYPE(C_PTR), VALUE :: op_data
       END FUNCTION h5ovisit_c
    END INTERFACE

    return_value = h5ovisit_c(object_id, index_type, order, op, op_data)

    IF(return_value.GE.0)THEN
       hdferr = 0
    ELSE
       hdferr = -1
    END IF

  END SUBROUTINE h5ovisit_f

!****s* H5O (F03)/h5oget_info_by_name_f_F03
!
! NAME
!  h5oget_info_by_name_f
!
! PURPOSE
!  Retrieves the metadata for an object, identifying the object by location and relative name.
!
! Inputs:
!  loc_id      - File or group identifier specifying location of group 
!                in which object is located.
!  name        - Name of group, relative to loc_id.
!
! Outputs:  
!  object_info - Buffer in which to return object information.
!  hdferr      - Returns 0 if successful and -1 if fails.
!
! Optional parameters:
!  lapl_id     - Link access property list.
!
! AUTHOR
!  M. Scot Breitenfeld
!  December 1, 2008
!
! Fortran2003 Interface:
  SUBROUTINE h5oget_info_by_name_f(loc_id, name, object_info, hdferr, lapl_id)

    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)            :: loc_id
    CHARACTER(LEN=*), INTENT(IN)            :: name
    TYPE(h5o_info_t), INTENT(OUT), TARGET   :: object_info
    INTEGER         , INTENT(OUT)           :: hdferr
    INTEGER(HID_T)  , INTENT(IN) , OPTIONAL :: lapl_id
!*****
    INTEGER(SIZE_T) :: namelen
    INTEGER(HID_T)  :: lapl_id_default
    TYPE(C_PTR)     :: ptr
    
    INTERFACE
       INTEGER FUNCTION h5oget_info_by_name_c(loc_id, name, namelen, lapl_id_default, &
           object_info)
         USE H5GLOBAL
         USE, INTRINSIC :: ISO_C_BINDING
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OGET_INFO_BY_NAME_C'::h5oget_info_by_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T)  , INTENT(IN)  :: loc_id
         CHARACTER(LEN=*), INTENT(IN)  :: name
         INTEGER(SIZE_T) , INTENT(IN)  :: namelen
         INTEGER(HID_T)  , INTENT(IN)  :: lapl_id_default
         TYPE(C_PTR),VALUE             :: object_info

       END FUNCTION h5oget_info_by_name_c
    END INTERFACE

    namelen = LEN(name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    ptr = C_LOC(object_info)

    hdferr = H5Oget_info_by_name_c(loc_id, name, namelen, lapl_id_default, ptr)

  END SUBROUTINE H5Oget_info_by_name_f

!****s* H5O (F03)/h5oget_info_f_F03
!
! NAME
!  h5oget_info_f
!
! PURPOSE
!  Retrieves the metadata for an object specified by an identifier.
!
! Inputs:
!  object_id   - Identifier for target object.
!
! Outputs:
!  object_info - Buffer in which to return object information.
!  hdferr      - Returns 0 if successful and -1 if fails.
!
! AUTHOR
!  M. Scot Breitenfeld
!  May 11, 2012
!
! Fortran2003 Interface:
  SUBROUTINE h5oget_info_f(object_id, object_info, hdferr)

    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)            :: object_id
    TYPE(h5o_info_t), INTENT(OUT), TARGET   :: object_info
    INTEGER         , INTENT(OUT)           :: hdferr
!*****
    TYPE(C_PTR) :: ptr
    
    INTERFACE
       INTEGER FUNCTION h5oget_info_c(object_id, object_info)
         USE H5GLOBAL
         USE, INTRINSIC :: ISO_C_BINDING
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OGET_INFO_C'::h5oget_info_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN)  :: object_id
         TYPE(C_PTR), VALUE          :: object_info

       END FUNCTION h5oget_info_c
    END INTERFACE

    ptr = C_LOC(object_info)
    hdferr = H5Oget_info_c(object_id, ptr)

  END SUBROUTINE H5Oget_info_f

!****s* H5O (F03)/h5oget_info_by_idx_f_F03
!
! NAME
!  h5oget_info_by_idx_f
!
! PURPOSE
!  Retrieves the metadata for an object, identifying the object by an index position.
!
! Inputs:
!  loc_id      - File or group identifier specifying location of group 
!                in which object is located.
!  group_name  - Name of group in which object is located.
!  index_field - Index or field that determines the order.
!  order       - Order within field or index.
!  n           - Object for which information is to be returned
!
! Outputs:  
!  object_info - Buffer in which to return object information.
!  hdferr      - Returns 0 if successful and -1 if fails.
!
! Optional parameters:
!  lapl_id     - Link access property list. (Not currently used.)
!
! AUTHOR
!  M. Scot Breitenfeld
!  May 11, 2012
!
! Fortran2003 Interface:
  SUBROUTINE h5oget_info_by_idx_f(loc_id, group_name, index_field, order, n, &
       object_info, hdferr, lapl_id)

    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)            :: loc_id
    CHARACTER(LEN=*), INTENT(IN)            :: group_name
    INTEGER         , INTENT(IN)            :: index_field
    INTEGER         , INTENT(IN)            :: order
    INTEGER(HSIZE_T), INTENT(IN)            :: n
    TYPE(h5o_info_t), INTENT(OUT), TARGET   :: object_info
    INTEGER         , INTENT(OUT)           :: hdferr
    INTEGER(HID_T)  , INTENT(IN) , OPTIONAL :: lapl_id
!*****
    INTEGER(SIZE_T) :: namelen
    INTEGER(HID_T)  :: lapl_id_default
    TYPE(C_PTR)     :: ptr
    
    INTERFACE
       INTEGER FUNCTION h5oget_info_by_idx_c(loc_id, group_name, namelen, &
            index_field, order, n, lapl_id_default, object_info)
         USE H5GLOBAL
         USE, INTRINSIC :: ISO_C_BINDING
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OGET_INFO_BY_IDX_C'::h5oget_info_by_idx_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: group_name
         INTEGER(HID_T)  , INTENT(IN)  :: loc_id
         CHARACTER(LEN=*), INTENT(IN)  :: group_name
         INTEGER(SIZE_T) , INTENT(IN)  :: namelen
         INTEGER         , INTENT(IN)  :: index_field
         INTEGER         , INTENT(IN)  :: order
         INTEGER(HSIZE_T), INTENT(IN)  :: n
         INTEGER(HID_T)  , INTENT(IN)  :: lapl_id_default
         TYPE(C_PTR), VALUE            :: object_info

       END FUNCTION h5oget_info_by_idx_c
    END INTERFACE

    namelen = LEN(group_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    ptr = C_LOC(object_info)
    hdferr = H5Oget_info_by_idx_c(loc_id, group_name, namelen, index_field, order, n, lapl_id_default, ptr)

  END SUBROUTINE H5Oget_info_by_idx_f


!****s* H5O (F03)/h5ovisit_by_name_f_F03
!
! NAME
!  h5ovisit_by_name_f
!
! PURPOSE
!  Recursively visits all objects starting from a specified object.
!
! Inputs:
!  loc_id      - Identifier of a file or group.
!  object_name - Name of the object, generally relative to loc_id, that will serve as root of the iteration 
!  index_type  - Type of index; valid values include:
!                 H5_INDEX_NAME_F
!                 H5_INDEX_CRT_ORDER_F
!  order       - Order in which index is traversed; valid values include:
!                 H5_ITER_DEC_F
!                 H5_ITER_INC_F
!                 H5_ITER_NATIVE_F
!  op 	       - Callback function passing data regarding the group to the calling application
!  op_data     - User-defined pointer to data required by the application for its processing of the group
!
! Outputs:
!  return_value - Returns the return value of the first operator that returns a positive value, or 
!                 zero if all members were processed with no operator returning non-zero.
!  hdferr       - Returns 0 if successful and -1 if fails
!
! Optional parameters:
!  lapl_id      - Link access property list identifier.
!
! AUTHOR
!  M. Scot Breitenfeld
!  November 19, 2008
!
! Fortran2003 Interface:
  SUBROUTINE h5ovisit_by_name_f(loc_id, object_name, index_type, order, op, op_data, &
       return_value, hdferr, lapl_id)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)             :: loc_id
    CHARACTER(LEN=*), INTENT(IN)             :: object_name
    INTEGER         , INTENT(IN)             :: index_type 
    INTEGER         , INTENT(IN)             :: order

    TYPE(C_FUNPTR)                           :: op
    TYPE(C_PTR)                              :: op_data
    INTEGER         , INTENT(OUT)            :: return_value
    INTEGER         , INTENT(OUT)            :: hdferr
    INTEGER(HID_T)  , INTENT(IN) , OPTIONAL  :: lapl_id
!*****

    INTEGER(SIZE_T) :: namelen
    INTEGER(HID_T)  :: lapl_id_default

    INTERFACE
       INTEGER FUNCTION h5ovisit_by_name_c(loc_id, object_name, namelen, index_type, order, &
            op, op_data, lapl_id)
         USE, INTRINSIC :: ISO_C_BINDING
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OVISIT_BY_NAME_C'::h5ovisit_by_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: object_name
         INTEGER(HID_T)  , INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: object_name
         INTEGER(SIZE_T)              :: namelen
         INTEGER         , INTENT(IN) :: index_type
         INTEGER         , INTENT(IN) :: order
         TYPE(C_FUNPTR)  , VALUE      :: op
         TYPE(C_PTR)     , VALUE      :: op_data
         INTEGER(HID_T)  , INTENT(IN) :: lapl_id
       END FUNCTION h5ovisit_by_name_c
    END INTERFACE

    namelen = LEN(object_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    return_value = h5ovisit_by_name_c(loc_id, object_name, namelen, index_type, order, &
         op, op_data, lapl_id_default)

    IF(return_value.GE.0)THEN
       hdferr = 0
    ELSE
       hdferr = -1
    END IF

  END SUBROUTINE h5ovisit_by_name_f

END MODULE H5O_PROVISIONAL

