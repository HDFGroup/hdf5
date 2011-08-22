!****h* ROBODoc/H5O (F03)
!
! NAME
!  H5O_PROVISIONAL
!
! PURPOSE
!  This file contains Fortran 90 and Fortran 2003 interfaces for H5O functions.
!  It contains the same functions as H5Off_DEPRECIATE.f90 but includes the
!  Fortran 2003 functions and the interface listings. This file will be compiled
!  instead of H5Off_DEPRECIATE.f90 if Fortran 2003 functions are enabled.
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
!  Windows dll file 'hdf5_fortrandll.def' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!
!*****

MODULE H5O_PROVISIONAL

  USE H5GLOBAL
  USE ISO_C_BINDING

  IMPLICIT NONE


  enum, bind(c)
     enumerator :: H5O_TYPE_UNKNOWN_F = -1
     enumerator :: H5O_TYPE_GROUP_F, H5O_TYPE_DATASET_F, H5O_TYPE_NAMED_DATATYPE_F, H5O_TYPE_NTYPES_F
  end enum


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
     INTEGER(c_int) :: version ! Version number of header format in file
                                 ! unsigned version 
     INTEGER(c_int) :: nmesgs  ! Number of object header messages
                                 ! unsigned nmesgs 
     INTEGER(c_int) :: nchunks ! Number of object header chunks 
                                 ! unsigned nchunks
     INTEGER(c_int) :: flags   ! Object header status flags
                                 ! unsigned flags
     TYPE(space_t) :: space
     TYPE(mesg_t) :: mesg
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
  

  TYPE, BIND(C) :: H5O_info_t
     INTEGER(HADDR_T) ::  fileno ! File number that object is located in
                                   ! unsigned long
     INTEGER(HADDR_T) :: addr    ! Object address in file  
     INTEGER :: TYPE             ! Basic object type (group, dataset, etc.) 
                                   ! H5O_type_t          type
     INTEGER(c_int) :: rc        ! Reference count of object
                                   ! unsigned rc
     INTEGER(c_int) :: atime    ! Access time
     INTEGER(c_int) :: mtime    ! Modification time
     INTEGER(c_int) :: ctime    ! Change time
     INTEGER(c_int) :: btime    ! Birth time
     INTEGER(hsize_t) :: num_attrs ! # of attributes attached to object

     TYPE(hdr_t) :: hdr
     TYPE(meta_size_t) :: meta_size
  END TYPE H5O_info_t

CONTAINS

!****s* H5O (F03)/h5ovisit_f
!
! NAME
!  h5ovisit_f
!
! PURPOSE
!  Recursively visits all objects starting from a specified object.
!
! INPUTS
!  group_id 	 - Identifier of the group at which the recursive iteration begins
!  index_type 	 - Type of index; valid values include:
!                    H5_INDEX_NAME_F
!                    H5_INDEX_CRT_ORDER_F
!  order 	 - Order in which index is traversed; valid values include:
!                    H5_ITER_DEC_F
!                    H5_ITER_INC_F
!                    H5_ITER_NATIVE_F
!  op 	         - Callback function passing data regarding the group to the calling application
!  op_data 	 - User-defined pointer to data required by the application for its processing of the group
!
! OUTPUTS
!  idx 	         - returns the return value of the first operator that returns a positive value, or 
!                  zero if all members were processed with no operator returning non-zero.
!  hdferr 	 - error code:
!                    0 on success and -1 on failure
! AUTHOR
!  M. Scot Breitenfeld
!  November 19, 2008
!
! SOURCE
  SUBROUTINE h5ovisit_f(group_id, index_type, order, op, op_data, return_value, hdferr)
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: group_id
    INTEGER, INTENT(IN) :: index_type 
    INTEGER, INTENT(IN) :: order
!!$    INTEGER(HSIZE_T), INTENT(INOUT) :: idx  ! IN : Iteration position at which to start
!!$                                            ! OUT: Position at which an interrupted iteration may be restarted

    TYPE(C_FUNPTR):: op      ! Callback function passing data regarding the link to the calling application
    TYPE(C_PTR)   :: op_data ! User-defined pointer to data required by the application for its processing of the link

    INTEGER, INTENT(OUT) :: return_value ! Success:   The return value of the first operator that
 				         !            returns non-zero, or zero if all members were
 				         !            processed with no operator returning non-zero.

 		                         ! Failure:   Negative if something goes wrong within the
 				         !            library, or the negative value returned by one
 				         !            of the operators.

    INTEGER, INTENT(OUT) :: hdferr       ! Error code:
                                         ! 0 on success and -1 on failure
!*****
    INTERFACE
       INTEGER FUNCTION h5ovisit_c(group_id, index_type, order, op, op_data)
         USE ISO_C_BINDING
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OVISIT_C'::h5ovisit_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: group_id
         INTEGER, INTENT(IN) :: index_type
         INTEGER, INTENT(IN) :: order
         TYPE(C_FUNPTR), VALUE :: op
         TYPE(C_PTR), VALUE :: op_data
       END FUNCTION h5ovisit_c
    END INTERFACE

    return_value = h5ovisit_c(group_id, index_type, order, op, op_data)

    IF(return_value.GE.0)THEN
       hdferr = 0
    ELSE
       hdferr = -1
    END IF

  END SUBROUTINE h5ovisit_f



!
!!$!****s* H5O/h5oget_info_by_name_f
!
! NAME
!  h5oget_info_by_name_f
!
! PURPOSE
!  Retrieves the metadata for an object, identifying the object by location and relative name.
!
! INPUTS
!  loc_id 	  - File or group identifier specifying location of group in which object 
!                   is located.
!  name 	  - Name of group, relative to loc_id
!
! OUTPUTS  NOTE: In C it is defined as a structure: H5O_info_t
!    **** NEED TO MAKE THIS DERIVED DATATYPE ****
!  hdferr 	  - Returns 0 if successful and -1 if fails
! OPTIONAL PARAMETERS
!  lapl_id 	  - Link access property list
!
! AUTHOR
!  M. Scot Breitenfeld
!  December 1, 2008
!
! SOURCE
  SUBROUTINE h5oget_info_by_name_f(loc_id, name, &
       object_info, hdferr, lapl_id)
   !    f_corder_valid, corder, cset, data_size, hdferr, lapl_id)

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)  :: loc_id
    CHARACTER(LEN=*), INTENT(IN)  :: name
    TYPE(C_PTR)                   :: object_info
!!$    LOGICAL         , INTENT(OUT)           :: f_corder_valid
!!$    INTEGER         , INTENT(OUT)           :: corder
!!$    INTEGER         , INTENT(OUT)           :: cset
!!$    INTEGER(HSIZE_T), INTENT(OUT)           :: data_size
    INTEGER         , INTENT(OUT)           :: hdferr
    INTEGER(HID_T)  , INTENT(IN) , OPTIONAL :: lapl_id
!*****
    INTEGER         :: corder_valid
    INTEGER(SIZE_T) :: namelen
    INTEGER(HID_T)  :: lapl_id_default
    
    INTERFACE
       INTEGER FUNCTION h5oget_info_by_name_c(loc_id, name, namelen, lapl_id_default, &
           object_info)
         USE H5GLOBAL
         USE, INTRINSIC :: ISO_C_BINDING
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OGET_INFO_BY_NAME_C'::h5oget_info_by_name_c
         !DEC$ENDIF
         INTEGER(HID_T)  , INTENT(IN)  :: loc_id
         CHARACTER(LEN=*), INTENT(IN)  :: name
         INTEGER(SIZE_T) , INTENT(IN)  :: namelen
         INTEGER(HID_T)  , INTENT(IN)  :: lapl_id_default
         TYPE(C_PTR),value                   :: object_info

       END FUNCTION h5oget_info_by_name_c
    END INTERFACE

    namelen = LEN(name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = H5Oget_info_by_name_c(loc_id, name, namelen, lapl_id_default, &
         object_info)


!!$    f_corder_valid =.FALSE.
!!$    IF (corder_valid .EQ. 1) f_corder_valid =.TRUE.

  END SUBROUTINE H5Oget_info_by_name_f

END MODULE H5O_PROVISIONAL

