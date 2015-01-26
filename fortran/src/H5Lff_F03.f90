!****h* ROBODoc/H5L (F03)
!
! NAME
!  H5L_PROVISIONAL
!
! FILE
!  src/fortran/src/H5Lff_F03.f90
!
! PURPOSE
!
!  This file contains Fortran 90 and Fortran 2003 interfaces for H5L functions.
!  It contains the same functions as H5Lff_F90.f90 but includes the
!  Fortran 2003 functions and the interface listings. This file will be compiled
!  instead of H5Lff_F90.f90 if Fortran 2003 functions are enabled.
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

MODULE H5L_PROVISIONAL

  USE H5GLOBAL
  USE ISO_C_BINDING

  IMPLICIT NONE

!****t* H5L (F03)/h5l_info_t
!
! Fortran2003 Derived Type:
!
  TYPE, bind(c) :: union_t
     INTEGER(haddr_t) :: address
     INTEGER(size_t)  :: val_size
  END TYPE union_t

  TYPE, bind(c) :: h5l_info_t
     INTEGER(c_int) :: type ! H5L_type_t     type
!       LOGICAL(c_bool) :: corder_valid ! hbool_t        corder_valid
     INTEGER(c_int64_t) :: corder ! int64_t        corder;
     INTEGER(c_int) :: cset ! H5T_cset_t     cset;
     TYPE(union_t) :: u
  END TYPE h5l_info_t

!*****

!type specifies the link class. Valid values include the following:
!     	H5L_TYPE_HARD 	Hard link
!     	H5L_TYPE_SOFT 	Soft link
!     	H5L_TYPE_EXTERNAL     	External link
!     	H5L_TYPE_ERROR 	Error 
!cset specifies the character set in which the link name is encoded. Valid values include the following:
!     	H5T_CSET_ASCII 	US ASCII
!     	H5T_CSET_UTF8     	UTF-8 Unicode encoding


CONTAINS

!****s* H5L (F03)/h5literate_f
!
! NAME
!  h5literate_f
!
! PURPOSE
!  Iterates through links in a group.
!
! Inputs:
!  group_id   - Identifier specifying subject group
!  index_type - Type of index which determines the order:
!                H5_INDEX_NAME_F      - Alpha-numeric index on name
!                H5_INDEX_CRT_ORDER_F - Index on creation order
!  order      - Order within index:
!                H5_ITER_INC_F    - Increasing order
!                H5_ITER_DEC_F    - Decreasing order
!                H5_ITER_NATIVE_F - Fastest available order
!  idx 	      - IN: Iteration position at which to start
!  op 	      - Callback function passing data regarding the link to the calling application
!  op_data    - User-defined pointer to data required by the application for its processing of the link
!
! Outputs:
!  idx 	        - OUT: Position at which an interrupted iteration may be restarted
!  return_value - Success: The return value of the first operator that
! 			   returns non-zero, or zero if all members were
! 			   processed with no operator returning non-zero.
!
! 		  Failure: Negative if something goes wrong within the
! 			   library, or the negative value returned by one
! 			   of the operators.
!
!  hdferr     - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  M. Scot Breitenfeld
!  July 8, 2008
!
! Fortran2003 Interface:
  SUBROUTINE h5literate_f(group_id, index_type, order, idx, op, op_data, return_value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)    :: group_id
    INTEGER         , INTENT(IN)    :: index_type
    INTEGER         , INTENT(IN)    :: order
    INTEGER(HSIZE_T), INTENT(INOUT) :: idx
    TYPE(C_FUNPTR)  , INTENT(IN)    :: op
    TYPE(C_PTR)     , INTENT(IN)    :: op_data
    INTEGER         , INTENT(OUT)   :: return_value
    INTEGER         , INTENT(OUT)   :: hdferr
!*****
    INTERFACE
       INTEGER FUNCTION h5literate_c(group_id, index_type, order, idx, op, op_data) &
            BIND(C, NAME='h5literate_c')
         USE, INTRINSIC :: ISO_C_BINDING, ONLY : c_ptr, c_funptr
         USE H5GLOBAL
         INTEGER(HID_T), INTENT(IN) :: group_id
         INTEGER, INTENT(IN) :: index_type
         INTEGER, INTENT(IN) :: order
         INTEGER(HSIZE_T), INTENT(INOUT) :: idx
         TYPE(C_FUNPTR), VALUE :: op
         TYPE(C_PTR), VALUE :: op_data
       END FUNCTION h5literate_c
    END INTERFACE

    return_value = h5literate_c(group_id, index_type, order, idx, op, op_data)

    IF(return_value.GE.0)THEN
       hdferr = 0
    ELSE
       hdferr = -1
    END IF

  END SUBROUTINE h5literate_f

!****s* H5L (F03)/h5literate_by_name_f
!
! NAME
!  h5literate_by_name_f
!
! PURPOSE
!  Iterates through links in a group.
!
! Inputs:
!  loc_id     - File or group identifier specifying location of subject group
!  group_name - Name of subject group
!  index_type - Type of index which determines the order:
!                H5_INDEX_NAME_F      - Alpha-numeric index on name
!                H5_INDEX_CRT_ORDER_F - Index on creation order
!  order      - Order within index:
!                H5_ITER_INC_F    - Increasing order
!                H5_ITER_DEC_F    - Decreasing order
!                H5_ITER_NATIVE_F - Fastest available order
!  idx 	      - IN: Iteration position at which to start
!  op 	      - Callback function passing data regarding the link to the calling application
!  op_data    - User-defined pointer to data required by the application for its processing of the link
!
! Outputs:
!  idx 	        - OUT: Position at which an interrupted iteration may be restarted
!  return_value - Success: The return value of the first operator that
! 			   returns non-zero, or zero if all members were
! 			   processed with no operator returning non-zero.
!
! 		  Failure: Negative if something goes wrong within the
! 			   library, or the negative value returned by one
! 			   of the operators.
!
!  hdferr        - Returns 0 if successful and -1 if fails
!
! Optional parameters:
!  lapl_id    - Link access property list
!
! AUTHOR
!  M. Scot Breitenfeld
!  Augest 18, 2008
!
! Fortran2003 Interface:
  SUBROUTINE h5literate_by_name_f(loc_id, group_name, index_type, order, &
       idx, op, op_data, return_value, hdferr, lapl_id)
    USE, INTRINSIC :: ISO_C_BINDING
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
!*****
    INTEGER(HID_T) :: lapl_id_default
    INTEGER(SIZE_T) :: namelen

    INTERFACE
       INTEGER FUNCTION h5literate_by_name_c(loc_id, name, namelen, index_type, order,&
            idx, op, op_data, lapl_id_default) BIND(C, NAME='h5literate_by_name_c')
         USE, INTRINSIC :: ISO_C_BINDING, ONLY : c_char, c_ptr, c_funptr
         USE H5GLOBAL
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

END MODULE H5L_PROVISIONAL
