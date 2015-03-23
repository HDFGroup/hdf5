!****h* ROBODoc/H5T (F03)
!
! NAME
!  H5T_PROVISIONAL
!
! PURPOSE
!  This file contains Fortran 90 and Fortran 2003 interfaces for H5T functions.
!  It contains the same functions as H5Tff_F90.f90 but includes the
!  Fortran 2003 functions and the interface listings. This file will be compiled
!  instead of H5Tff_F90.f90 if Fortran 2003 functions are enabled.
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
!  If you add a new H5T function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!
!*****

MODULE H5T_PROVISIONAL

  USE H5GLOBAL
  USE, INTRINSIC :: ISO_C_BINDING

!****t* H5T (F03)/hvl_t
! Fortran2003 Derived Type:
  TYPE hvl_t
     INTEGER(size_t) :: len ! Length of VL data (in base type units)
     TYPE(C_PTR) :: p       ! Pointer to VL data
  END TYPE hvl_t

!*****

  INTERFACE h5tenum_insert_f
     MODULE PROCEDURE h5tenum_insert_f03
     MODULE PROCEDURE h5tenum_insert_f90
  END INTERFACE

CONTAINS

!****s* H5T (F03)/H5Tconvert_f_F03
!
! NAME
!  H5Tconvert_f
!
! PURPOSE
!  Converts data from between specified datatypes.
!
! Inputs:
!  src_id     - Identifier for the source datatype.
!  dst_id     - Identifier for the destination datatype.
!  nelmts     - Size of array buf.
!  buf 	      - Array containing pre-conversion values.
!  background - Optional background buffer.
!  plist_id   -	Dataset transfer property list identifier.
!
! Outputs:
!  buf 	      - Array containing post-conversion values.
!  hdferr     - error code:
!                 0 on success and -1 on failure
! AUTHOR
!  M. Scot Breitenfeld
!  Decemember 8, 2008
!
! Fortran2003 Interface:
  SUBROUTINE h5tconvert_f(src_id, dst_id, nelmts, buf, hdferr, background, plist_id)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T) , INTENT(IN)               :: src_id
    INTEGER(HID_T) , INTENT(IN)               :: dst_id
    INTEGER(SIZE_T), INTENT(IN)               :: nelmts
    TYPE(C_PTR)    , INTENT(INOUT)            :: buf
    INTEGER        , INTENT(OUT)              :: hdferr
    TYPE(C_PTR)    , INTENT(INOUT), OPTIONAL  :: background
    INTEGER(HID_T) , INTENT(IN)   , OPTIONAL  :: plist_id
!*****
    INTEGER(HID_T) :: plist_id_default
    TYPE(C_PTR) :: background_default

    INTERFACE
       INTEGER FUNCTION h5tconvert_c(src_id, dst_id, nelmts, buf, background, plist_id) &
            BIND(C, NAME='h5tconvert_c')
         USE, INTRINSIC :: ISO_C_BINDING, ONLY : c_ptr
         USE H5GLOBAL
         IMPLICIT NONE
         INTEGER(HID_T) , INTENT(IN)           :: src_id
         INTEGER(HID_T) , INTENT(IN)           :: dst_id
         INTEGER(SIZE_T), INTENT(IN)           :: nelmts
         TYPE(C_PTR)                , VALUE    :: buf
         TYPE(C_PTR)                , VALUE    :: background
         INTEGER(HID_T) , INTENT(IN)           :: plist_id
       END FUNCTION h5tconvert_c
    END INTERFACE

    plist_id_default = H5P_DEFAULT_F
    IF(PRESENT(plist_id)) plist_id_default = plist_id

    background_default = C_NULL_PTR
    IF(PRESENT(background)) background_default = background

    hdferr = H5Tconvert_c(src_id, dst_id, nelmts, buf, background_default, plist_id_default)

  END SUBROUTINE h5tconvert_f
!
!****s* (F03) H5T/h5tenum_insert_f90
!
! NAME
!  h5tenum_insert_f
!
! PURPOSE
!  Inserts a new enumeration datatype member.
!
! INPUTS
!  type_id  - Datatype identifier for the enumeration datatype.
!  name     - Datatype identifier.
!  value    - Value of the new member.
! OUTPUTS
!  hdferr   - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  March 7, 2001
! SOURCE
  SUBROUTINE h5tenum_insert_f90(type_id,  name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  !Name of  the new member
    INTEGER, INTENT(IN) :: value ! value of the new member
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
!*****
    INTEGER :: namelen
    INTERFACE
       INTEGER FUNCTION h5tenum_insert_c(type_id, name, namelen, value)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TENUM_INSERT_C'::h5tenum_insert_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: type_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN) :: value
         INTEGER :: namelen
       END FUNCTION h5tenum_insert_c
    END INTERFACE
            
    namelen = LEN(name)
    hdferr = h5tenum_insert_c(type_id, name, namelen, value)
  END SUBROUTINE h5tenum_insert_f90

!
!****s* (F03) H5T/h5tenum_insert_f03
!
! NAME
!  h5tenum_insert_f
!
! PURPOSE
!  Inserts a new enumeration datatype member.
!
! INPUTS
!  type_id  - Datatype identifier for the enumeration datatype.
!  name     - Datatype identifier.
!  value    - Pointer to the value of the new member.
! OUTPUTS
!  hdferr   - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  M. Scot Breitenfeld
!  February 6, 2015
!
! HISTORY
!  F2003 implementation of function
! SOURCE
  SUBROUTINE h5tenum_insert_f03(type_id, name, value, hdferr)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : c_ptr, c_char
    USE H5GLOBAL
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN) :: type_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    TYPE(C_PTR)     , INTENT(IN) :: value
    INTEGER, INTENT(OUT) :: hdferr
!*****
    INTEGER :: namelen
    
    INTERFACE
       INTEGER FUNCTION h5tenum_insert_ptr_c(type_id, name, namelen, value) &
            BIND(C, NAME='h5tenum_insert_ptr_c')
         USE, INTRINSIC :: ISO_C_BINDING, ONLY : c_ptr, c_char
         USE H5GLOBAL
         IMPLICIT NONE
         INTEGER(HID_T)  , INTENT(IN) :: type_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
         INTEGER :: namelen
         TYPE(C_PTR), VALUE :: value
       END FUNCTION h5tenum_insert_ptr_c
    END INTERFACE
            
    namelen = LEN(name)
    hdferr = h5tenum_insert_ptr_c(type_id, name, namelen, value)
  END SUBROUTINE h5tenum_insert_f03


END MODULE H5T_PROVISIONAL

