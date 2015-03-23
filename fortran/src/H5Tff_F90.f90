!****h* ROBODoc/H5T (F90)
!
! NAME
!  MODULE H5T_PROVISIONAL
!
! PURPOSE
!  This file contains Fortran 90 interfaces for H5T functions. It contains
!  the same functions as H5Tff_F03.f90 but excludes the Fortran 2003 functions
!  and the interface listings. This file will be compiled instead of H5Tff_F03.f90
!  if Fortran 2003 functions are not enabled.
!
! NOTE
!  Currently contains no functions.
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
!                         *** IMPORTANT ***
!  If you add a new H5D function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!*****

MODULE H5T_PROVISIONAL

  USE H5GLOBAL

CONTAINS

!
!****s* H5T/h5tenum_insert_f
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
  SUBROUTINE h5tenum_insert_f(type_id,  name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: type_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(IN) :: value
    INTEGER, INTENT(OUT) :: hdferr
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
  END SUBROUTINE h5tenum_insert_f
  

END MODULE H5T_PROVISIONAL
