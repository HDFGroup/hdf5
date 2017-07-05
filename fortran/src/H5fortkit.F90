!****h* ROBODoc/H5fortkit
!
! NAME
!  MODULE H5fortkit
!
! FILE
!  fortran/src/H5fortkit.F90
!
! PURPOSE
!  Routines to deal with C-FORTRAN issues.
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
!*****
MODULE H5fortkit

CONTAINS

!****if* H5fortkit/HD5c2fstring
! NAME
!  HD5c2fstring
! INPUTS
!  cstring  -  C string stored as a string array of size 'len' of string size LEN=1
!  len      -  length of Fortran string
! OUTPUT
!   fstring -  Fortran string array of LEN=1
! PURPOSE
!   Copies a Fortran array of strings having a length of one to a fortran string and removes the C Null
!   terminator. The Null terminator is returned from C when calling the C APIs directly.
!
!   The fortran standard does not allow C_LOC to be used on a character string of
!   length greater than one, which is why we use the array of characters instead.
!
! SOURCE
  SUBROUTINE HD5c2fstring(fstring,cstring,len)
!*****
    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: len
    CHARACTER(LEN=len) :: fstring
    CHARACTER(LEN=1), DIMENSION(1:len) :: cstring 
    
    fstring = ''
    DO i = 1, len
       IF (cstring(i)(1:1)==CHAR(0)) EXIT
       fstring(i:i) = cstring(i)(1:1)
    END DO

  END SUBROUTINE HD5c2fstring

END MODULE H5fortkit
