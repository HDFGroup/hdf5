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
!*****
MODULE H5fortkit

  USE H5FORTRAN_TYPES, ONLY : SIZE_T

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
  SUBROUTINE HD5c2fstring(fstring,cstring,flen,clen)
!*****
    IMPLICIT NONE

    INTEGER(SIZE_T) :: i
    INTEGER(SIZE_T) :: flen
    INTEGER(SIZE_T) :: clen
    CHARACTER(*)    :: fstring
    CHARACTER(LEN=1), DIMENSION(1:clen) :: cstring

    INTEGER(SIZE_T) :: flen_max

    fstring = ''
    flen_max = LEN(fstring, KIND=SIZE_T)
    DO i = 1, clen
       IF (i .GT. flen_max) EXIT
       IF (i .GT. flen) EXIT
       IF (cstring(i)(1:1)==CHAR(0)) EXIT
       fstring(i:i) = cstring(i)(1:1)
    END DO

  END SUBROUTINE HD5c2fstring

END MODULE H5fortkit
