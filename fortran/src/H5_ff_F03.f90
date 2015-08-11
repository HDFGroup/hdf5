!****h* ROBODoc/H5LIB_PROVISIONAL
!
! NAME
!  MODULE H5LIB_PROVISIONAL
!
! PURPOSE
!  This file contains helper functions for Fortran 2003 features and is
!  only compiled when Fortran 2003 features are enabled, otherwise
!  the file H5_ff_F90.f90 is compiled.
!
! COPYRIGHT
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!  Copyright by The HDF Group.                                               *
!  Copyright by the Board of Trustees of the University of Illinois.         *
!  All rights reserved.                                                      *
!  *
!  This file is part of HDF5.  The full HDF5 copyright notice, including     *
!  terms governing use, modification, and redistribution, is contained in    *
!  the files COPYING and Copyright.html.  COPYING can be found at the root   *
!  of the source code distribution tree; Copyright.html can be found at the  *
!  root level of an installed copy of the electronic HDF5 document set and   *
!  is linked from the top-level documents page.  It can also be found at     *
!  http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
!  access to either file, you may request a copy from help@hdfgroup.org.     *
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! NOTES
!                         *** IMPORTANT ***
!  If you add a new function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!*****

MODULE H5LIB_PROVISIONAL

CONTAINS
!****f* H5LIB_PROVISIONAL/h5offsetof
!
! NAME
!  h5offsetof
!
! PURPOSE
!  Computes the offset in memory
!
! Inputs:
!  start - starting pointer address
!  end 	 - ending pointer address
!
! Outputs:
!  offset - offset of a member within the derived type
!
! AUTHOR
!  M. Scot Breitenfeld
!  Augest 25, 2008
!
! ACKNOWLEDGEMENTS
!  Joe Krahn
!
! Fortran2003 Interface:
  FUNCTION h5offsetof(start,end) RESULT(offset)
    USE, INTRINSIC :: ISO_C_BINDING
    USE H5GLOBAL
    IMPLICIT NONE
    INTEGER(SIZE_T) :: offset
    TYPE(C_PTR), VALUE, INTENT(IN) :: start, end
!*****
    INTEGER(C_INTPTR_T) :: int_address_start, int_address_end
    int_address_start = TRANSFER(start, int_address_start)
    int_address_end   = TRANSFER(end  , int_address_end  )

    offset = int_address_end - int_address_start

  END FUNCTION h5offsetof

END MODULE H5LIB_PROVISIONAL
