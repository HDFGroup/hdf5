!****h* ROBODoc/H5E (F90)
!
! NAME
!  MODULE H5E_PROVISIONAL
!
! PURPOSE
!  This file contains Fortran 90 interfaces for H5E functions. It contains
!  the same functions as H5Eff_F03.f90 but excludes the Fortran 2003 functions
!  and the interface listings. This file will be compiled instead of H5Eff_F03.f90
!  if Fortran 2003 functions are not enabled.
!
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
!                         *** IMPORTANT ***
!  If you add a new H5D function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!*****

MODULE H5E_PROVISIONAL

  USE H5GLOBAL

CONTAINS

!****s* H5E/h5eset_auto_f
!
! NAME
!  h5eset_auto_f
!
! PURPOSE
!  Turns automatic error printing on or off.
!
! INPUTS
!  printflag 	 - Flag to turn automatic error printing on or off;
!                  possible values are:
!                    printon (1)
!                    printoff(0)
! OUTPUTS
!  hdferr 	 - Returns 0 if successful and -1 if fails
!
! AUTHOR
!  Elena Pourmal
!  August 12, 1999
!
! HISTORY
!  Explicit Fortran interfaces were added for
!  called C functions (it is needed for Windows
!  port).  April 6, 2001
!
! SOURCE
  SUBROUTINE h5eset_auto_f(printflag, hdferr)
    INTEGER, INTENT(IN) :: printflag  ! flag to turn automatic error
                                      ! printing on or off
                                      ! possible values are:
                                      !     printon (1)
                                      !     printoff(0)
    INTEGER, INTENT(OUT) :: hdferr    ! Error code
!*****
    INTERFACE
       INTEGER FUNCTION h5eset_auto_c(printflag)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ESET_AUTO_C'::h5eset_auto_c
         !DEC$ENDIF
         INTEGER :: printflag
       END FUNCTION h5eset_auto_c
    END INTERFACE

    hdferr = h5eset_auto_c(printflag)
  END SUBROUTINE h5eset_auto_f


END MODULE H5E_PROVISIONAL
