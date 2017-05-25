!****h* ROBODoc/H5L (F90)
!
! NAME
!  MODULE H5L_PROVISIONAL
!
! PURPOSE
!  This file contains Fortran 90 interfaces for H5L functions. It contains
!  the same functions as H5Lff_F03.f90 but excludes the Fortran 2003 functions
!  and the interface listings. This file will be compiled instead of H5Lff_F03.f90
!  if Fortran 2003 functions are not enabled.
!
! NOTE
!  Currently contains no functions.
!
! COPYRIGHT
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!  Copyright by The HDF Group.                                               *
!  Copyright by the Board of Trustees of the University of Illinois.         *
!  All rights reserved.                                                      *
!  *
!  This file is part of HDF5.  The full HDF5 copyright notice, including     *
!  terms governing use, modification, and redistribution, is contained in    *
!   the COPYING file, which can be found at the root of the source code       *
!   distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
!   If you do not have access to either file, you may request a copy from     *
!   help@hdfgroup.org.                                                        *
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
!                         *** IMPORTANT ***
!  If you add a new H5D function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!*****

MODULE H5L_PROVISIONAL

END MODULE H5L_PROVISIONAL
