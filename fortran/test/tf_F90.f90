!****h* root/fortran/test/tf_F90.f90
!
! NAME
!  tf_F90.f90
!
! FUNCTION
!  Module for when the compiler is not F2003 or F2008 compliant. 
!  Needed by tf.f90 for the test programs.
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
!*****
MODULE TH5_MISC_PROVISIONAL
  IMPLICIT NONE

  INTEGER, PARAMETER :: sp = KIND(0.0)
  INTEGER, PARAMETER :: dp = KIND(0.D0)

END MODULE TH5_MISC_PROVISIONAL
