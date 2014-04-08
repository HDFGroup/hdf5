!****h* root/fortran/test/tf_include.f90
!
! NAME
!  tf_include.f90
!
! FUNCTION
!  Contains overloaded operators for the hdf5 fortran tests, include in
!  tf.f90
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
! CONTAINS SUBROUTINES
! test_eqv
!
!*****

! Function for comparing two REAL(KIND=*) numbers.

PURE FUNCTION test_eqv(a,b)
  LOGICAL test_eqv
  REAL(wp), INTENT (in):: a,b
  test_eqv = ABS(a-b) .LT. 1.e-8
END FUNCTION test_eqv
