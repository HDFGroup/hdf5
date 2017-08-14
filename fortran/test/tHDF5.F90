!****h* ROBODoc/THDF5
!
! NAME
!  MODULE THDF5
!
! FILE
!  src/fortran/test/tHDF5.f90
!
! PURPOSE
!  This is the test  module used for testing the Fortran90 HDF library APIs.
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

MODULE THDF5
  USE TH5_MISC
  USE TH5_MISC_GEN
  USE TH5A
  USE TH5D
  USE TH5E
  USE TH5F
  USE TH5G
  USE TH5I
  USE TH5P
  USE TH5R
  USE TH5S
  USE TH5SSELECT
  USE TH5T
  USE TH5VL
  USE TH5Z
END MODULE THDF5
