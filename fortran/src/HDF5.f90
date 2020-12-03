!****h* ROBODoc/HDF5
!
! NAME
!  MODULE HDF5
!
! FILE
!  src/fortran/src/HDF5.f90
!
! PURPOSE
!  This is the main module used for linking to the Fortran HDF library.
!
! COPYRIGHT
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!  Copyright by The HDF Group.                                                 *
!  Copyright by the Board of Trustees of the University of Illinois.           *
!  All rights reserved.                                                        *
!                                                                              *
!  This file is part of HDF5.  The full HDF5 copyright notice, including       *
!  terms governing use, modification, and redistribution, is contained in      *
!  the COPYING file, which can be found at the root of the source code         *
!  distribution tree, or in https://www.hdfgroup.org/licenses.                 *
!  If you do not have access to either file, you may request a copy from       *
!  help@hdfgroup.org.                                                          *
!  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
!*****

MODULE HDF5
  USE H5GLOBAL
  USE H5F
  USE H5F_PROVISIONAL
  USE H5G
  USE H5E
  USE H5E_PROVISIONAL
  USE H5I
  USE H5L
  USE H5L_PROVISIONAL
  USE H5S
  USE H5D
  USE H5D_PROVISIONAL
  USE H5A
  USE H5A_PROVISIONAL
  USE H5T
  USE H5T_PROVISIONAL
  USE H5O
  USE H5O_PROVISIONAL
  USE H5P
  USE H5P_PROVISIONAL
  USE H5R
  USE H5R_PROVISIONAL
  USE H5Z
  USE H5_DBLE_INTERFACE
  USE H5LIB
END MODULE HDF5
