! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   Copyright by the Board of Trustees of the University of Illinois.         *
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
!
! The following example shows how to write and read to/from an existing dataset.
! It opens the file created in the previous example, obtains the dataset
! identifier, writes the data to the dataset in the file,
! then reads the dataset  to memory.
!
! This example is used in the HDF5 Tutorial.

PROGRAM H5_RDWT

  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE

  CHARACTER(LEN=8), PARAMETER :: filename = "dsetf.h5" ! File name
  CHARACTER(LEN=4), PARAMETER :: dsetname = "dset"     ! Dataset name

  INTEGER(HID_T) :: file_id       ! File identifier
  INTEGER(HID_T) :: dset_id       ! Dataset identifier

  INTEGER     ::   error ! Error flag
  INTEGER     ::  i, j

  INTEGER, DIMENSION(4,6) :: dset_data, data_out ! Data buffers
  INTEGER(HSIZE_T), DIMENSION(2) :: data_dims

  !
  ! Initialize the dset_data array.
  !
  DO i = 1, 4
     DO j = 1, 6
        dset_data(i,j) = (i-1)*6 + j
     END DO
  END DO

  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(error)

  !
  ! Open an existing file.
  !
  CALL h5fopen_f (filename, H5F_ACC_RDWR_F, file_id, error)

  !
  ! Open an existing dataset.
  !
  CALL h5dopen_f(file_id, dsetname, dset_id, error)

  !
  ! Write the dataset.
  !
  data_dims(1) = 4
  data_dims(2) = 6
  CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, dset_data, data_dims, error)

  !
  ! Read the dataset.
  !
  CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, data_out, data_dims, error)

  !
  ! Close the dataset.
  !
  CALL h5dclose_f(dset_id, error)

  !
  ! Close the file.
  !
  CALL h5fclose_f(file_id, error)

  !
  ! Close FORTRAN interface.
  !
  CALL h5close_f(error)

END PROGRAM H5_RDWT



