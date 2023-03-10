! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! Copyright by The HDF Group.                                               *
! All rights reserved.                                                      *
!                                                                           *
! This file is part of HDF5.  The full HDF5 copyright notice, including     *
! terms governing use, modification, and redistribution, is contained in    *
! the COPYING file, which can be found at the root of the source code       *
! distribution tree, or in https://www.hdfgroup.org/licenses.               *
! If you do not have access to either file, you may request a copy from     *
! help@hdfgroup.org.                                                        *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! This example shows how to write and read a hyperslab.
! It is used in the HDF5 Tutorial.
!

PROGRAM H5_SUBSET

  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE

  CHARACTER(LEN=9), PARAMETER :: filename = "subset.h5"  ! File name
  CHARACTER(LEN=8), PARAMETER :: dsetname = "IntArray"   ! Dataset name

  INTEGER(HID_T) :: file_id       ! File identifier
  INTEGER(HID_T) :: dset_id       ! Dataset identifier
  INTEGER(HID_T) :: dataspace     ! Dataspace identifier
  INTEGER(HID_T) :: memspace      ! memspace identifier

  !
  ! To change the subset size, modify size of dimsm, sdata, dim0_sub,
  ! dim1_sub, and count
  !
  INTEGER(HSIZE_T), DIMENSION(1:2) :: dimsm = (/4,3/) ! Dataset dimensions
  INTEGER, DIMENSION(1:4,1:3) :: sdata                  ! Subset buffer
  INTEGER :: dim0_sub = 4
  INTEGER :: dim1_sub = 3
  INTEGER(HSIZE_T), DIMENSION(1:2) :: count = (/4,3/)  ! Size of hyperslab
  INTEGER(HSIZE_T), DIMENSION(1:2) :: offset = (/2,1/) ! Hyperslab offset
  INTEGER(HSIZE_T), DIMENSION(1:2) :: stride = (/1,1/) ! Hyperslab stride
  INTEGER(HSIZE_T), DIMENSION(1:2) :: block = (/1,1/)  ! Hyperslab block size

  INTEGER(HSIZE_T), DIMENSION(1:2) :: dimsf = (/10,8/) ! Dataset dimensions


  INTEGER, DIMENSION(1:10,1:8) :: data     ! Data to write
  INTEGER, DIMENSION(1:10,1:8) :: rdata    ! Data to read

  INTEGER :: rank = 2      ! Dataset rank ( in file )
  INTEGER :: dim0 = 10     ! Dataset size in file
  INTEGER :: dim1 = 8

  INTEGER :: i, j

  INTEGER :: error         ! Error flag
  INTEGER(HSIZE_T), DIMENSION(2) :: data_dims

  !
  ! Write data to the HDF5 file.
  !

  !
  ! Data initialization.
  !
  DO i = 1, dim0
     DO j = 1, dim1
        IF (i .LE. (dim0 / 2)) THEN
           data(i,j) = 1
        ELSE
           data(i,j) = 2
        END IF
     END DO
  END DO

  !
  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(error)

  !
  ! Create a new file using default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)

  !
  ! Create the data space for the  dataset.
  !
  CALL h5screate_simple_f(rank, dimsf, dataspace, error)

  !
  ! Create the dataset with default properties.
  !
  CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, dataspace, &
       dset_id, error)

  !
  ! Write the dataset.
  !
  data_dims(1) = dim0
  data_dims(2) = dim1
  CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, data_dims, error)

  !
  ! Data Written to File
  !
  WRITE(*,'(/,A)') "Original Data Written to File:"
  DO i = 1, dim0
     WRITE(*,'(100(1X,I0,1X))') DATA(i,1:dim1)
  END DO

  !
  !
  ! Close the dataspace, dataset, and file.
  !
  CALL h5sclose_f(dataspace, error)
  CALL h5dclose_f(dset_id, error)
  CALL h5fclose_f(file_id, error)

  !
  ! Initialize subset data array.
  !
  sdata(1:dim0_sub,1:dim1_sub) = 5

  !
  ! Open the file.
  !
  CALL h5fopen_f(filename, H5F_ACC_RDWR_F, file_id, error)

  !
  ! Open the  dataset.
  !
  CALL h5dopen_f(file_id, dsetname, dset_id, error)

  !
  ! Get dataset's dataspace identifier and select subset.
  !
  CALL h5dget_space_f(dset_id, dataspace, error)
  CALL h5sselect_hyperslab_f(dataspace, H5S_SELECT_SET_F, &
       offset, count, error, stride, BLOCK)
  !
  ! Create memory dataspace.
  !
  CALL h5screate_simple_f(rank, dimsm, memspace, error)

  WRITE(*,'(/,A)') "Write subset to file specifying:"
  WRITE(*,'(A,/)') "   offset=2x1 stride=1x1 count=4x3 block=1x1"

  !
  ! Write subset to dataset
  !
  data_dims(1:2) = (/dim0_sub, dim1_sub/)
  CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, sdata, data_dims, error, &
       memspace, dataspace)

  data_dims(1:2) = (/dim0, dim1/)
  CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, rdata, data_dims, error)

  !
  ! Read entire dataset back
  !
  WRITE(*,'(A)') "Data in File after Subset Written:"
  DO i = 1, dim0
    WRITE(*,'(100(1X,I0,1X))') rdata(i,1:dim1)
  END DO
  PRINT *, " "

  !
  ! Close everything opened.
  !
  CALL h5sclose_f(dataspace, error)
  CALL h5sclose_f(memspace, error)
  CALL h5dclose_f(dset_id, error)
  CALL h5fclose_f(file_id, error)

  !
  ! Close FORTRAN interface.
  !
  CALL h5close_f(error)

END PROGRAM H5_SUBSET
