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
! This example extends an HDF5 dataset. It is used in the HDF5 Tutorial.

PROGRAM H5_EXTEND

  USE HDF5 ! This module contains all necessary modules 

  IMPLICIT NONE

  !
  !the dataset is stored in file "extend.h5" 
  !
  CHARACTER(LEN=9), PARAMETER :: filename = "extend.h5"

  !
  !dataset rank is 2 and name is "ExtendibleArray"
  !
  CHARACTER(LEN=15), PARAMETER :: dsetname = "ExtendibleArray"
  INTEGER :: RANK = 2

  INTEGER(HID_T) :: file_id       ! File identifier 
  INTEGER(HID_T) :: dset_id       ! Dataset identifier 
  INTEGER(HID_T) :: dataspace     ! Dataspace identifier 
  INTEGER(HID_T) :: memspace      ! Memory dataspace identifier 
  INTEGER(HID_T) :: crp_list      ! Dataset creation property identifier 

  !
  !dataset dimensions at creation time
  !
  INTEGER(HSIZE_T), DIMENSION(1:2) :: dims = (/3,3/)

  !
  !data dimensions 
  !
  INTEGER(HSIZE_T), DIMENSION(1:2) :: dimsc = (/2,5/)
  INTEGER(HSIZE_T), DIMENSION(1:2) :: dimsm = (/3,7/)

  !
  !Maximum dimensions
  !
  INTEGER(HSIZE_T), DIMENSION(1:2) :: maxdims 

  INTEGER(HSIZE_T), DIMENSION(1:2) :: offset
  INTEGER(HSIZE_T), DIMENSION(1:2) :: count 

  !
  ! Variables for reading and writing 
  !
  INTEGER, DIMENSION(1:3,1:3)  :: data1 
  INTEGER, DIMENSION(1:21) :: data2 = &
       (/2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4/)
  INTEGER(HSIZE_T), DIMENSION(1:2) :: data_dims

  !
  !Size of data in the file 
  !
  INTEGER(HSIZE_T), DIMENSION(1:2) :: size

  !
  !general purpose integer 
  !
  INTEGER(HSIZE_T) :: i, j

  !
  !flag to check operation success 
  !
  INTEGER :: error 

  !
  !Variables used in reading data back
  !  
  INTEGER(HSIZE_T), DIMENSION(1:2) :: dimsr, maxdimsr
  INTEGER :: rankr
  INTEGER, DIMENSION(1:3,1:10)  :: rdata 

  !
  !Initialize FORTRAN predifined datatypes
  !
  CALL h5open_f(error) 

  !
  !Create a new file using default properties.
  ! 
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)

  !
  !Create the data space with unlimited dimensions.
  !
  maxdims = (/H5S_UNLIMITED_F, H5S_UNLIMITED_F/)

  CALL h5screate_simple_f(RANK, dims, dataspace, error, maxdims)

  !
  !Modify dataset creation properties, i.e. enable chunking
  !
  CALL h5pcreate_f(H5P_DATASET_CREATE_F, crp_list, error)

  CALL h5pset_chunk_f(crp_list, RANK, dimsc, error)

  !
  !Create a dataset with 3X3 dimensions using cparms creation propertie .
  !
  CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, dataspace, &
       dset_id, error, crp_list )
  CALL h5sclose_f(dataspace, error)

  !
  !Fill data array with 1's 
  !
  DO i = 1, dims(1)
     DO j = 1, dims(2)
        data1(i,j) = 1
     END DO
  END DO

  !
  !Write data array to dataset
  !
  data_dims(1:2) = (/3,3/) 
  CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data1, data_dims, error)

  !
  !Extend the dataset. Dataset becomes 10 x 3.
  !
  size(1:2)   = (/3,10/)
  CALL h5dset_extent_f(dset_id, size, error)

  offset(1:2) = (/0,3/)
  count(1:2)  = (/3,7/)

  CALL h5screate_simple_f (2, dimsm, memspace, error)

  !
  !Write to 3x7 extended part of dataset
  !   
  CALL h5dget_space_f(dset_id, dataspace, error)
  CALL h5sselect_hyperslab_f(dataspace, H5S_SELECT_SET_F, &
       offset, count, error)

  data_dims(1:2) = (/3,7/)
  CALL H5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data2, data_dims, error, &
       memspace, dataspace)

  !
  !Close the objects that were opened.
  !
  CALL h5sclose_f(dataspace, error)
  CALL h5pclose_f(crp_list, error)
  CALL h5dclose_f(dset_id, error)
  CALL h5fclose_f(file_id, error)

  !
  !read the data back
  !
  !Open the file.
  !
  CALL h5fopen_f (filename, H5F_ACC_RDONLY_F, file_id, error)

  !
  !Open the  dataset.
  !
  CALL h5dopen_f(file_id, dsetname, dset_id, error)

  !
  !Get dataset's dataspace handle.
  !
  CALL h5dget_space_f(dset_id, dataspace, error)

  !
  !Get dataspace's rank.
  !
  CALL h5sget_simple_extent_ndims_f(dataspace, rankr, error)

  !
  !Get dataspace's dimensions.
  ! 
  CALL h5sget_simple_extent_dims_f(dataspace, dimsr, maxdimsr, error)

  !
  !Get creation property list.
  !
  CALL h5dget_create_plist_f(dset_id, crp_list, error)

  !
  ! Fill read buffer with zeroes
  !
  rdata(1:dimsr(1),1:dimsr(2)) = 0

  !
  !create memory dataspace
  !
  CALL h5screate_simple_f(rankr, dimsr, memspace, error)

  !
  !Read data 
  !
  data_dims(1:2) = (/3,10/)
  CALL H5dread_f(dset_id, H5T_NATIVE_INTEGER, rdata, data_dims, &
       error, memspace, dataspace)

  WRITE(*,'(A)') "Dataset:" 
  DO i = 1, dimsr(1)
     WRITE(*,'(100(I0,1X))') rdata(i,1:dimsr(2))    
  END DO

  !
  !Close the objects that were opened.
  !
  CALL h5sclose_f(dataspace, error)
  CALL h5sclose_f(memspace, error)
  CALL h5pclose_f(crp_list, error)
  CALL h5dclose_f(dset_id, error)
  CALL h5fclose_f(file_id, error)

  !Close FORTRAN predefined datatypes
  !
  CALL h5close_f(error)

END PROGRAM H5_EXTEND
