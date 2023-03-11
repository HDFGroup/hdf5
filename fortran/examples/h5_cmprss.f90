! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
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
!  This example illustrates how to create a compressed dataset.
!  It is used in the HDF5 Tutorial.
!
PROGRAM h5_cmprss

  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  !
  ! The dataset is stored in file "h5_cmprss.h5"
  !
  CHARACTER(LEN=12), PARAMETER :: filename = "h5_cmprss.h5"
  INTEGER, PARAMETER  :: rank = 2   ! Rank of the data set
  INTEGER, PARAMETER  :: dim0 = 100 ! Data set sizes
  INTEGER, PARAMETER  :: dim1 = 20

  INTEGER(hid_t) :: file_id, dataset_id, dataspace_id ! Identifiers
  INTEGER(hid_t) :: plist_id ! Property list identifier

  INTEGER :: error
  INTEGER(hsize_t), DIMENSION(1:rank) :: dims ! dimensions of data
  INTEGER(hsize_t), DIMENSION(1:rank) :: cdims ! sizes of chunked data

  INTEGER :: i,j, numfilt
  INTEGER, DIMENSION(1:dim0,1:dim1) :: buf ! write buffer
  INTEGER, DIMENSION(1:dim0,1:dim1) :: rbuf ! read buffer
  INTEGER(HSIZE_T), DIMENSION(1:rank) :: data_dims ! dimensions of data buffers

  INTEGER, DIMENSION(1:1) :: cd_values ! Auxiliary data for the filter
  INTEGER(size_t) :: nelmts            ! Number of elements in cd_values
  INTEGER :: flags ! Bit vector specifying certain general properties of the filter
  INTEGER(SIZE_T) :: namelen = 180 ! Anticipated number of characters in name
  CHARACTER(LEN=180) :: name ! Name of the filter
  INTEGER :: filter_id ! Filter identification number

  ! Uncomment these variables to use SZIP compression
  !INTEGER :: szip_options_mask
  !INTEGER :: szip_pixels_per_block

  !
  !Initialize FORTRAN predefined datatypes
  !
  CALL h5open_f(error)
  !
  ! Create a file
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
  !
  ! Create dataset "Compressed Data" in the group using absolute name.
  dims(1:2) = (/dim0, dim1/)
  CALL h5screate_simple_f(rank, dims, dataspace_id, error)
  CALL h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, error)
  !
  ! Dataset must be chunked for compression
  cdims(1:2) = 20
  CALL h5pset_chunk_f(plist_id, 2, cdims, error)

  ! Set ZLIB / DEFLATE Compression using compression level 6.
  ! To use SZIP Compression comment out these lines.
  CALL h5pset_deflate_f(plist_id, 6, error)

  ! Uncomment these lines to set SZIP Compression
  !szip_options_mask = H5_SZIP_NN_OM_F
  !szip_pixels_per_block = 16
  !CALL H5Pset_szip_f(plist_id, szip_options_mask, szip_pixels_per_block, error)

  ! Create data set
  CALL h5dcreate_f(file_id, "Compressed_Data", H5T_NATIVE_INTEGER, dataspace_id, &
       dataset_id, error, dcpl_id=plist_id)

  DO j = 1, dim1
     DO i = 1, dim0
        buf(i,j) = i+j
     ENDDO
  ENDDO

  data_dims(1:2) = (/dim0,dim1/)
  CALL h5dwrite_f(dataset_id, H5T_NATIVE_INTEGER, buf, data_dims, error)

  ! Close resources
  CALL h5sclose_f(dataspace_id, error)
  CALL h5pclose_f(plist_id, error)
  CALL h5dclose_f(dataset_id, error)
  CALL h5fclose_f(file_id, error)

  ! Now reopen the file and dataset in the file.
  CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error)
  CALL h5dopen_f(file_id, "Compressed_Data", dataset_id, error)

  ! Retrieve filter information.
  CALL h5dget_create_plist_f(dataset_id, plist_id, error)

  CALL h5pget_nfilters_f(plist_id, numfilt, error)
  WRITE(*,'(A, I0)') "Number of filters associated with dataset: ", numfilt

  DO i = 1, numfilt
     nelmts = 1
     CALL h5pget_filter_f(plist_id, 0, flags, nelmts, cd_values, &
          namelen, name, filter_id, error)

      WRITE(*,'(30X,A)', ADVANCE='NO')"Filter Type: "
      IF(filter_id.EQ.H5Z_FILTER_DEFLATE_F)THEN
         WRITE(*,'(A)') "H5Z_FILTER_DEFLATE"
      ELSEIF (filter_id.EQ.H5Z_FILTER_SZIP_F)THEN
         WRITE(*,'(A)') "H5Z_FILTER_SZIP"
      ELSE
         WRITE(*,'(A)') "Other filter type included"
      ENDIF
   ENDDO
  data_dims(1:2) = (/dim0,dim1/)
  CALL h5dread_f(dataset_id, H5T_NATIVE_INTEGER, rbuf, data_dims, error)

  CALL h5dclose_f(dataset_id, error)
  CALL h5pclose_f(plist_id, error)
  CALL h5fclose_f(file_id, error)

END PROGRAM h5_cmprss
