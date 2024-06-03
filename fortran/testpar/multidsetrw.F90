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
! writes/reads dataset by hyperslabs using multi-dataset routines, h5dread_multi and
! h5dwrite_multi
!

SUBROUTINE pmultiple_dset_hyper_rw(do_collective, do_chunk, mpi_size, mpi_rank, nerrors)

  USE iso_c_binding
  USE TH5_MISC
  USE hdf5
  USE mpi
  IMPLICIT NONE

  LOGICAL, INTENT(in) :: do_collective              ! use collective IO
  LOGICAL, INTENT(in) :: do_chunk                   ! use chunking
  INTEGER(KIND=MPI_INTEGER_KIND), INTENT(in) :: mpi_size ! number of processes in the group of communicator
  INTEGER(KIND=MPI_INTEGER_KIND), INTENT(in) :: mpi_rank ! rank of the calling process in the communicator
  INTEGER, INTENT(inout) :: nerrors                 ! number of errors
  CHARACTER(LEN=80):: dsetname ! Dataset name
  INTEGER(hsize_t), DIMENSION(1:2) :: cdims           ! chunk dimensions

  INTEGER(HID_T) :: file_id       ! File identifier
  INTEGER(HID_T) :: filespace     ! Dataspace identifier in file 
  INTEGER(HID_T) :: memspace      ! Dataspace identifier in memory
  INTEGER(HID_T) :: plist_id      ! Property list identifier 
  INTEGER(HID_T) :: dcpl_id       ! Dataset creation property list
  INTEGER(HSIZE_T), DIMENSION(1:2) :: dimsf  ! Dataset dimensions.

  INTEGER(HSIZE_T), DIMENSION(1:2) :: count  
  INTEGER(HSSIZE_T), DIMENSION(1:2) :: offset 
  INTEGER, ALLOCATABLE, DIMENSION(:,:,:), TARGET :: DATA  ! Data to write
  INTEGER, ALLOCATABLE, DIMENSION(:,:,:), TARGET :: rDATA  ! Data to write
  INTEGER, PARAMETER :: rank = 2 ! Dataset rank 
  INTEGER :: i
  INTEGER(HSIZE_T) :: ii, jj, kk, istart
  INTEGER :: error          ! Error flags

  INTEGER(SIZE_T), PARAMETER :: ndsets = 5
  INTEGER(HID_T), DIMENSION(1:ndsets) :: dset_id
  INTEGER(HID_T), DIMENSION(1:ndsets) :: mem_type_id
  INTEGER(HID_T), DIMENSION(1:ndsets) :: mem_space_id
  INTEGER(HID_T), DIMENSION(1:ndsets) :: file_space_id
  TYPE(C_PTR), DIMENSION(1:ndsets) :: buf_md
  INTEGER(SIZE_T) :: obj_count
  INTEGER :: data_xfer_mode

  dimsf(1) = 5_hsize_t
  dimsf(2) = INT(mpi_size, hsize_t)*8_hsize_t

  ! 
  ! Setup file access property list with parallel I/O access.
  !
  CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
  CALL check("h5pcreate_f", error, nerrors)
  CALL h5pset_fapl_mpio_f(plist_id, MPI_COMM_WORLD, MPI_INFO_NULL, error)
  CALL check("h5pset_fapl_mpio_f", error, nerrors)
  !
  ! Create the file collectively.
  ! 
  CALL h5fcreate_f("parf2.h5", H5F_ACC_TRUNC_F, file_id, error, access_prp = plist_id)
  CALL check("h5fcreate_f", error, nerrors)
  CALL h5pclose_f(plist_id, error)
  CALL check("h5pclose_f", error, nerrors)
  !
  ! Create the data space for the  dataset. 
  !
  CALL h5screate_simple_f(rank, dimsf, filespace, error)
  CALL check("h5screate_simple_f", error, nerrors)
  !
  ! Each process defines dataset in memory and writes it to the hyperslab
  ! in the file. 
  !
  count(1) = dimsf(1)
  count(2) = dimsf(2)/mpi_size 
  offset(1) = 0
  offset(2) = mpi_rank * count(2) 
  CALL h5screate_simple_f(rank, count, memspace, error) 
  CALL check("h5screate_simple_f", error, nerrors)

  !
  ! Modify dataset creation properties to enable chunking
  !

  CALL h5pcreate_f(H5P_DATASET_CREATE_F, dcpl_id, error)
  CALL check("h5pcreate_f", error, nerrors)

  IF (do_chunk) THEN
     cdims(1) = dimsf(1)
     cdims(2) = dimsf(2)/mpi_size/2
     CALL h5pset_chunk_f(dcpl_id, 2, cdims, error)
     CALL check("h5pset_chunk_f", error, nerrors)
  ENDIF
  ! 
  ! Select hyperslab in the file.
  !
  CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, offset, count, error)
  CALL check("h5sselect_hyperslab_f", error, nerrors)
  !
  ! Initialize data buffer
  !
  ALLOCATE ( DATA(COUNT(1),COUNT(2), ndsets))
  ALLOCATE ( rdata(COUNT(1),COUNT(2), ndsets))

  ! Create property list for collective dataset write
  !
  CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
  CALL check("h5pcreate_f", error, nerrors)
  IF(do_collective)THEN
     CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, error)
     CALL check("h5pset_dxpl_mpio_f", error, nerrors)
  ELSE
     CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_INDEPENDENT_F, error)
     CALL check("h5pset_dxpl_mpio_f", error, nerrors)
  ENDIF
     
  !
  ! Create the dataset with default properties.
  !
  mem_type_id(1:ndsets) = H5T_NATIVE_INTEGER
  mem_space_id(1:ndsets) = memspace
  file_space_id(1:ndsets)= filespace

  DO ii = 1, ndsets
     ! Create the data
     DO kk = 1, COUNT(1)
        DO jj = 1, COUNT(2)
           istart = (kk-1)*dimsf(2) + mpi_rank*COUNT(2)
           DATA(kk,jj,ii) = INT((istart + jj)*10**(ii-1))
        ENDDO
     ENDDO
     ! Point to te data
     buf_md(ii) = C_LOC(DATA(1,1,ii))

     ! direct the output of the write statement to unit "dsetname"
     WRITE(dsetname,'("dataset ",I0)') ii
     ! create the dataset
     CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, filespace, dset_id(ii), error, dcpl_id)
     CALL check("h5dcreate_f", error, nerrors)
  ENDDO

  !
  ! Write the dataset collectively. 
  !  
  CALL h5dwrite_multi_f(ndsets, dset_id, mem_type_id, mem_space_id, file_space_id, buf_md, error, plist_id)
  CALL check("h5dwrite_multi_f", error, nerrors)

  return
  CALL h5pget_dxpl_mpio_f(plist_id, data_xfer_mode, error)
  CALL check("h5pget_dxpl_mpio_f", error, nerrors)

  IF(do_collective)THEN
     IF(data_xfer_mode.NE.H5FD_MPIO_COLLECTIVE_F)THEN
        nerrors = nerrors + 1
     ENDIF
  ENDIF

  DO i = 1, ndsets
     ! Point to the read buffer
     buf_md(i) = C_LOC(rdata(1,1,i))
  ENDDO

  CALL H5Dread_multi_f(ndsets, dset_id, mem_type_id, mem_space_id, file_space_id, buf_md, error, plist_id)
  CALL check("h5dread_multi_f", error, nerrors)

  CALL h5pget_dxpl_mpio_f(plist_id, data_xfer_mode, error)
  CALL check("h5pget_dxpl_mpio_f", error, nerrors)

  IF(do_collective)THEN
     IF(data_xfer_mode.NE.H5FD_MPIO_COLLECTIVE_F)THEN
        nerrors = nerrors + 1
     ENDIF
  ENDIF

  DO i = 1, ndsets
     ! Close all the datasets
     CALL h5dclose_f(dset_id(i), error)
     CALL check("h5dclose_f", error, nerrors)
  ENDDO

  ! check the data read and write buffers
  DO ii = 1, ndsets
     ! Create the data
     DO kk = 1, COUNT(1)
        DO jj = 1, COUNT(2)
           IF(rDATA(kk,jj,ii).NE.DATA(kk,jj,ii))THEN
              nerrors = nerrors + 1
           ENDIF
        ENDDO
     ENDDO
  ENDDO
  !
  ! Deallocate data buffer.
  !
  DEALLOCATE(data, rdata)

  !
  ! Close dataspaces.
  !
  CALL h5sclose_f(filespace, error)
  CALL check("h5sclose_f", error, nerrors)
  CALL h5sclose_f(memspace, error)
  CALL check("h5sclose_f", error, nerrors)
  !
  ! Close the dataset and property list.
  !
  CALL h5pclose_f(dcpl_id, error)
  CALL check("h5pclose_f", error, nerrors)
  CALL h5pclose_f(plist_id, error)
  CALL check("h5pclose_f", error, nerrors)

  CALL h5fget_obj_count_f(file_id, H5F_OBJ_ALL_F, obj_count, error)
  IF(obj_count.NE.1)THEN
     nerrors = nerrors + 1
  END IF

  !
  ! Close the file.
  !
  CALL h5fclose_f(file_id, error)
  CALL check("h5fclose_f", error, nerrors)

END SUBROUTINE pmultiple_dset_hyper_rw
