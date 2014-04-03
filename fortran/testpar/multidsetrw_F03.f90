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
! writes/reads dataset by hyperslabs using multi-dataset routines, h5dread_multi and
! h5dwrite_multi
!

SUBROUTINE pmultiple_dset_hyper_rw(do_collective, do_chunk, mpi_size, mpi_rank, nerrors)

  USE iso_c_binding
  USE hdf5
  USE mpi
  IMPLICIT NONE

  LOGICAL, INTENT(in) :: do_collective              ! use collective IO
  LOGICAL, INTENT(in) :: do_chunk                   ! use chunking
  INTEGER, INTENT(in) :: mpi_size                   ! number of processes in the group of communicator
  INTEGER, INTENT(in) :: mpi_rank                   ! rank of the calling process in the communicator
  INTEGER, INTENT(inout) :: nerrors                 ! number of errors
  CHARACTER(LEN=80):: dsetname ! Dataset name
  TYPE(H5D_rw_multi_t), ALLOCATABLE, DIMENSION(:) :: info_md
  INTEGER(hsize_t), DIMENSION(1:2) :: cdims           ! chunk dimensions

  INTEGER(HID_T) :: file_id       ! File identifier
  INTEGER(HID_T) :: filespace     ! Dataspace identifier in file 
  INTEGER(HID_T) :: memspace      ! Dataspace identifier in memory
  INTEGER(HID_T) :: plist_id      ! Property list identifier 
  INTEGER(HID_T) :: dcpl_id       ! Dataset creation property list
  INTEGER(HSIZE_T), DIMENSION(1:2) :: dimsf  ! Dataset dimensions.
  INTEGER(HSIZE_T), DIMENSION(1:2) :: dimsfi = (/5,8/)

  INTEGER(HSIZE_T), DIMENSION(1:2) :: count  
  INTEGER(HSSIZE_T), DIMENSION(1:2) :: offset 
  INTEGER, ALLOCATABLE, DIMENSION(:,:,:), TARGET :: DATA  ! Data to write
  INTEGER, ALLOCATABLE, DIMENSION(:,:,:), TARGET :: rDATA  ! Data to write
  INTEGER, PARAMETER :: rank = 2 ! Dataset rank 
  INTEGER :: i, j, k, istart
  INTEGER :: error          ! Error flags

  dimsf = (/5,mpi_size*8/)

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
  ALLOCATE ( DATA(COUNT(1),COUNT(2), mpi_size))
  ALLOCATE ( rdata(COUNT(1),COUNT(2), mpi_size))

  ALLOCATE(info_md(1:mpi_size))

  !
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
  info_md(1:mpi_size)%mem_type_id = H5T_NATIVE_INTEGER
  info_md(1:mpi_size)%mem_space_id = memspace
  info_md(1:mpi_size)%dset_space_id = filespace

  DO i = 1, mpi_size
     ! Create the data
     DO k = 1, COUNT(1)
        DO j = 1, COUNT(2)
           istart = (k-1)*dimsf(2) + mpi_rank*COUNT(2)
           DATA(k,j,i) = (istart + j)*10**(i-1)
        ENDDO
     ENDDO
     ! Point to te data
     info_md(i)%buf = C_LOC(DATA(1,1,i))

     ! direct the output of the write statement to unit "dsetname"
     WRITE(dsetname,'("dataset ",I0)') i
     ! create the dataset
     CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, filespace, info_md(i)%dset_id, error, dcpl_id)
     CALL check("h5dcreate_f", error, nerrors)
  ENDDO
  !
  ! Write the dataset collectively. 
  !
  CALL h5dwrite_multi_f(file_id, plist_id, mpi_size, info_md, error)
  CALL check("h5dwrite_multi_f", error, nerrors)

  DO i = 1, mpi_size
     ! Point to the read buffer
     info_md(i)%buf = C_LOC(rdata(1,1,i))
  ENDDO

  CALL H5Dread_multi_f(file_id, plist_id, mpi_size, info_md, error)
  CALL check("h5dread_multi_f", error, nerrors)

  DO i = 1, mpi_size
     ! Close all the datasets
     CALL h5dclose_f(info_md(i)%dset_id, error)
     CALL check("h5dclose_f", error, nerrors)
  ENDDO

  ! check the data read and write buffers
  DO i = 1, mpi_size
     ! Create the data
     DO k = 1, COUNT(1)
        DO j = 1, COUNT(2)
           IF(rDATA(k,j,i).NE.DATA(k,j,i))THEN
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
  CALL h5pclose_f(plist_id, error)
  CALL check("h5pclose_f", error, nerrors)

  !
  ! Close the file.
  !
  CALL h5fclose_f(file_id, error)
  CALL check("h5fclose_f", error, nerrors)

END SUBROUTINE pmultiple_dset_hyper_rw
