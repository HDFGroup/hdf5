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
!
! This test writes/reads dataset by hyperslabs collectively. 

     SUBROUTINE dataset_wr_by_hyperslabs(cleanup, total_error) 
     USE THDF5
     IMPLICIT NONE
     LOGICAL, INTENT(IN) :: cleanup
     INTEGER, INTENT(OUT) :: total_error

     CHARACTER(LEN=8), PARAMETER :: filename = "par_sdsf"  ! File name
     CHARACTER(LEN=80) :: fix_filename
     CHARACTER(LEN=8), PARAMETER :: dsetname = "IntArray" ! Dataset name

     INTEGER(HID_T) :: file_id       ! File identifier 
     INTEGER(HID_T) :: dset_id       ! Dataset identifier 
     INTEGER(HID_T) :: filespace     ! Dataspace identifier in file 
     INTEGER(HID_T) :: memspace      ! Dataspace identifier in memory
     INTEGER(HID_T) :: plac_id      ! Property list identifier 
     INTEGER(HID_T) :: plxfer_id      ! Property list identifier 

     INTEGER(HSIZE_T), DIMENSION(2) :: dimsf = (/DIM1,DIM2/) ! Dataset dimensions.

     INTEGER(HSIZE_T), DIMENSION(2) :: count  
     INTEGER(HSIZE_T), DIMENSION(2) :: offset 
     INTEGER, ALLOCATABLE :: data (:,:)  ! Data to write
     INTEGER, ALLOCATABLE :: data_out (:,:)  ! Buffer to store data
     INTEGER :: rank = 2 ! Dataset rank 
     INTEGER :: i, j
     INTEGER(HSIZE_T), DIMENSION(2) :: dims
     INTEGER(HID_T) :: driver

     INTEGER :: error  ! Error flag
     !
     ! MPI definitions and calls.
     !
     INTEGER :: mpierror       ! MPI error flag
     INTEGER :: comm, info
     INTEGER :: mpi_size, mpi_rank
     comm = MPI_COMM_WORLD
     info = MPI_INFO_NULL
     CALL MPI_COMM_SIZE(comm, mpi_size, mpierror)
     CALL MPI_COMM_RANK(comm, mpi_rank, mpierror) 
     ! 
     ! Setup file access property list with parallel I/O access.
     !
     CALL h5pcreate_f(H5P_FILE_ACCESS_F, plac_id, error)
          CALL check("h5pcreate_f", error, total_error)
     CALL h5pset_fapl_mpio_f(plac_id, comm, info, error)
          CALL check("h5pset_fapl_mpio_f", error, total_error)
     CALL h5pget_driver_f(plac_id, driver, error)
          CALL check("h5pget_driver_f", error, total_error)
     if( driver .ne. H5FD_MPIO_F) then
          write(*,*) "Wrong driver information returned"
     endif
     CALL h5_fixname_f(filename, fix_filename, plac_id, error)

     !
     ! Create the file collectively.
     ! 
     CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error, access_prp = plac_id)
          CALL check("h5fcreate_f", error, total_error)
     CALL h5pclose_f(plac_id, error)
          CALL check("h5pclose_f", error, total_error)
     !
     ! Create the data space for the  dataset. 
     !
     CALL h5screate_simple_f(rank, dimsf, filespace, error)
          CALL check("h5screate_simple_f", error, total_error)

     !
     ! Create the dataset with default properties.
     !
     CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, filespace, &
                      dset_id, error)
          CALL check("h5dcreate_f", error, total_error)
     CALL h5sclose_f(filespace, error)
          CALL check("h5sclose_f", error, total_error)
     !
     ! Each process defines dataset in memory and writes it to the hyperslab
     ! in the file. 
     !
     count(1) = dimsf(1)
     count(2) = dimsf(2)/mpi_size 
     offset(1) = 0
     offset(2) = mpi_rank * count(2) 
     CALL h5screate_simple_f(rank, count, memspace, error) 
          CALL check("h5screate_simple_f", error, total_error)
     ! 
     ! Select hyperslab in the file.
     !
     CALL h5dget_space_f(dset_id, filespace, error)
          CALL check("h5dget_space_f", error, total_error)
     CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, offset, count, error)
          CALL check("h5sselect_hyperslab_f", error, total_error)
     ! 
     ! Initialize data buffer with trivial data.
     !
     ALLOCATE ( data(count(1),count(2)))
     dims(1) = count(1)
     dims(2) = count(2)
     data = mpi_rank + 10
     !
     ! Create property list for collective dataset write
     !
     CALL h5pcreate_f(H5P_DATASET_XFER_F, plxfer_id, error) 
          CALL check("h5pcreate_f", error, total_error)
     CALL h5pset_dxpl_mpio_f(plxfer_id, H5FD_MPIO_COLLECTIVE_F, error)
          CALL check("h5pset_dxpl_mpio_f", error, total_error)
     
     !
     ! Write the dataset collectively. 
     !
     CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, dims, error, &
                     file_space_id = filespace, mem_space_id = memspace, xfer_prp = plxfer_id)
          CALL check("h5dwrite_f", error, total_error)
     !
     ! Deallocate data buffer.
     !
     DEALLOCATE(data)

     !
     ! Close dataspaces.
     !
     CALL h5sclose_f(filespace, error)
          CALL check("h5sclose_f", error, total_error)
     CALL h5sclose_f(memspace, error)
          CALL check("h5sclose_f", error, total_error)

     !
     ! Close the dataset.
     !
     CALL h5dclose_f(dset_id, error)
          CALL check("h5dclose_f", error, total_error)

     CALL h5pclose_f(plxfer_id, error)
          CALL check("h5pclose_f", error, total_error)

     !
     ! Close the file.
     !
     CALL h5fclose_f(file_id, error)
          CALL check("h5fclose_f", error, total_error)
     !
     ! Reopen the file with || access.
     !
     CALL h5pcreate_f(H5P_FILE_ACCESS_F, plac_id, error)
          CALL check("h5pcreate_f", error, total_error)
     CALL h5pset_fapl_mpio_f(plac_id, comm, info, error)
          CALL check("h5pset_fapl_mpio_f", error, total_error)
     CALL h5fopen_f(fix_filename, H5F_ACC_RDWR_F, file_id, error, plac_id)
          CALL check("h5fopen_f", error, total_error)
     !
     ! Open dataset.
     !
     CALL h5dopen_f(file_id, dsetname, dset_id, error)
          CALL check("h5dopen_f", error, total_error)

     !
     ! Each process defines dataset in memory and reads hyperslab
     ! from the file. 
     !
     count(1) = dimsf(1)
     count(2) = dimsf(2)/mpi_size 
     offset(1) = 0
     offset(2) = mpi_rank * count(2) 
     CALL h5screate_simple_f(rank, count, memspace, error) 
          CALL check("h5screate_simple_f", error, total_error)
     ! 
     ! Select hyperslab in the file.
     !
     CALL h5dget_space_f(dset_id, filespace, error)
          CALL check("h5dget_space_f", error, total_error)
     CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, offset, count, error)
          CALL check("h5sselect_hyperslab_f", error, total_error)
     ! 
     ! Allocate data buffer.
     !
     ALLOCATE ( data(count(1),count(2)))
     ALLOCATE ( data_out(count(1),count(2)))
     data = mpi_rank + 10
     !
     ! Create property list for collective dataset write
     !
     CALL h5pcreate_f(H5P_DATASET_XFER_F, plxfer_id, error) 
          CALL check("h5pcreate_f", error, total_error)
     CALL h5pset_dxpl_mpio_f(plxfer_id, H5FD_MPIO_COLLECTIVE_F, error)
          CALL check("h5pset_dxpl_mpio_f", error, total_error)
     
     !
     ! Write the dataset collectively. 
     !
     CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, data_out, dims, error, &
                     file_space_id = filespace, mem_space_id = memspace, xfer_prp = plxfer_id)
          CALL check("h5dread_f", error, total_error)
     do j = 1, count(2)
      do i = 1, count(1)
        if( data(i,j) .ne. data_out(i,j)) then
              total_error = total_error + 1
              goto 100
        endif
      enddo 
     enddo 
 100 continue
     !
     ! Deallocate data buffer.
     !
     DEALLOCATE(data)
     DEALLOCATE(data_out)
     !
     ! Close dataspaces.
     !
     CALL h5sclose_f(filespace, error)
          CALL check("h5sclose_f", error, total_error)
     CALL h5sclose_f(memspace, error)
          CALL check("h5sclose_f", error, total_error)
     !
     ! Close property list.
     !
     CALL h5pclose_f(plxfer_id, error)
          CALL check("h5pclose_f", error, total_error)
     !
     ! Close dataset.
     !
     CALL h5dclose_f(dset_id, error)
          CALL check("h5dclose_f", error, total_error)
     !
     ! Close the file.
     !
     CALL h5fclose_f(file_id, error)
          CALL check("h5fclose_f", error, total_error)

          if(cleanup) CALL h5_cleanup_f(filename, plac_id, error)
              CALL check("h5_cleanup_f", error, total_error)

     CALL h5pclose_f(plac_id, error)
          CALL check("h5pclose_f", error, total_error)

     RETURN
     END SUBROUTINE dataset_wr_by_hyperslabs 
