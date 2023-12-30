!
! Number of processes is assumed to be 1 or powers of 2 (2,4,8)
!
     PROGRAM DATASET_BY_COL

     USE HDF5 ! This module contains all necessary modules

     IMPLICIT NONE

     include 'mpif.h'
     CHARACTER(LEN=10), PARAMETER :: filename = "sds_col.h5"  ! File name
     CHARACTER(LEN=8), PARAMETER :: dsetname = "IntArray" ! Dataset name

     INTEGER(HID_T) :: file_id       ! File identifier
     INTEGER(HID_T) :: dset_id       ! Dataset identifier
     INTEGER(HID_T) :: filespace     ! Dataspace identifier in file
     INTEGER(HID_T) :: memspace      ! Dataspace identifier in memory
     INTEGER(HID_T) :: plist_id      ! Property list identifier

     INTEGER(HSIZE_T), DIMENSION(2) :: dimsf = (/5,8/) ! Dataset dimensions.
!     INTEGER, DIMENSION(7) :: dimsfi = (/5,8,0,0,0,0,0/)
     INTEGER(HSIZE_T), DIMENSION(2) :: dimsfi = (/5,8/)

     INTEGER(HSIZE_T), DIMENSION(2) :: count
     INTEGER(HSSIZE_T), DIMENSION(2) :: offset
     INTEGER, ALLOCATABLE :: data (:,:)  ! Data to write
     INTEGER :: rank = 2 ! Dataset rank

     INTEGER :: error, error_n  ! Error flags
     !
     ! MPI definitions and calls.
     !
     INTEGER :: mpierror       ! MPI error flag
     INTEGER :: comm, info
     INTEGER :: mpi_size, mpi_rank
     comm = MPI_COMM_WORLD
     info = MPI_INFO_NULL
     CALL MPI_INIT(mpierror)
     CALL MPI_COMM_SIZE(comm, mpi_size, mpierror)
     CALL MPI_COMM_RANK(comm, mpi_rank, mpierror)
     !
     ! Initialize FORTRAN predefined datatypes
     !
     CALL h5open_f(error)

     !
     ! Setup file access property list with parallel I/O access.
     !
     CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
     CALL h5pset_fapl_mpio_f(plist_id, comm, info, error)

     !
     ! Create the file collectively.
     !
     CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error, access_prp = plist_id)
     CALL h5pclose_f(plist_id, error)
     !
     ! Create the data space for the  dataset.
     !
     CALL h5screate_simple_f(rank, dimsf, filespace, error)

     !
     ! Create the dataset with default properties.
     !
     CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, filespace, &
                      dset_id, error)
     CALL h5sclose_f(filespace, error)
     !
     ! Each process defines dataset in memory and writes it to the hyperslab
     ! in the file.
     !
     count(1) = dimsf(1)
     count(2) = dimsf(2)/mpi_size
     offset(1) = 0
     offset(2) = mpi_rank * count(2)
     CALL h5screate_simple_f(rank, count, memspace, error)
     !
     ! Select hyperslab in the file.
     !
     CALL h5dget_space_f(dset_id, filespace, error)
     CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, offset, count, error)
     !
     ! Initialize data buffer with trivial data.
     !
     ALLOCATE ( data(count(1),count(2)))
     data = mpi_rank + 10
     !
     ! Create property list for collective dataset write
     !
     CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
     CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, error)

     !
     ! Write the dataset collectively.
     !
     CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, dimsfi, error, &
                     file_space_id = filespace, mem_space_id = memspace, xfer_prp = plist_id)
     !
     ! Write the dataset independently.
     !
!    CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, dimsfi, error, &
!                     file_space_id = filespace, mem_space_id = memspace)
     !
     ! Deallocate data buffer.
     !
     DEALLOCATE(data)

     !
     ! Close dataspaces.
     !
     CALL h5sclose_f(filespace, error)
     CALL h5sclose_f(memspace, error)

     !
     ! Close the dataset and property list.
     !
     CALL h5dclose_f(dset_id, error)
     CALL h5pclose_f(plist_id, error)

     !
     ! Close the file.
     !
     CALL h5fclose_f(file_id, error)

     !
     ! Close FORTRAN predefined datatypes.
     !
     CALL h5close_f(error)
     IF(mpi_rank.EQ.0) WRITE(*,'(A)') "PHDF5 example finished with no errors"
     CALL MPI_FINALIZE(mpierror)

     END PROGRAM DATASET_BY_COL
