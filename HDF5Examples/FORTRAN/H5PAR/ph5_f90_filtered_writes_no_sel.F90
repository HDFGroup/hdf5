!
! Example of using the parallel HDF5 library to collectively write to
! datasets with filters applied to them when one or MPI ranks do not
! have data to contribute to the dataset.
!
! If the HDF5_NOCLEANUP environment variable is set, the file that
! this example creates will not be removed as the example finishes.
!
! The need of requirement of parallel file prefix is that in general
! the current working directory in which compiling is done, is not suitable
! for parallel I/O and there is no standard pathname for parallel file
! systems. In some cases, the parallel file name may even need some
! parallel file type prefix such as: "pfs:/GF/...".  Therefore, this
! example parses the HDF5_PARAPREFIX environment variable for a prefix,
! if one is needed.

MODULE filter
  USE HDF5
  USE MPI

  IMPLICIT NONE

  CHARACTER(LEN=29), PARAMETER :: EXAMPLE_FILE = "ph5_filtered_writes_no_sel.h5"
  INTEGER          , PARAMETER :: EXAMPLE_DSET_DIMS = 2
  CHARACTER(LEN=4) , PARAMETER :: EXAMPLE_DSET_NAME = "DSET"
  INTEGER          , PARAMETER :: EXAMPLE_DSET_CHUNK_DIM_SIZE = 10
  INTEGER          , PARAMETER :: PATH_MAX = 512

  ! Global variables 
  INTEGER(KIND=MPI_INTEGER_KIND) :: mpi_rank, mpi_size

CONTAINS
  !
  ! Routine to set an HDF5 filter on the given DCPL
  !
  SUBROUTINE set_filter(dcpl_id)

    IMPLICIT NONE
    INTEGER(HID_T) :: dcpl_id
    LOGICAL :: filter_avail
    INTEGER :: status

    !
    ! Check if 'deflate' filter is available
    !
    CALL H5Zfilter_avail_f(H5Z_FILTER_DEFLATE_F, filter_avail, status)
    IF(status .LT. 0)THEN
       RETURN
    ELSE IF(filter_avail)THEN
       !
       ! Set 'deflate' filter with reasonable
       ! compression level on DCPL

       CALL H5Pset_deflate_f(dcpl_id, 6, status)
    ELSE
       !
       ! Set Fletcher32 checksum filter on DCPL
       ! since it is always available in HDF5
       CALL H5Pset_fletcher32_f(dcpl_id, status)
    ENDIF
  END SUBROUTINE set_filter
  !
  ! Routine to fill a data buffer with data. Assumes
  ! dimension rank is 2 and data is stored contiguous.


  SUBROUTINE fill_databuf(start, count, stride, wdata)

    IMPLICIT NONE
    INTEGER(HSIZE_T), DIMENSION(*) :: start, count, stride
    INTEGER, DIMENSION(*) :: wdata
    INTEGER(HSIZE_T) :: i, j, icnt

    ! Use MPI rank value for data
    icnt = 1
    DO i = 1, COUNT(1)
       DO j = 1, COUNT(2)
          wdata(icnt) = mpi_rank
          icnt = icnt + 1
       ENDDO
    ENDDO

  END SUBROUTINE fill_databuf
  !
  ! Cleanup created files
  !
  SUBROUTINE cleanup(filename)

    IMPLICIT NONE
    CHARACTER(*) :: filename

    LOGICAL :: do_cleanup
    INTEGER :: status
    INTEGER(KIND=MPI_INTEGER_KIND) :: mpierror

    CALL get_environment_variable("HDF5_NOCLEANUP", STATUS=status)
    IF(status.EQ.0)THEN
       CALL MPI_File_delete(filename, MPI_INFO_NULL, mpierror)
    ENDIF

  END SUBROUTINE cleanup
  !
  ! Routine to write to a dataset in a fashion
  ! where no chunks in the dataset are written
  ! to by more than 1 MPI rank. This will
  ! generally give the best performance as the
  ! MPI ranks will need the least amount of
  ! inter-process communication.

  SUBROUTINE write_dataset_some_no_sel(file_id, dxpl_id)

    IMPLICIT NONE
    INTEGER(HID_T) :: file_id, dxpl_id

    INTEGER, DIMENSION(1:EXAMPLE_DSET_CHUNK_DIM_SIZE, 4*EXAMPLE_DSET_CHUNK_DIM_SIZE), TARGET :: wdata
    INTEGER(hsize_t), DIMENSION(1:EXAMPLE_DSET_DIMS) :: dataset_dims
    INTEGER(hsize_t), DIMENSION(1:EXAMPLE_DSET_DIMS) :: chunk_dims
    INTEGER(hsize_t), DIMENSION(1:EXAMPLE_DSET_DIMS) :: start
    INTEGER(hsize_t), DIMENSION(1:EXAMPLE_DSET_DIMS) :: stride
    INTEGER(hsize_t), DIMENSION(1:EXAMPLE_DSET_DIMS) :: count
    LOGICAL        :: no_selection   = .FALSE.
    INTEGER(hid_t) :: dset_id
    INTEGER(hid_t) :: dcpl_id
    INTEGER(hid_t) :: file_dataspace
    INTEGER(hid_t) :: sel_type
    TYPE(C_PTR) :: f_ptr
    INTEGER :: status

    !
    ! ------------------------------------
    ! Setup Dataset Creation Property List
    ! ------------------------------------

    CALL H5Pcreate_f(H5P_DATASET_CREATE_F, dcpl_id, status)

    !
    ! REQUIRED: Dataset chunking must be enabled to
    !           apply a data filter to the dataset.
    !           Chunks in the dataset are of size
    !           EXAMPLE_DSET_CHUNK_DIM_SIZE x EXAMPLE_DSET_CHUNK_DIM_SIZE.
     
    chunk_dims(1) = EXAMPLE_DSET_CHUNK_DIM_SIZE
    chunk_dims(2) = EXAMPLE_DSET_CHUNK_DIM_SIZE
    CALL H5Pset_chunk_f(dcpl_id, EXAMPLE_DSET_DIMS, chunk_dims, status)

    ! Set filter to be applied to created datasets 
    CALL set_filter(dcpl_id)

    !
    ! ------------------------------------
    ! Define the dimensions of the dataset
    ! and create it
    ! ------------------------------------

    ! Create a dataset composed of 4 chunks
    ! per MPI rank. The first dataset dimension
    ! scales according to the number of MPI ranks.
    ! The second dataset dimension stays fixed
    ! according to the chunk size.
     
    dataset_dims(1) = EXAMPLE_DSET_CHUNK_DIM_SIZE * mpi_size
    dataset_dims(2) = 4 * EXAMPLE_DSET_CHUNK_DIM_SIZE

    CALL H5Screate_simple_f(EXAMPLE_DSET_DIMS, dataset_dims, file_dataspace, status)

    ! Create the dataset 
    CALL H5Dcreate_f(file_id, EXAMPLE_DSET_NAME, H5T_NATIVE_INTEGER, file_dataspace, dset_id, status, dcpl_id=dcpl_id)

    !
    ! ------------------------------------
    ! Setup selection in the dataset for
    ! each MPI rank
    ! ------------------------------------

    !
    ! Odd rank value MPI ranks do not
    ! contribute any data to the dataset.
     
    IF(MOD(mpi_rank, 2) .NE. 0) no_selection = .TRUE.

    IF(no_selection)THEN
       !
       ! MPI ranks not contributing data to
       ! the dataset should call H5Sselect_none
       ! on the file dataspace that will be
       ! passed to H5Dwrite.
         
       CALL H5Sselect_none_f(file_dataspace, status)
       sel_type = H5S_BLOCK_F
    ELSE
        !
        ! Even MPI ranks contribute data to
        ! the dataset. Each MPI rank's selection
        ! covers a single chunk in the first dataset
        ! dimension. Each MPI rank's selection
        ! covers 4 chunks in the second dataset
        ! dimension. This leads to each contributing
        ! MPI rank writing to 4 chunks of the dataset.
         
        start(1)  = mpi_rank * EXAMPLE_DSET_CHUNK_DIM_SIZE
        start(2)  = 0
        stride(1) = 1
        stride(2) = 1
        count(1)  = EXAMPLE_DSET_CHUNK_DIM_SIZE
        count(2)  = 4 * EXAMPLE_DSET_CHUNK_DIM_SIZE

        CALL H5Sselect_hyperslab_f(file_dataspace, H5S_SELECT_SET_F, start, count, status, stride=stride)

        sel_type = H5S_ALL_F
        !
        ! --------------------------------------
        ! Fill data buffer with MPI rank's rank
        ! value to make it easy to see which
        ! part of the dataset each rank wrote to
        ! --------------------------------------

        CALL fill_databuf(start, count, stride, wdata)
     ENDIF

     !
     ! ---------------------------------
     ! Write to the dataset collectively
     ! ---------------------------------
     f_ptr = C_LOC(wdata)
     CALL H5Dwrite_f(dset_id, H5T_NATIVE_INTEGER, f_ptr, status, &
          mem_space_id=sel_type, file_space_id=file_dataspace, xfer_prp=dxpl_id)

     !
     ! --------------
     ! Close HDF5 IDs
     ! --------------

     CALL H5Sclose_f(file_dataspace,status)
     CALL H5Pclose_f(dcpl_id,status)
     CALL H5Dclose_f(dset_id,status)

   END SUBROUTINE write_dataset_some_no_sel
 END MODULE filter

 PROGRAM main

   USE filter
   IMPLICIT NONE

   INTEGER(KIND=MPI_INTEGER_KIND) :: comm = MPI_COMM_WORLD
   INTEGER(KIND=MPI_INTEGER_KIND) :: info = MPI_INFO_NULL
   INTEGER(hid_t) :: file_id
   INTEGER(hid_t) :: fapl_id
   INTEGER(hid_t) :: dxpl_id
   CHARACTER(LEN=PATH_MAX) :: par_prefix
   CHARACTER(LEN=PATH_MAX) :: filename
   INTEGER :: status
   INTEGER(KIND=MPI_INTEGER_KIND) :: mpierror

   CALL MPI_Init(mpierror)
   CALL MPI_Comm_size(comm, mpi_size, mpierror)
   CALL MPI_Comm_rank(comm, mpi_rank, mpierror)

  !
  ! Initialize HDF5 library and Fortran interfaces.
  !
  CALL h5open_f(status)
  !
  ! ----------------------------------
  ! Start parallel access to HDF5 file
  ! ----------------------------------

  ! Setup File Access Property List with parallel I/O access
  CALL H5Pcreate_f(H5P_FILE_ACCESS_F, fapl_id, status)
  CALL H5Pset_fapl_mpio_f(fapl_id, comm, info, status)

  !
  ! OPTIONAL: Set collective metadata reads on FAPL to allow
  !           parallel writes to filtered datasets to perform
  !           better at scale. While not strictly necessary,
  !           this is generally recommended.
     
  CALL H5Pset_all_coll_metadata_ops_f(fapl_id, .TRUE., status)

  !
  ! OPTIONAL: Set the latest file format version for HDF5 in
  !           order to gain access to different dataset chunk
  !           index types and better data encoding methods.
  !           While not strictly necessary, this is generally
  !           recommended.
     
  CALL H5Pset_libver_bounds_f(fapl_id, H5F_LIBVER_LATEST_F, H5F_LIBVER_LATEST_F, status)

  ! Parse any parallel prefix and create filename
  par_prefix(:) = ""
  CALL get_environment_variable("HDF5_PARAPREFIX", VALUE=par_prefix, STATUS=status)
  filename = TRIM(par_prefix)//EXAMPLE_FILE

  ! Create HDF5 file
  CALL H5Fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, status, access_prp = fapl_id)

  !
  ! --------------------------------------
  ! Setup Dataset Transfer Property List
  ! with collective I/O
  ! --------------------------------------
     

  CALL H5Pcreate_f(H5P_DATASET_XFER_F, dxpl_id, status)

  !
  ! REQUIRED: Setup collective I/O for the dataset
  !           write operations. Parallel writes to
  !           filtered datasets MUST be collective,
  !           even if some ranks have no data to
  !           contribute to the write operation.

  CALL H5Pset_dxpl_mpio_f(dxpl_id, H5FD_MPIO_COLLECTIVE_F, status)

  !
  ! --------------------------------
  ! Create and write to the dataset
  ! --------------------------------
     
  !
  ! Write to a dataset in a fashion where no
  ! chunks in the dataset are written to by
  ! more than 1 MPI rank and some MPI ranks
  ! have nothing to contribute to the dataset.
  ! In this case, the MPI ranks that have no
  ! data to contribute must still participate
  ! in the collective H5Dwrite call, but should
  ! call H5Sselect_none on the file dataspace
  ! passed to the H5Dwrite call.

  CALL write_dataset_some_no_sel(file_id, dxpl_id)

  !
  ! ------------------
  ! Close all HDF5 IDs
  ! ------------------

  CALL H5Pclose_f(dxpl_id, status)
  CALL H5Pclose_f(fapl_id, status)
  CALL H5Fclose_f(file_id, status)
  !
  ! Close FORTRAN interfaces and HDF5 library.
  !
  CALL h5close_f(status)

  IF(mpi_rank .EQ. 0) WRITE(*,"(A)") "PHDF5 example finished with no errors"

  !
  ! ------------------------------------
  ! Cleanup created HDF5 file and finish
  ! ------------------------------------
  CALL cleanup(filename)

  CALL MPI_Finalize(mpierror)

END PROGRAM main
