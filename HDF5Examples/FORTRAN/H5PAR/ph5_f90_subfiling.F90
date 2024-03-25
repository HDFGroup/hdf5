!
! Example of using HDF5's Subfiling VFD to write to an
! HDF5 file that is striped across multiple subfiles
!
! If the HDF5_NOCLEANUP environment variable is set, the
! files that this example creates will not be removed as
! the example finishes.
!
! In general, the current working directory in which compiling
! is done, is not suitable for parallel I/O and there is no
! standard pathname for parallel file systems. In some cases,
! the parallel file name may even need some parallel file type
! prefix such as: "pfs:/GF/...".  Therefore, this example parses
! the HDF5_PARAPREFIX environment variable for a prefix, if one
! is needed.
!

MODULE subf

  USE HDF5
  USE MPI

  CHARACTER(LEN=31), PARAMETER :: EXAMPLE_FILE  = "h5_subfiling_default_example.h5"
  CHARACTER(LEN=30), PARAMETER :: EXAMPLE_FILE2 = "h5_subfiling_custom_example.h5"
  CHARACTER(LEN=33), PARAMETER :: EXAMPLE_FILE3 = "h5_subfiling_precreate_example.h5"

  CHARACTER(LEN=4), PARAMETER :: EXAMPLE_DSET_NAME = "DSET"
  INTEGER         , PARAMETER :: EXAMPLE_DSET_DIMS = 2

  ! Have each MPI rank write 16MiB of data
  INTEGER, PARAMETER :: EXAMPLE_DSET_NY = 4194304

CONTAINS

  ! Cleanup created files

  SUBROUTINE cleanup(filename, fapl_id)

    IMPLICIT NONE
    INTEGER(HID_T) :: fapl_id
    CHARACTER(*) :: filename

    LOGICAL :: do_cleanup
    INTEGER :: status

    CALL get_environment_variable("HDF5_NOCLEANUP", STATUS=status)
    !IF(status.EQ.0) CALL H5Fdelete_f(filename, fapl_id, status)
    IF(status.EQ.0)THEN
       OPEN(UNIT=15, IOSTAT=status, FILE=filename, STATUS='old')
       IF(status .EQ. 0) CLOSE(15, STATUS='DELETE')
    ENDIF

  END SUBROUTINE cleanup

    ! An example of using the HDF5 Subfiling VFD with
    ! its default settings of 1 subfile per node, with
    ! a stripe size of 32MiB

  SUBROUTINE subfiling_write_default(fapl_id, mpi_size, mpi_rank)

    IMPLICIT NONE
    INTEGER(HID_T) :: fapl_id
    INTEGER(KIND=MPI_INTEGER_KIND) :: mpi_size
    INTEGER(KIND=MPI_INTEGER_KIND) :: mpi_rank

    INTEGER, DIMENSION(:), ALLOCATABLE, TARGET  :: wdata
    INTEGER(hsize_t), DIMENSION(1:EXAMPLE_DSET_DIMS) :: dset_dims
    INTEGER(hsize_t), DIMENSION(1:EXAMPLE_DSET_DIMS) :: start
    INTEGER(hsize_t), DIMENSION(1:EXAMPLE_DSET_DIMS) :: count
    INTEGER(hid_t) :: file_id
    INTEGER(hid_t) :: subfiling_fapl
    INTEGER(hid_t) :: dset_id
    INTEGER(hid_t) :: filespace
    CHARACTER(LEN=512) :: filename, par_prefix
    INTEGER :: status
    INTEGER(SIZE_T) :: i
    TYPE(C_PTR) :: f_ptr

    !
    ! Make a copy of the FAPL so we don't disturb
    ! it for the other examples
    !
    CALL H5Pcopy_f(fapl_id, subfiling_fapl, status)

    !
    ! Set Subfiling VFD on FAPL using default settings
    ! (use IOC VFD, 1 IOC per node, 32MiB stripe size)
    !
    ! Note that all of Subfiling's configuration settings
    ! can be adjusted with environment variables as well
    ! in this case.
    !

    CALL H5Pset_fapl_subfiling_f(subfiling_fapl, status)

    !
    ! OPTIONAL: Set alignment of objects in HDF5 file to
    !           be equal to the Subfiling stripe size.
    !           Choosing a Subfiling stripe size and HDF5
    !           object alignment value that are some
    !           multiple of the disk block size can
    !           generally help performance by ensuring
    !           that I/O is well-aligned and doesn't
    !           excessively cross stripe boundaries.
    !
    !           Note that this option can substantially
    !           increase the size of the resulting HDF5
    !           files, so it is a good idea to keep an eye
    !           on this.
    !

    CALL H5Pset_alignment_f(subfiling_fapl, 0_HSIZE_T, 33554432_HSIZE_T, status) ! ALIGN to default 32MiB stripe size

    ! Parse any parallel prefix and create filename
    par_prefix(:) = ""
    CALL get_environment_variable("HDF5_PARAPREFIX", VALUE=par_prefix, STATUS=status)
    filename = TRIM(par_prefix)//EXAMPLE_FILE

    ! Create a new file collectively
    CALL H5Fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, status, access_prp = subfiling_fapl)

    ! Create the dataspace for the dataset. The second
    ! dimension varies with the number of MPI ranks
    ! while the first dimension is fixed.

    dset_dims(1) = EXAMPLE_DSET_NY
    dset_dims(2) = mpi_size
    CALL H5Screate_simple_f(EXAMPLE_DSET_DIMS, dset_dims, filespace, status)

    ! Create the dataset with default properties
     
    CALL H5Dcreate_f(file_id, EXAMPLE_DSET_NAME, H5T_NATIVE_INTEGER, filespace, dset_id, status)
    ! Each MPI rank writes from a contiguous memory
    ! region to the hyperslab in the file
     
    start(1) = 0
    start(2) = mpi_rank
    count(1) = dset_dims(1)
    count(2) = 1
    CALL H5Sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, start, count, status)

    ! Initialize data buffer
    ALLOCATE(wdata(COUNT(1)*COUNT(2)))
    DO i = 1, COUNT(1)*COUNT(2)
       wdata(i) = mpi_rank
    ENDDO

    ! Write to dataset
    f_ptr = C_LOC(wdata)
    CALL H5Dwrite_f(dset_id, H5T_NATIVE_INTEGER, f_ptr, status, mem_space_id=H5S_BLOCK_F, file_space_id=filespace)

    ! Close/release resources.
    DEALLOCATE(wdata)
    CALL H5Dclose_f(dset_id, status)
    CALL H5Sclose_f(filespace, status)

    CALL H5Fclose_f(file_id, status)

    CALL cleanup(EXAMPLE_FILE, subfiling_fapl)

    CALL H5Pclose_f(subfiling_fapl, status)

  END SUBROUTINE subfiling_write_default

  !
  ! An example of using the HDF5 Subfiling VFD with
  ! custom settings
  !

  SUBROUTINE subfiling_write_custom(fapl_id, mpi_size, mpi_rank)

    IMPLICIT NONE
    INTEGER(HID_T) :: fapl_id
    INTEGER(KIND=MPI_INTEGER_KIND) :: mpi_size
    INTEGER(KIND=MPI_INTEGER_KIND) :: mpi_rank

    INTEGER, DIMENSION(:), ALLOCATABLE, TARGET  :: wdata

    TYPE(H5FD_subfiling_config_t) :: subf_config
    TYPE(H5FD_ioc_config_t)       :: ioc_config
    INTEGER(hsize_t), DIMENSION(1:EXAMPLE_DSET_DIMS) :: dset_dims
    INTEGER(hsize_t), DIMENSION(1:EXAMPLE_DSET_DIMS) :: start
    INTEGER(hsize_t), DIMENSION(1:EXAMPLE_DSET_DIMS) :: count
    INTEGER(hid_t) :: file_id
    INTEGER(hid_t) :: subfiling_fapl
    INTEGER(hid_t) :: dset_id
    INTEGER(hid_t) :: filespace
    CHARACTER(LEN=512) :: filename, par_prefix
    INTEGER :: status
    INTEGER(SIZE_T) :: i
    TYPE(C_PTR) :: f_ptr

    ! Make a copy of the FAPL so we don't disturb
    ! it for the other examples

    CALL H5Pcopy_f(fapl_id, subfiling_fapl, status)

    ! Get a default Subfiling and IOC configuration
    CALL h5pget_fapl_subfiling_f(subfiling_fapl, subf_config, status)
    CALL h5pget_fapl_ioc_f(subfiling_fapl,ioc_config, status)

    ! Set Subfiling configuration to use a 1MiB
    ! stripe size and the SELECT_IOC_EVERY_NTH_RANK
    ! selection method. By default, without a setting
    ! in the H5FD_SUBFILING_IOC_SELECTION_CRITERIA
    ! environment variable, this will use every MPI
    ! rank as an I/O concentrator.
     
    subf_config%shared_cfg%stripe_size   = 1048576
    subf_config%shared_cfg%ioc_selection = SELECT_IOC_EVERY_NTH_RANK_F

    ! Set IOC configuration to use 2 worker threads
    ! per IOC instead of the default setting and
    ! update IOC configuration with new subfiling
    ! configuration.
     
    ioc_config%thread_pool_size = 2

    ! Set our new configuration on the IOC
    ! FAPL used for Subfiling
     
    CALL H5Pset_fapl_ioc_f(subf_config%ioc_fapl_id, status, ioc_config)
    
    ! Finally, set our new Subfiling configuration
    ! on the original FAPL
     
    CALL H5Pset_fapl_subfiling_f(subfiling_fapl, status, subf_config)
    !
    ! OPTIONAL: Set alignment of objects in HDF5 file to
    !           be equal to the Subfiling stripe size.
    !           Choosing a Subfiling stripe size and HDF5
    !           object alignment value that are some
    !           multiple of the disk block size can
    !           generally help performance by ensuring
    !           that I/O is well-aligned and doesn't
    !           excessively cross stripe boundaries.
    !
    !           Note that this option can substantially
    !           increase the size of the resulting HDF5
    !           files, so it is a good idea to keep an eye
    !           on this.
    !

    CALL H5Pset_alignment_f(subfiling_fapl, 0_HSIZE_T, 33554432_HSIZE_T, status) ! ALIGN to default 32MiB stripe size

    ! Parse any parallel prefix and create filename
    par_prefix(:) = ""
    CALL get_environment_variable("HDF5_PARAPREFIX", VALUE=par_prefix, STATUS=status)
    filename = TRIM(par_prefix)//EXAMPLE_FILE

    ! Create a new file collectively
    CALL H5Fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, status, access_prp = subfiling_fapl)

    ! Create the dataspace for the dataset. The second
    ! dimension varies with the number of MPI ranks
    ! while the first dimension is fixed.

    dset_dims(1) = EXAMPLE_DSET_NY
    dset_dims(2) = mpi_size
    CALL H5Screate_simple_f(EXAMPLE_DSET_DIMS, dset_dims, filespace, status)

    ! Create the dataset with default properties
     
    CALL H5Dcreate_f(file_id, EXAMPLE_DSET_NAME, H5T_NATIVE_INTEGER, filespace, dset_id, status)
    ! Each MPI rank writes from a contiguous memory
    ! region to the hyperslab in the file
     
    start(1) = 0
    start(2) = mpi_rank
    count(1) = dset_dims(1)
    count(2) = 1
    CALL H5Sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, start, count, status)

    ! Initialize data buffer
    ALLOCATE(wdata(COUNT(1)*COUNT(2)))
    DO i = 1, COUNT(1)*COUNT(2)
       wdata(i) = mpi_rank
    ENDDO

    ! Write to dataset
    f_ptr = C_LOC(wdata)
    CALL H5Dwrite_f(dset_id, H5T_NATIVE_INTEGER, f_ptr, status, mem_space_id=H5S_BLOCK_F, file_space_id=filespace)

    ! Close/release resources.
    DEALLOCATE(wdata)
    CALL H5Dclose_f(dset_id, status)
    CALL H5Sclose_f(filespace, status)

    CALL H5Fclose_f(file_id, status)

    CALL cleanup(EXAMPLE_FILE, subfiling_fapl)

    CALL H5Pclose_f(subfiling_fapl, status)

  END SUBROUTINE subfiling_write_custom

  !
  ! An example of pre-creating an HDF5 file on MPI rank
  ! 0 when using the HDF5 Subfiling VFD. In this case,
  ! the subfiling stripe count must be set so that rank
  ! 0 knows how many subfiles to pre-create.

  SUBROUTINE subfiling_write_precreate(fapl_id, mpi_size, mpi_rank)

    IMPLICIT NONE
    INTEGER(HID_T) :: fapl_id
    INTEGER(KIND=MPI_INTEGER_KIND) :: mpi_size
    INTEGER(KIND=MPI_INTEGER_KIND) :: mpi_rank

    INTEGER, DIMENSION(:), ALLOCATABLE, TARGET  :: wdata
    TYPE(H5FD_subfiling_config_t) :: subf_config
    INTEGER(hsize_t), DIMENSION(1:EXAMPLE_DSET_DIMS) :: dset_dims
    INTEGER(hsize_t), DIMENSION(1:EXAMPLE_DSET_DIMS) :: start
    INTEGER(hsize_t), DIMENSION(1:EXAMPLE_DSET_DIMS) :: count
    INTEGER(hid_t) :: file_id
    INTEGER(hid_t) :: subfiling_fapl
    INTEGER(hid_t) :: dset_id
    INTEGER(hid_t) :: filespace
    CHARACTER(LEN=512) :: filename, par_prefix
    INTEGER :: status
    INTEGER(SIZE_T) :: i
    TYPE(C_PTR) :: f_ptr
    INTEGER(KIND=MPI_INTEGER_KIND) :: mpierror

    ! Make a copy of the FAPL so we don't disturb
    ! it for the other examples

    CALL H5Pcopy_f(fapl_id, subfiling_fapl, status)

    ! Get a default Subfiling and IOC configuration
    CALL h5pget_fapl_subfiling_f(subfiling_fapl, subf_config, status)

    !
    ! Set the Subfiling stripe count so that rank
    ! 0 knows how many subfiles the logical HDF5
    ! file should consist of. In this case, use
    ! 5 subfiles with a default stripe size of
    ! 32MiB.

    subf_config%shared_cfg%stripe_count = 5
    !
    ! OPTIONAL: Set alignment of objects in HDF5 file to
    !           be equal to the Subfiling stripe size.
    !           Choosing a Subfiling stripe size and HDF5
    !           object alignment value that are some
    !           multiple of the disk block size can
    !           generally help performance by ensuring
    !           that I/O is well-aligned and doesn't
    !           excessively cross stripe boundaries.
    !
    !           Note that this option can substantially
    !           increase the size of the resulting HDF5
    !           files, so it is a good idea to keep an eye
    !           on this.
    !

    CALL H5Pset_alignment_f(subfiling_fapl, 0_HSIZE_T, 1048576_HSIZE_T, status) ! Align to custom 1MiB stripe size

    ! Parse any parallel prefix and create filename
    par_prefix(:) = ""
    CALL get_environment_variable("HDF5_PARAPREFIX", VALUE=par_prefix, STATUS=status)
    filename = TRIM(par_prefix)//EXAMPLE_FILE

    ! Set dataset dimensionality
    dset_dims(1) = EXAMPLE_DSET_NY
    dset_dims(2) = mpi_size

    IF (mpi_rank .EQ. 0) THEN
       !
       ! Make sure only this rank opens the file
       !
       CALL H5Pset_mpi_params_f(subfiling_fapl, MPI_COMM_SELF, MPI_INFO_NULL, status)

       !
       ! Set the Subfiling VFD on our FAPL using
       ! our custom configuration
       !
       CALL H5Pset_fapl_subfiling_f(subfiling_fapl, status, subf_config);

       !
       ! Create a new file on rank 0
       !
       CALL H5Fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, status, access_prp = subfiling_fapl)

       ! Create the dataspace for the dataset. The second
       ! dimension varies with the number of MPI ranks
       ! while the first dimension is fixed.
       !
       CALL H5Screate_simple_f(EXAMPLE_DSET_DIMS, dset_dims, filespace, status)

       ! Create the dataset with default properties
     
       CALL H5Dcreate_f(file_id, EXAMPLE_DSET_NAME, H5T_NATIVE_INTEGER, filespace, dset_id, status)

       ! Initialize data buffer
       ALLOCATE(wdata(dset_dims(1)*dset_dims(2)))
       DO i = 1, dset_dims(1)*dset_dims(2)
          wdata(i) = i
       ENDDO

       !
       ! Rank 0 writes to the whole dataset
       !
       f_ptr = C_LOC(wdata)
       CALL H5Dwrite_f(dset_id, H5T_NATIVE_INTEGER, f_ptr, status, mem_space_id=H5S_BLOCK_F, file_space_id=filespace)

       !
       ! Close/release resources.
       !
       DEALLOCATE(wdata)
       CALL H5Dclose_f(dset_id, status)
       CALL H5Sclose_f(filespace, status)

       CALL H5Fclose_f(file_id, status)
    ENDIF

    CALL MPI_Barrier(MPI_COMM_WORLD, mpierror)

    !
    ! Use all MPI ranks to re-open the file and
    ! read back the dataset that was created
    !
    CALL H5Pset_mpi_params_f(subfiling_fapl, MPI_COMM_WORLD, MPI_INFO_NULL, status)

    !
    ! Use the same subfiling configuration as rank 0
    ! used to create the file
    !
    CALL H5Pset_fapl_subfiling_f(subfiling_fapl, status, subf_config)

    !
    ! Re-open the file on all ranks
    !

    CALL H5Fopen_f(filename, H5F_ACC_RDONLY_F, file_id, status, access_prp=subfiling_fapl)

    !
    ! Open the dataset that was created
    !
    CALL H5Dopen_f(file_id, EXAMPLE_DSET_NAME, dset_id, status)

    !
    ! Initialize data buffer
    !

    ALLOCATE(wdata(dset_dims(1)*dset_dims(2)))
    !
    ! Read the dataset on all ranks
    !
    f_ptr = C_LOC(wdata)
    CALL H5Dread_f(dset_id, H5T_NATIVE_INTEGER, f_ptr, status, mem_space_id=H5S_BLOCK_F, file_space_id=H5S_ALL_F)

    DEALLOCATE(wdata)

    CALL H5Dclose_f(dset_id, status)
    CALL H5Fclose_f(file_id, status)

    CALL cleanup(EXAMPLE_FILE, subfiling_fapl)

    CALL H5Pclose_f(subfiling_fapl, status)

  END SUBROUTINE subfiling_write_precreate

END MODULE subf

PROGRAM main

  USE SUBF
  IMPLICIT NONE

  INTEGER(KIND=MPI_INTEGER_KIND) :: comm = MPI_COMM_WORLD
  INTEGER(KIND=MPI_INTEGER_KIND) :: info = MPI_INFO_NULL
  INTEGER(HID_T) :: fapl_id
  INTEGER(KIND=MPI_INTEGER_KIND) :: mpi_size
  INTEGER(KIND=MPI_INTEGER_KIND) :: mpi_rank
  INTEGER(KIND=MPI_INTEGER_KIND) :: required
  INTEGER(KIND=MPI_INTEGER_KIND) :: provided
  INTEGER(KIND=MPI_INTEGER_KIND) :: mpierror
  INTEGER :: status

  ! HDF5 Subfiling VFD requires MPI_Init_thread with MPI_THREAD_MULTIPLE
  required = MPI_THREAD_MULTIPLE
  provided = 0
  CALL mpi_init_thread(required, provided, mpierror)
  IF (provided .NE. required) THEN
     WRITE(*,*) "MPI doesn't support MPI_Init_thread with MPI_THREAD_MULTIPLE *FAILED*"
     CALL MPI_Abort(comm, -1_MPI_INTEGER_KIND, mpierror)
  ENDIF

  CALL MPI_Comm_size(comm, mpi_size, mpierror)
  CALL MPI_Comm_rank(comm, mpi_rank, mpierror)

  !
  ! Initialize HDF5 library and Fortran interfaces.
  !
  CALL h5open_f(status)

  !
  ! Set up File Access Property List with MPI
  ! parameters for the Subfiling VFD to use
  CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, status)
  CALL H5Pset_mpi_params_f(fapl_id, comm, info, status)

  ! Use Subfiling VFD with default settings
  CALL subfiling_write_default(fapl_id, mpi_size, mpi_rank)

  ! Use Subfiling VFD with custom settings
  CALL subfiling_write_custom(fapl_id, mpi_size, mpi_rank)

  ! Use Subfiling VFD to precreate the HDF5 file on MPI rank
  CALL subfiling_write_precreate(fapl_id, mpi_size, mpi_rank)

  CALL H5Pclose_f(fapl_id, status)
  !
  ! Close FORTRAN interfaces and HDF5 library.
  !
  CALL h5close_f(status)

  IF(mpi_rank .EQ. 0) WRITE(*,"(A)") "PHDF5 example finished with no errors"

  CALL MPI_Finalize(mpierror)

END PROGRAM main
