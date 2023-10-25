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
! writes/reads dataset by hyperslabs
!

SUBROUTINE hyper(length,do_collective,do_chunk, mpi_size, mpi_rank, nerrors)
  USE HDF5
  USE MPI
  USE TH5_MISC

  IMPLICIT NONE

  INTEGER, INTENT(in) :: length                     ! array length
  LOGICAL, INTENT(in) :: do_collective              ! use collective I/O
  LOGICAL, INTENT(in) :: do_chunk                   ! use chunking
  INTEGER, INTENT(in) :: mpi_size                   ! number of processes in the group of communicator
  INTEGER, INTENT(in) :: mpi_rank                   ! rank of the calling process in the communicator
  INTEGER, INTENT(inout) :: nerrors                 ! number of errors
  INTEGER :: hdferror                               ! HDF hdferror flag
  INTEGER(hsize_t), DIMENSION(1) :: dims            ! dataset dimensions
  INTEGER(hsize_t), DIMENSION(1) :: cdims           ! chunk dimensions
  INTEGER, ALLOCATABLE :: wbuf(:)                   ! write buffer
  INTEGER, ALLOCATABLE :: rbuf(:)                   ! read buffer
  INTEGER(hsize_t), DIMENSION(1) :: counti          ! hyperslab selection
  INTEGER(hsize_t), DIMENSION(1) :: start           ! hyperslab selection
  INTEGER(hid_t) :: fapl_id                         ! file access identifier
  INTEGER(hid_t) :: dxpl_id                         ! dataset transfer property list
  INTEGER(hid_t) :: dcpl_id                         ! dataset creation property list
  INTEGER(hid_t) :: file_id                         ! file identifier
  INTEGER(hid_t) :: dset_id                         ! dataset identifier
  INTEGER(hid_t) :: fspace_id                       ! file space identifier
  INTEGER(hid_t) :: mspace_id                       ! memory space identifier
  INTEGER(hid_t) :: driver_id                       ! low-level file driver identifier
  INTEGER        :: istart                          ! start position in array
  INTEGER        :: iend                            ! end position in array
  INTEGER        :: icount                          ! number of elements in array
  CHARACTER(len=80) :: filename                     ! filename
  INTEGER        :: i
  INTEGER        :: actual_io_mode                  ! The type of I/O performed by this process
  LOGICAL        :: is_coll
  LOGICAL        :: is_coll_true = .TRUE.

  INTEGER :: local_no_collective_cause
  INTEGER :: global_no_collective_cause
  INTEGER :: no_selection_io_cause
  INTEGER :: actual_selection_io_mode

  !
  ! initialize the array data between the processes (3)
  ! for the 12 size array we get
  ! p0 = 1,2,3,4
  ! p1 = 5,6,7,8
  ! p2 = 9,10,11,12
  !

  ALLOCATE(wbuf(0:length-1),stat=hdferror)
  IF (hdferror /= 0) THEN
     WRITE(*,*) 'allocate error'
     RETURN
  ENDIF

  ALLOCATE(rbuf(0:length-1),stat=hdferror)
  IF (hdferror /= 0) THEN
     WRITE(*,*) 'allocate error'
     RETURN
  ENDIF

  icount  = length/mpi_size     ! divide the array by the number of processes
  istart  = mpi_rank*icount     ! start position
  iend    = istart + icount     ! end position

  DO i = istart, iend-1
     wbuf(i) = i
  ENDDO

  !
  ! HDF5 I/O
  !

  dims(1)  = length
  cdims(1) = length/mpi_size     ! define chunks as the number of processes

  !
  ! setup file access property list with parallel I/O access
  !
  CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)

  CALL h5pset_fapl_mpio_f(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL, hdferror)
  CALL check("h5pset_fapl_mpio_f", hdferror, nerrors)

  CALL h5pget_driver_f(fapl_id, driver_id, hdferror)
  CALL check("h5pget_driver_f", hdferror, nerrors)

  IF( driver_id /= H5FD_MPIO_F) THEN
     WRITE(*,*) "Wrong driver information returned"
     nerrors = nerrors + 1
  ENDIF

  !
  ! create the file collectively
  !
  CALL h5_fixname_f("parf1", filename, fapl_id, hdferror)

  IF(do_collective)THEN
     ! verify settings for file access properties

     ! Collective metadata writes
     CALL h5pget_coll_metadata_write_f(fapl_id, is_coll, hdferror)
     CALL check("h5pget_coll_metadata_write_f", hdferror, nerrors)
     IF(is_coll .NEQV. .FALSE.)THEN
        PRINT*, "Incorrect property setting for coll metadata writes"
        nerrors = nerrors + 1
     ENDIF

     ! Collective metadata read API calling requirement
     CALL h5pget_all_coll_metadata_ops_f(fapl_id, is_coll, hdferror)
     CALL check("h5pget_all_coll_metadata_ops_f", hdferror, nerrors)
     IF(is_coll .NEQV. .FALSE.)THEN
        PRINT*, "Incorrect property setting for coll metadata API calls requirement"
        nerrors = nerrors + 1
     ENDIF

     ! Collective metadata writes
     CALL h5pset_coll_metadata_write_f(fapl_id, .TRUE., hdferror)
     CALL check("h5pset_coll_metadata_write_f", hdferror, nerrors)
     ! Collective metadata READ API calling requirement
     CALL h5pset_all_coll_metadata_ops_f(fapl_id, is_coll_true, hdferror)
     CALL check("h5pset_all_coll_metadata_ops_f", hdferror, nerrors)

     CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, hdferror, access_prp = fapl_id)
     CALL check("h5fcreate_f", hdferror, nerrors)

     ! close fapl and retrieve it from file
     CALL h5pclose_f(fapl_id, hdferror)
     CALL check("h5pclose_f", hdferror, nerrors)
     CALL h5fget_access_plist_f(file_id, fapl_id, hdferror)
     CALL check("h5fget_access_plist_f", hdferror, nerrors)

     ! verify settings for file access properties

     ! Collective metadata writes
     CALL h5pget_coll_metadata_write_f(fapl_id, is_coll, hdferror)
     CALL check("h5pget_coll_metadata_write_f", hdferror, nerrors)
     IF(is_coll .NEQV. .TRUE.)THEN
        PRINT*, "Incorrect property setting for coll metadata writes"
        nerrors = nerrors + 1
     ENDIF

     ! Collective metadata read API calling requirement
     CALL h5pget_all_coll_metadata_ops_f(fapl_id, is_coll, hdferror)
     CALL check("h5pget_all_coll_metadata_ops_f", hdferror, nerrors)
     IF(is_coll .NEQV. .TRUE.)THEN
        PRINT*, "Incorrect property setting for coll metadata API calls requirement"
        nerrors = nerrors + 1
     ENDIF
  ELSE
     CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, hdferror, access_prp = fapl_id)
     CALL check("h5fcreate_f", hdferror, nerrors)
  ENDIF

  CALL h5screate_simple_f(1, dims, fspace_id, hdferror)
  CALL check("h5screate_simple_f", hdferror, nerrors)

  CALL h5screate_simple_f(1, dims, mspace_id, hdferror)
  CALL check("h5screate_simple_f", hdferror, nerrors)

  !
  ! modify dataset creation properties to enable chunking
  !

  CALL h5pcreate_f(H5P_DATASET_CREATE_F, dcpl_id, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)

  IF (do_chunk) THEN
     CALL h5pset_chunk_f(dcpl_id, 1, cdims, hdferror)
     CALL check("h5pset_chunk_f", hdferror, nerrors)
  ENDIF

  !
  ! create the dataset
  !

  CALL h5dcreate_f(file_id, "dset", H5T_NATIVE_INTEGER, fspace_id, dset_id, hdferror, dcpl_id)
  CALL check("h5dcreate_f", hdferror, nerrors)

  !
  ! define hyperslab
  !

  counti(1) = icount
  start(1)  = istart

  !
  ! select hyperslab in memory
  !

  CALL h5sselect_hyperslab_f(mspace_id, H5S_SELECT_SET_F, start, counti, hdferror)
  CALL check("h5sselect_hyperslab_f", hdferror, nerrors)

  !
  ! select hyperslab in the file
  !

  CALL h5sselect_hyperslab_f(fspace_id, H5S_SELECT_SET_F, start, counti, hdferror)
  CALL check("h5sselect_hyperslab_f", hdferror, nerrors)


  !
  ! create a property list for collective dataset write
  !

  CALL h5pcreate_f(H5P_DATASET_XFER_F, dxpl_id, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)

  IF (do_collective) THEN
     CALL h5pset_dxpl_mpio_f(dxpl_id, H5FD_MPIO_COLLECTIVE_F, hdferror)
     CALL check("h5pset_dxpl_mpio_f", hdferror, nerrors)
  ENDIF

  !
  ! write dataset
  !

  CALL h5dwrite_f(dset_id,H5T_NATIVE_INTEGER,wbuf,dims,hdferror,file_space_id=fspace_id,mem_space_id=mspace_id,xfer_prp=dxpl_id)
  CALL check("h5dwrite_f", hdferror, nerrors)

  CALL h5pget_actual_selection_io_mode_f(dxpl_id, actual_selection_io_mode, hdferror)
  CALL check("h5pget_actual_selection_io_mode_f", hdferror, nerrors)
  IF(do_collective)THEN
     IF(actual_selection_io_mode .NE. H5D_SELECTION_IO_F)THEN
        PRINT*, "Incorrect actual selection io mode"
        nerrors = nerrors + 1
     ENDIF
  ELSE
     IF(actual_selection_io_mode .NE. IOR(H5D_SELECTION_IO_F, H5D_SCALAR_IO_F))THEN
        PRINT*, "Incorrect actual selection io mode"
        nerrors = nerrors + 1
     ENDIF
  ENDIF

  ! Check h5pget_mpio_actual_io_mode_f function
  CALL h5pget_mpio_actual_io_mode_f(dxpl_id, actual_io_mode, hdferror)
  CALL check("h5pget_mpio_actual_io_mode_f", hdferror, nerrors)

  CALL h5pget_mpio_no_collective_cause_f(dxpl_id, local_no_collective_cause, global_no_collective_cause, hdferror)
  CALL check("h5pget_mpio_no_collective_cause_f", hdferror, nerrors)

  IF(do_collective) THEN
     IF(local_no_collective_cause .NE. H5D_MPIO_COLLECTIVE_F) &
          CALL check("h5pget_mpio_no_collective_cause_f", -1, nerrors)
     IF(global_no_collective_cause .NE. H5D_MPIO_COLLECTIVE_F) &
          CALL check("h5pget_mpio_no_collective_cause_f", -1, nerrors)
  ELSE
     IF(local_no_collective_cause .NE. H5D_MPIO_SET_INDEPENDENT_F) &
          CALL check("h5pget_mpio_no_collective_cause_f", -1, nerrors)
     IF(global_no_collective_cause .NE. H5D_MPIO_SET_INDEPENDENT_F) &
          CALL check("h5pget_mpio_no_collective_cause_f", -1, nerrors)
  ENDIF

  IF(do_collective.AND.do_chunk)THEN
     IF(actual_io_mode.NE.H5D_MPIO_CHUNK_COLLECTIVE_F)THEN
       CALL check("h5pget_mpio_actual_io_mode_f", -1, nerrors)
     ENDIF
  ELSEIF(.NOT.do_collective)THEN
     IF(actual_io_mode.NE.H5D_MPIO_NO_COLLECTIVE_F)THEN
        CALL check("h5pget_mpio_actual_io_mode_f", -1, nerrors)
     ENDIF
  ELSEIF( do_collective.AND.(.NOT.do_chunk))THEN
     IF(actual_io_mode.NE.H5D_MPIO_CONTIG_COLLECTIVE_F)THEN
        CALL check("h5pget_mpio_actual_io_mode_f", -1, nerrors)
     ENDIF
  ENDIF

  CALL h5pset_selection_io_f(dxpl_id, H5D_SELECTION_IO_MODE_OFF_F, hdferror)
  CALL check("h5pset_selection_io_f", hdferror, nerrors)

  CALL h5dwrite_f(dset_id,H5T_NATIVE_INTEGER,wbuf,dims,hdferror,file_space_id=fspace_id,mem_space_id=mspace_id,xfer_prp=dxpl_id)
  CALL check("h5dwrite_f", hdferror, nerrors)

  ! Verify bitwise operations are correct

  IF( IOR(H5D_MPIO_DATATYPE_CONVERSION_F,H5D_MPIO_DATA_TRANSFORMS_F).NE.6)THEN
     PRINT*, "Incorrect bitwise operations for Fortran defined constants"
     nerrors = nerrors + 1
  ENDIF

  CALL h5pget_no_selection_io_cause_f(dxpl_id, no_selection_io_cause, hdferror)
  CALL check("h5pget_no_selection_io_cause_f", hdferror, nerrors)

  IF(no_selection_io_cause .NE. H5D_SEL_IO_DISABLE_BY_API_F) &
       CALL check("h5pget_no_selection_io_cause_f", -1, nerrors)

  !
  ! close HDF5 I/O
  !

  CALL h5pclose_f(fapl_id, hdferror)
  CALL check("h5pclose_f", hdferror, nerrors)

  CALL h5pclose_f(dcpl_id, hdferror)
  CALL check("h5pclose_f", hdferror, nerrors)

  CALL h5pclose_f(dxpl_id, hdferror)
  CALL check("h5pclose_f", hdferror, nerrors)

  CALL h5sclose_f(mspace_id, hdferror)
  CALL check("h5sclose_f", hdferror, nerrors)

  CALL h5sclose_f(fspace_id, hdferror)
  CALL check("h5sclose_f", hdferror, nerrors)

  CALL h5dclose_f(dset_id, hdferror)
  CALL check("h5dclose_f", hdferror, nerrors)

  CALL h5fclose_f(file_id, hdferror)
  CALL check("h5fclose_f", hdferror, nerrors)

  !
  ! reopen file with read access
  !

  CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)

  CALL h5pset_fapl_mpio_f(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL, hdferror)
  CALL check("h5pset_fapl_mpio_f", hdferror, nerrors)

  CALL h5fopen_f(filename, H5F_ACC_RDWR_F, file_id, hdferror, access_prp = fapl_id)
  CALL check("h5fopen_f", hdferror, nerrors)

  CALL h5screate_simple_f(1, dims, fspace_id, hdferror)
  CALL check("h5screate_simple_f", hdferror, nerrors)

  CALL h5screate_simple_f(1, dims, mspace_id, hdferror)
  CALL check(" h5screate_simple_f", hdferror, nerrors)

  CALL h5dopen_f(file_id, "dset", dset_id, hdferror)
  CALL check("h5dopen_f", hdferror, nerrors)

  !
  ! select hyperslab in memory
  !

  CALL h5sselect_hyperslab_f(mspace_id, H5S_SELECT_SET_F, start, counti, hdferror)
  CALL check("h5sselect_hyperslab_f", hdferror, nerrors)

  !
  ! select hyperslab in the file
  !

  CALL h5sselect_hyperslab_f(fspace_id, H5S_SELECT_SET_F, start, counti, hdferror)
  CALL check("h5sselect_hyperslab_f", hdferror, nerrors)

  !
  ! create a property list for collective dataset read
  !

  CALL h5pcreate_f(H5P_DATASET_XFER_F, dxpl_id, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)
  IF (do_collective) THEN
     CALL h5pset_dxpl_mpio_f(dxpl_id, H5FD_MPIO_COLLECTIVE_F, hdferror)
     CALL check("h5pset_dxpl_mpio_f", hdferror, nerrors)
  ENDIF

  !
  ! read dataset
  !

  CALL h5dread_f(dset_id,H5T_NATIVE_INTEGER,rbuf,dims,hdferror,file_space_id=fspace_id,mem_space_id=mspace_id,xfer_prp=dxpl_id)
  CALL check("h5dread_f", hdferror, nerrors)

  !
  ! close HDF5 I/O
  !

  CALL h5pclose_f(fapl_id, hdferror)
  CALL check("h5pclose_f", hdferror, nerrors)

  CALL h5pclose_f(dxpl_id, hdferror)
  CALL check("h5pclose_f", hdferror, nerrors)

  CALL h5sclose_f(fspace_id, hdferror)
  CALL check("h5sclose_f", hdferror, nerrors)

  CALL h5sclose_f(mspace_id, hdferror)
  CALL check("h5sclose_f", hdferror, nerrors)

  CALL h5dclose_f(dset_id, hdferror)
  CALL check("h5dclose_f", hdferror, nerrors)

  CALL h5fclose_f(file_id, hdferror)
  CALL check("h5fclose_f", hdferror, nerrors)

  !
  ! compare read and write data. each process compares a subset of the array
  !

  DO i = istart, iend-1
     IF( wbuf(i) /= rbuf(i)) THEN
        WRITE(*,*) 'buffers differs at ', i, rbuf(i), wbuf(i)
        nerrors = nerrors + 1
     ENDIF
  ENDDO

  DEALLOCATE(wbuf)
  DEALLOCATE(rbuf)

END SUBROUTINE hyper

