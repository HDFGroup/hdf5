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
! Benchmarking writes/reads dataset by hyperslabs using/(not using)  multi-dataset routines.
!
MODULE timer
  USE MPI
  IMPLICIT NONE

  ! Statistics structure: contains stats that aggregate timer
  !    values from all ranks

  TYPE timer_statinfo
     REAL*8 min, max, mean, std
  END TYPE timer_statinfo


CONTAINS
  
  REAL*8 FUNCTION timer_tick(barrier, comm) RESULT(timer)
    IMPLICIT NONE
    LOGICAL, OPTIONAL :: barrier
    INTEGER, OPTIONAL :: comm
    INTEGER :: error

    IF(PRESENT(barrier))THEN
       IF(PRESENT(comm))THEN
          IF(barrier) CALL MPI_Barrier(comm, error)
       ELSE
          IF(barrier) CALL MPI_Barrier(MPI_COMM_WORLD, error)
       ENDIF
    ENDIF
    timer = MPI_Wtime()

  END FUNCTION timer_tick

  REAL*8 FUNCTION timer_tock(t1) RESULT(timer)
    IMPLICIT NONE
    
    REAL*8 t1, t2
    t2 = MPI_Wtime()

    timer = t2 - t1

  END FUNCTION timer_tock

  TYPE(timer_statinfo) FUNCTION timer_collectstats(timer, destrank, comm) RESULT(stats)

    IMPLICIT NONE
    REAL*8 :: timer

    INTEGER, OPTIONAL :: destrank
    INTEGER, OPTIONAL :: comm


    INTEGER ::  rank, nprocs, i
    REAL*8, DIMENSION(:), ALLOCATABLE :: rtimers ! All timers from ranks
    INTEGER :: comm_default, destrank_default
    INTEGER :: error

    comm_default = MPI_COMM_WORLD
    IF(PRESENT(comm)) comm_default = comm
    destrank_default = 0
    IF(PRESENT(destrank)) destrank_default = destrank
    
    CALL MPI_Comm_rank(comm_default, rank, error)
    CALL MPI_Comm_size(comm_default, nprocs, error)
    IF(rank .EQ. destrank_default)THEN
       ALLOCATE(rtimers(1:nprocs))
       stats%mean = 0.
       stats%min = timer
       stats%max = timer
       stats%std = 0.
    ENDIF

    CALL MPI_Gather(timer, 1, MPI_DOUBLE_PRECISION, rtimers, 1, MPI_DOUBLE_PRECISION, &
         destrank_default, comm_default, error)

    IF(rank == destrank_default)THEN
        DO i = 1, nprocs
           IF(rtimers(i) > stats%max)  stats%max = rtimers(i)
           IF(rtimers(i) < stats%min)  stats%min = rtimers(i)
           stats%mean = stats%mean + rtimers(i)
        ENDDO
        stats%mean = stats%mean/nprocs
        DO i = 1, nprocs
           stats%std = stats%std + (rtimers(i)-stats%mean)*(rtimers(i)-stats%mean)
        ENDDO
        stats%std = SQRT(stats%std / nprocs)
        DEALLOCATE(rtimers)
     ENDIF
   END FUNCTION timer_collectstats

   !Collect statistics of timers on all ranks to one rank and PRINT from that rank
   !    timer: elapsed time for rank
   !    comm: communicator for collecting stats
   !    destrank: the rank to which to collect stats
   !    prefix: string to print in front of stats, use "" for no string

   SUBROUTINE timer_collectprintstats(timer, prefix, destrank, comm)

     IMPLICIT NONE
     
     REAL*8 :: timer
     CHARACTER(*), OPTIONAL :: prefix
     INTEGER, OPTIONAL :: comm
     INTEGER, OPTIONAL :: destrank
     CHARACTER(LEN=180) :: prefix_default


     TYPE(timer_statinfo) :: stats
     INTEGER ::  rank, error
     INTEGER :: comm_default, destrank_default

     comm_default = MPI_COMM_WORLD
     IF(PRESENT(comm)) comm_default = comm
     destrank_default = 0
     IF(PRESENT(destrank)) destrank_default = destrank
     prefix_default =""
     IF(PRESENT(prefix)) prefix_default = prefix 
    
     CALL MPI_Comm_rank(comm_default, rank, error)

     stats = timer_collectstats(timer, destrank_default, comm_default)

     IF(rank.EQ.destrank_default) CALL timer_printstats(prefix_default, stats)

   END SUBROUTINE timer_collectprintstats

   ! Print time statistics
   !    prefix: string to print in front of stats
   !    stats: timerstats

   SUBROUTINE timer_printstats(prefix, stats)

     IMPLICIT NONE
     
     CHARACTER(*) :: prefix
     TYPE(timer_statinfo) :: stats

     WRITE(*,'(A,4(A,1X,F6.3))') TRIM(prefix)," timer seconds mean =",stats%mean, &
          ", min =", stats%min, ", max =", stats%max, ", std =", stats%std

   END SUBROUTINE timer_printstats



END MODULE timer


SUBROUTINE pmultiple_dset_hyper_rw(do_collective, do_chunk, mpi_size, mpi_rank, ndsets, multi)

  USE iso_c_binding
  USE hdf5
  USE mpi
  USE timer
  IMPLICIT NONE

  LOGICAL, INTENT(in) :: do_collective              ! use collective IO
  LOGICAL, INTENT(in) :: do_chunk                   ! use chunking
  INTEGER, INTENT(in) :: mpi_size                   ! number of processes in the group of communicator
  INTEGER, INTENT(in) :: mpi_rank                   ! rank of the calling process in the communicator
  CHARACTER(LEN=80):: dsetname ! Dataset name
  TYPE(H5D_rw_multi_t), ALLOCATABLE, DIMENSION(:) :: info_md
  INTEGER(hsize_t), DIMENSION(1:2) :: cdims           ! chunk dimensions

  INTEGER :: nerrors
  INTEGER(SIZE_T):: ndsets
  INTEGER(HID_T) :: file_id       ! File identifier
  INTEGER(HID_T) :: filespace     ! Dataspace identifier in file 
  INTEGER(HID_T) :: memspace      ! Dataspace identifier in memory
  INTEGER(HID_T) :: plist_id      ! Property list identifier 
  INTEGER(HID_T) :: dcpl_id       ! Dataset creation property list
  INTEGER(HID_T) :: dset_id       ! Dataset creation property list
  INTEGER(HSIZE_T), DIMENSION(1:2) :: dimsf  ! Dataset dimensions.
  INTEGER(HSIZE_T), DIMENSION(1:2) :: dimsfi = (/5,8/)

  INTEGER(HSIZE_T), DIMENSION(1:2) :: count  
  INTEGER(HSSIZE_T), DIMENSION(1:2) :: offset 
  INTEGER, ALLOCATABLE, DIMENSION(:,:,:), TARGET :: DATA  ! Data to write
  INTEGER, ALLOCATABLE, DIMENSION(:,:,:), TARGET :: rDATA  ! Data to write
  INTEGER, PARAMETER :: rank = 2 ! Dataset rank 
  INTEGER :: i, j, k, istart
  INTEGER :: error          ! Error flags
  logical :: multi
  REAL*8 t_write,t_read
  CHARACTER(LEN=5) :: ichr5

  dimsf = (/25000_hsize_t,INT(mpi_size*8, hsize_t)/)

  IF(mpi_rank.EQ.0)THEN

     WRITE(*,'(/,A,I0)') "TOTAL DATASETS SIZE (MB): ", dimsf(1)*dimsf(2)*ndsets*storage_size(INT(1))/8/1048576
     WRITE(*,*) "Multi-dataset API: ", multi

     CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
     CALL H5Pset_libver_bounds_f(plist_id, H5F_LIBVER_LATEST_F, H5F_LIBVER_LATEST_F, error)
     !
     ! Create the file.
     ! 
     CALL h5fcreate_f("md.h5", H5F_ACC_TRUNC_F, file_id, error, access_prp = plist_id)
     CALL h5pclose_f(plist_id, error)

     CALL h5pcreate_f(H5P_DATASET_CREATE_F, dcpl_id, error)
     CALL H5Pset_alloc_time_f(dcpl_id, H5D_ALLOC_TIME_EARLY_F, error)
     CALL h5screate_simple_f(rank, dimsf, filespace, error)
     DO i = 1, ndsets
        WRITE(dsetname,'("dataset ",I0)') i
        ! create the dataset
        CALL h5dcreate_f(file_id, TRIM(dsetname), H5T_NATIVE_INTEGER, filespace, dset_id, error, dcpl_id)
        CALL h5dclose_f(dset_id, error)
     ENDDO
     
     CALL h5pclose_f(dcpl_id, error)
     CALL h5sclose_f(filespace, error)
     CALL h5fclose_f(file_id, error)

  ENDIF
  CALL MPI_BARRIER(MPI_COMM_WORLD, error)

  CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
  CALL H5Pset_libver_bounds_f(plist_id, H5F_LIBVER_LATEST_F, H5F_LIBVER_LATEST_F, error)
  CALL H5Pset_coll_metadata_write_f(plist_id, .TRUE., error)
  CALL H5Pset_all_coll_metadata_ops_f(plist_id, .TRUE., error)

  CALL h5pset_fapl_mpio_f(plist_id, MPI_COMM_WORLD, MPI_INFO_NULL, error)

  CALL H5Fopen_f("md.h5", H5F_ACC_RDWR_F, file_id, error, access_prp = plist_id)
  CALL h5pclose_f(plist_id, error)

  !
  ! Create the data space for the  dataset. 
  !
  CALL h5screate_simple_f(rank, dimsf, filespace, error)
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
  ! Modify dataset creation properties to enable chunking
  !
  CALL h5pcreate_f(H5P_DATASET_CREATE_F, dcpl_id, error)
  
!!$  IF (do_chunk) THEN
!!$     cdims(1) = dimsf(1)
!!$     cdims(2) = dimsf(2)/mpi_size/2
!!$     CALL h5pset_chunk_f(dcpl_id, 2, cdims, error)
!!$  ENDIF
  ! 
  ! Select hyperslab in the file.
  !
  CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, offset, count, error)
  !
  ! Initialize data buffer
  !
  ALLOCATE ( DATA(COUNT(1),COUNT(2), ndsets))
  ALLOCATE ( rdata(COUNT(1),COUNT(2), ndsets))

  ALLOCATE(info_md(1:ndsets))
  !
  ! Create property list for collective dataset write
  !
  CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
  IF(do_collective)THEN
     CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, error)
  ELSE
     CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_INDEPENDENT_F, error)
  ENDIF
     
  !
  ! Create the dataset with default properties.
  !
  info_md(1:ndsets)%mem_type_id = H5T_NATIVE_INTEGER
  info_md(1:ndsets)%mem_space_id = memspace
  info_md(1:ndsets)%dset_space_id = filespace

  DO i = 1, ndsets
     ! Create the data
     DO k = 1, COUNT(1)
        DO j = 1, COUNT(2)
           istart = (k-1)*dimsf(2) + mpi_rank*COUNT(2)
           DATA(k,j,i) = (istart + j)*10**(i-1)
        ENDDO
     ENDDO
     ! Point to the data
     info_md(i)%buf = C_LOC(DATA(1,1,i))

     ! direct the output of the write statement to unit "dsetname"
     WRITE(dsetname,'("dataset ",I0)') i
     ! open the dataset
     CALL h5dopen_f(file_id, dsetname, info_md(i)%dset_id, error)
     
  ENDDO

  IF(multi)THEN
  !
  ! Write the dataset collectively. 
  !
     t_write = timer_tick(.TRUE.)
     CALL h5dwrite_multi_f(plist_id, ndsets, info_md, error)
     t_write = timer_tock(t_write)

     t_read = timer_tick(.TRUE.)
     DO i = 1, ndsets
        ! Point to the read buffer
        info_md(i)%buf = C_LOC(rdata(1,1,i))
     ENDDO
     CALL H5Dread_multi_f(plist_id, ndsets, info_md, error)
     t_read = timer_tock(t_read)

  ELSE
    
     t_write = timer_tick(.TRUE.)
     DO i = 1, ndsets
        CALL h5dwrite_f(info_md(i)%dset_id, info_md(i)%mem_type_id, info_md(i)%buf, error, &
             file_space_id=info_md(i)%dset_space_id,mem_space_id=info_md(i)%mem_space_id, xfer_prp=plist_id)
     ENDDO
     t_write = timer_tock(t_write)

     t_read = timer_tick(.TRUE.)
     DO  i = 1, ndsets
        ! Point to the read buffer
        info_md(i)%buf = C_LOC(rdata(1,1,i))
        CALL h5dread_f(info_md(i)%dset_id,info_md(i)%mem_type_id, info_md(i)%buf, error, &
             file_space_id=info_md(i)%dset_space_id,mem_space_id=info_md(i)%mem_space_id, xfer_prp=plist_id)   
     ENDDO
     t_read = timer_tock(t_read)

  ENDIF

  WRITE(ichr5,'(I5.5)') ndsets

  CALL timer_collectprintstats(t_write,"write."//ichr5)
  CALL timer_collectprintstats(t_read,"read."//ichr5)

  DO i = 1, ndsets
     ! Close all the datasets
     CALL h5dclose_f(info_md(i)%dset_id, error)
  ENDDO

  ! check the data read and write buffers
  nerrors=0
  DO i = 1, ndsets
     ! Create the data
     DO k = 1, COUNT(1)
        DO j = 1, COUNT(2)
           IF(rDATA(k,j,i).NE.DATA(k,j,i))THEN
              nerrors = nerrors + 1
           ENDIF
        ENDDO
     ENDDO
  ENDDO

  IF(nerrors.NE.0)THEN
     CALL MPI_Abort(MPI_COMM_WORLD, -1, error) 
     PRINT*,'ERROR: READ .NE. WRITE DATA'
  ENDIF

  !
  ! Deallocate data buffer.
  !
  DEALLOCATE(data, rdata)

  !
  ! Close dataspaces.
  !
  CALL h5sclose_f(filespace, error)
  CALL h5sclose_f(memspace, error)
  !
  ! Close the dataset and property list.
  !
  CALL h5pclose_f(plist_id, error)

  !
  ! Close the file.
  !
  CALL h5fclose_f(file_id, error)

END SUBROUTINE pmultiple_dset_hyper_rw

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
! MAIN PROGRAM
!

PROGRAM parallel_test_F03
  USE hdf5
  USE mpi
  IMPLICIT NONE

  INTEGER :: mpierror       ! MPI hdferror flag
  INTEGER :: hdferror       ! HDF hdferror flag
  INTEGER :: nerrors        ! number of errors
  INTEGER :: mpi_size       ! number of processes in the group of communicator
  INTEGER :: mpi_rank       ! rank of the calling process in the communicator
  INTEGER :: i,j
  ! use collective MPI I/O
  LOGICAL, DIMENSION(1:2) :: do_collective = (/.FALSE.,.TRUE./)
  !CHARACTER(LEN=11), DIMENSION(1:2) :: chr_collective =(/"independent", "collective "/)
  ! use chunking
  LOGICAL, DIMENSION(1:2) :: do_chunk = (/.FALSE.,.TRUE./)
  !CHARACTER(LEN=10), DIMENSION(1:2) :: chr_chunk =(/"contiguous", "chunk     "/)
  LOGICAL multi
  INTEGER(SIZE_T):: ndsets,max_ndsets
  CHARACTER(len=32) :: arg

  !
  ! initialize MPI
  !
  CALL mpi_init(mpierror)
  IF (mpierror .NE. MPI_SUCCESS) WRITE(*,*) "MPI_INIT  *FAILED*"
  CALL mpi_comm_rank( MPI_COMM_WORLD, mpi_rank, mpierror )
  IF (mpierror .NE. MPI_SUCCESS) WRITE(*,*) "MPI_COMM_RANK  *FAILED* Process = ", mpi_rank
  CALL mpi_comm_size( MPI_COMM_WORLD, mpi_size, mpierror )
  IF (mpierror .NE. MPI_SUCCESS) WRITE(*,*) "MPI_COMM_SIZE  *FAILED* Process = ", mpi_rank

  max_ndsets = 9
  multi = .FALSE.
  j = 1
  DO i = 1, command_argument_count()
     CALL get_command_argument(j, arg)
     IF (LEN_TRIM(arg) == 0) EXIT
     
     SELECT CASE (arg)
     CASE ('-d', '--d')
        CALL get_command_argument(j+1, arg, STATUS=hdferror)
        IF(hdferror.NE.0)THEN
           PRINT '(A,/)', 'Expected number of datasets '
           CALL MPI_Abort(MPI_COMM_WORLD, -1, hdferror)
        ENDIF
           
        READ(arg,'(I10)') max_ndsets
        j = j + 1
     CASE ('-m', '--m')
        multi=.TRUE.
     CASE default
        PRINT '(a,a,/)', 'Unrecognized command-line option: ', arg
        CALL MPI_Abort(MPI_COMM_WORLD, -1, hdferror)
     END SELECT
     j = j + 1
  END DO

  IF(mpi_rank.EQ.0)THEN
     WRITE(*,'(A,I0)') 'Max. Number of Datasets, 2^max: ', max_ndsets
     WRITE(*,'(A,L1)') 'Use Multi-Dataset API: ', multi
  ENDIF
  
  !
  ! initialize the HDF5 fortran interface
  !
  CALL h5open_f(hdferror)
  !
  ! test write/read multiple hyperslab datasets
  !
  DO i = 1, max_ndsets
     ndsets = 2**(i-1)
     multi=.TRUE.
     CALL pmultiple_dset_hyper_rw(do_collective(2), do_chunk(1), mpi_size, mpi_rank, ndsets, multi)
     multi=.FALSE.
     CALL pmultiple_dset_hyper_rw(do_collective(2), do_chunk(1), mpi_size, mpi_rank, ndsets, multi)
  ENDDO

  !
  ! close HDF5 interface
  !
  CALL h5close_f(hdferror)

  CALL mpi_finalize(hdferror)

END PROGRAM parallel_test_F03
