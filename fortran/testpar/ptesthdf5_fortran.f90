!
! Main program for parallel HDF5 Fortran tests. 

     PROGRAM PHDF5F90TEST

     USE HDF5 ! This module contains all necessary modules 
     USE THDF5
!     USE MPI
        
     IMPLICIT NONE
     INTEGER :: error        ! Error flags
     INTEGER :: error_1 = 0  ! Error flags
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
     ! Check that datasets can be divided into equal parts by the processes.
     !
     if ( (mod(DIM1, mpi_size) .ne. 0) .or. (mod(DIM2, mpi_size) .ne. 0)) then
         if (mpi_rank .eq. 0) then
             write(*,*) "Number of processors is", mpi_size
             write(*,*)  "It must be a factor of ", DIM1, " and ", DIM2
             write(*,*) "Exiting..."
         endif
         goto 1000
     endif
     !
     ! Initialize FORTRAN predefined datatypes
     !
     CALL h5open_f(error) 
     if (mpi_rank .eq. 0) then
         write(*,*) '==========================================='
         write(*,*) '         Parallel Fortran Tests            ' 
         write(*,*) '==========================================='
         write(*,*) 
     endif
     if (mpi_rank .eq. 0) then 
         write(*,*) 'Writing/reading dataset by hyperslabs'
     endif
     CALL dataset_wr_by_hyperslabs(error_1)
     if (error_1 .ne. 0 ) write(*,*) 'Process ', mpi_rank, 'reports failure'
     if (mpi_rank .eq. 0) then
         write(*,*) 
         write(*,*) '==========================================='
         write(*,*) '      Parallel Fortran Tests finished      ' 
         write(*,*) '==========================================='
     endif
     !
     ! Close FORTRAN predefined datatypes.
     !
     CALL h5close_f(error)

1000 continue

     CALL MPI_FINALIZE(mpierror)

     END PROGRAM PHDF5F90TEST
