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
! Main program for parallel HDF5 Fortran tests. 

     PROGRAM PHDF5F90TEST

     USE HDF5 ! This module contains all necessary modules 
     USE THDF5
!     USE MPI
        
     IMPLICIT NONE
     INTEGER :: h5retcode    ! HDF5 call return code
     INTEGER :: nerrors = 0  ! Error flags
     !
     ! MPI definitions and calls.
     !
     INTEGER :: mpiretcode       ! MPI calls return code
     INTEGER :: comm, info
     INTEGER :: mpi_size, mpi_rank
     LOGICAL :: cleanup = .TRUE.
!     LOGICAL :: cleanup = .FALSE.
     comm = MPI_COMM_WORLD
     info = MPI_INFO_NULL
     CALL MPI_INIT(mpiretcode)
     CALL MPI_COMM_SIZE(comm, mpi_size, mpiretcode)
     CALL MPI_COMM_RANK(comm, mpi_rank, mpiretcode) 
     !
     ! Check that datasets can be divided into equal parts by the processes.
     !
     if ( (mod(DIM1, mpi_size) .ne. 0) .or. (mod(DIM2, mpi_size) .ne. 0)) then
         if (mpi_rank .eq. 0) then
             write(*,*) "Number of processors is", mpi_size
             write(*,*)  "It must be a factor of ", DIM1, " and ", DIM2
             write(*,*) "Exiting..."
         endif
	 nerrors = nerrors + 1
         goto 1000
     endif
     !
     ! Initialize FORTRAN predefined datatypes
     !
     CALL h5open_f(h5retcode) 
     if (mpi_rank .eq. 0) then
         write(*,*) '==========================================='
         write(*,*) '         Parallel Fortran Tests            ' 
         write(*,*) '==========================================='
         write(*,*) 
     endif
     if (mpi_rank .eq. 0) then 
         write(*,*) 'Writing/reading dataset by hyperslabs'
     endif
     CALL dataset_wr_by_hyperslabs(cleanup, nerrors)
     if (nerrors .ne. 0 ) write(*,*) 'Process ', mpi_rank, 'reports failure'
     if (mpi_rank .eq. 0) then
         write(*,*) 
         write(*,*) '==========================================='
         write(*,*) '      Parallel Fortran Tests finished      ' 
         write(*,*) '==========================================='
     endif
     !
     ! Close FORTRAN predefined datatypes.
     !
     CALL h5close_f(h5retcode)

1000 continue

     if (nerrors .eq. 0) then
	 CALL MPI_FINALIZE(mpiretcode)
     else
         CALL MPI_ABORT(MPI_COMM_WORLD, 1, mpiretcode)
     endif

     END PROGRAM PHDF5F90TEST
