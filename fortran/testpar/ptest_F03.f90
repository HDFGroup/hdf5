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
! MAIN PROGRAM FOR PARALLEL HDF5 FORTRAN 2003 TESTS
!

PROGRAM parallel_test_F03
  USE hdf5
  USE mpi
  IMPLICIT NONE

  INTEGER :: mpierror       ! MPI hdferror flag
  INTEGER :: hdferror       ! HDF hdferror flag
  LOGICAL :: do_collective  ! use collective MPI IO
  LOGICAL :: do_chunk       ! use chunking
  INTEGER :: nerrors = 0    ! number of errors
  INTEGER :: mpi_size       ! number of processes in the group of communicator
  INTEGER :: mpi_rank       ! rank of the calling process in the communicator
  !
  ! initialize MPI
  !
  CALL mpi_init(mpierror)
  IF (mpierror .NE. MPI_SUCCESS) WRITE(*,*) "MPI_INIT  *FAILED*"
  CALL mpi_comm_rank( MPI_COMM_WORLD, mpi_rank, mpierror )
  IF (mpierror .NE. MPI_SUCCESS) WRITE(*,*) "MPI_COMM_RANK  *FAILED* Process = ", mpi_rank
  CALL mpi_comm_size( MPI_COMM_WORLD, mpi_size, mpierror )
  IF (mpierror .NE. MPI_SUCCESS) WRITE(*,*) "MPI_COMM_SIZE  *FAILED* Process = ", mpi_rank

  !
  ! initialize the HDF5 fortran interface
  !
  CALL h5open_f(hdferror)
  !
  ! test write/read several hyperslab datasets
  !
  do_collective = .TRUE.
  do_chunk      = .FALSE.
  IF (mpi_rank == 0) WRITE(*,*) 'Writing/Reading several hyperslab datasets (contiguous layout, collective MPI IO)'
  CALL pmultiple_dset_hyper_rw(do_collective, do_chunk, mpi_size, mpi_rank, nerrors)

  do_collective = .FALSE.
  do_chunk      = .FALSE.
  IF (mpi_rank == 0) WRITE(*,*) 'Writing/Reading several hyperslab datasets (contiguous layout, independent MPI IO)'
  CALL pmultiple_dset_hyper_rw(do_collective, do_chunk, mpi_size, mpi_rank, nerrors)

  do_collective = .TRUE.
  do_chunk      = .TRUE.
  IF (mpi_rank == 0) WRITE(*,*) 'Writing/Reading several hyperslab datasets (chunked, collective MPI IO)'
  CALL pmultiple_dset_hyper_rw(do_collective, do_chunk, mpi_size, mpi_rank, nerrors)

  do_collective = .FALSE.
  do_chunk      = .TRUE.
  IF (mpi_rank == 0) WRITE(*,*) 'Writing/Reading several hyperslab datasets (chunked, independent MPI IO)'
  CALL pmultiple_dset_hyper_rw(do_collective, do_chunk, mpi_size, mpi_rank, nerrors)
  
  !
  ! close HDF5 interface
  !
  CALL h5close_f(hdferror)
  !
  ! close MPI
  !
  IF (nerrors == 0) THEN
     CALL mpi_finalize(mpierror)
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_FINALIZE  *FAILED* Process = ", mpi_rank
     ENDIF
  ELSE
     WRITE(*,*) 'Errors detected in process ', mpi_rank
     CALL mpi_abort(MPI_COMM_WORLD, 1, mpierror)
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_ABORT  *FAILED* Process = ", mpi_rank
     ENDIF
  ENDIF

END PROGRAM parallel_test_F03

