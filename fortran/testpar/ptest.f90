! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   Copyright by the Board of Trustees of the University of Illinois.         *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the COPYING file, which can be found at the root of the source code       *
!   distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
!   If you do not have access to either file, you may request a copy from     *
!   help@hdfgroup.org.                                                        *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

!
! main program for parallel HDF5 Fortran tests
!

PROGRAM parallel_test
  USE HDF5
  USE MPI
  USE TH5_MISC 
  
  IMPLICIT NONE

  INTEGER :: mpierror                             ! MPI hdferror flag
  INTEGER :: hdferror                             ! HDF hdferror flag
  INTEGER :: ret_total_error = 0                  ! number of errors in subroutine
  INTEGER :: total_error = 0                      ! sum of the number of errors
  INTEGER :: mpi_size                             ! number of processes in the group of communicator
  INTEGER :: mpi_rank                             ! rank of the calling process in the communicator
  INTEGER :: length = 12000                       ! length of array
  INTEGER :: i,j
  ! use collective MPI I/O
  LOGICAL, DIMENSION(1:2) :: do_collective = (/.FALSE.,.TRUE./)
  CHARACTER(LEN=11), DIMENSION(1:2) :: chr_collective =(/"independent", "collective "/)
  ! use chunking
  LOGICAL, DIMENSION(1:2) :: do_chunk = (/.FALSE.,.TRUE./)
  CHARACTER(LEN=10), DIMENSION(1:2) :: chr_chunk =(/"contiguous", "chunk     "/)

  !
  ! initialize MPI
  !
  CALL mpi_init(mpierror)
  IF (mpierror .NE. MPI_SUCCESS) THEN
     WRITE(*,*) "MPI_INIT  *FAILED*"
  ENDIF
  CALL mpi_comm_rank( MPI_COMM_WORLD, mpi_rank, mpierror )
  IF (mpierror .NE. MPI_SUCCESS) THEN
     WRITE(*,*) "MPI_COMM_RANK  *FAILED* Process = ", mpi_rank
  ENDIF
  CALL mpi_comm_size( MPI_COMM_WORLD, mpi_size, mpierror )
  IF (mpierror .NE. MPI_SUCCESS) THEN
     WRITE(*,*) "MPI_COMM_SIZE  *FAILED* Process = ", mpi_rank
  ENDIF
  !
  ! initialize the HDF5 fortran interface
  !
  CALL h5open_f(hdferror)
  !
  ! test write/read dataset by hyperslabs (contiguous/chunk) with independent/collective MPI I/O
  !
  DO i = 1, 2
     DO j = 1, 2
        ret_total_error = 0
        CALL hyper(length, do_collective(j), do_chunk(i), mpi_size, mpi_rank, ret_total_error)
        IF(mpi_rank==0) CALL write_test_status(ret_total_error, &
             "Writing/reading dataset by hyperslabs ("//TRIM(chr_chunk(i))//" layout, "//TRIM(chr_collective(j))//" MPI I/O)", &
             total_error)
     ENDDO
  ENDDO

  !
  ! test write/read several datasets (independent MPI I/O)
  !
  ret_total_error = 0
  CALL multiple_dset_write(length, do_collective(1), do_chunk(1), mpi_size, mpi_rank, ret_total_error)
  IF(mpi_rank==0) CALL write_test_status(ret_total_error, &
       'Writing/reading several datasets (contiguous layout, independent MPI I/O)', total_error)

  !
  ! close HDF5 interface
  !
  CALL h5close_f(hdferror)

  !
  ! close MPI
  !
  IF (total_error == 0) THEN
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
  !
  ! end main program
  !
END PROGRAM parallel_test
