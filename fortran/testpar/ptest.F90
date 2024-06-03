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
! main program for parallel HDF5 Fortran tests
!

PROGRAM parallel_test
  USE HDF5
  USE MPI
  USE TH5_MISC

  IMPLICIT NONE

  INTEGER(KIND=MPI_INTEGER_KIND) :: mpierror      ! MPI hdferror flag
  INTEGER :: hdferror                             ! HDF hdferror flag
  INTEGER :: ret_total_error = 0                  ! number of errors in subroutine
  INTEGER :: total_error = 0                      ! sum of the number of errors
  INTEGER(KIND=MPI_INTEGER_KIND) :: mpi_size      ! number of processes in the group of communicator
  INTEGER(KIND=MPI_INTEGER_KIND) :: mpi_rank      ! rank of the calling process in the communicator
  INTEGER :: length = 12000                       ! length of array
  INTEGER :: i,j, sum
  ! use collective MPI I/O
  LOGICAL, DIMENSION(1:2) :: do_collective = (/.FALSE.,.TRUE./)
  CHARACTER(LEN=11), DIMENSION(1:2) :: chr_collective =(/"independent", "collective "/)
  ! use chunking
  LOGICAL, DIMENSION(1:2) :: do_chunk = (/.FALSE.,.TRUE./)
  CHARACTER(LEN=10), DIMENSION(1:2) :: chr_chunk =(/"contiguous", "chunk     "/)
  INTEGER(KIND=MPI_INTEGER_KIND) :: mpi_int_type

  INTERFACE

    SUBROUTINE mpi_param_03(ret_total_error)
      IMPLICIT NONE
      INTEGER, INTENT(inout) :: ret_total_error
    END SUBROUTINE mpi_param_03

    SUBROUTINE mpi_param_08(ret_total_error)
      IMPLICIT NONE
      INTEGER, INTENT(inout) :: ret_total_error
    END SUBROUTINE mpi_param_08

    SUBROUTINE hyper(length,do_collective,do_chunk, mpi_size, mpi_rank, nerrors)
       USE MPI
       IMPLICIT NONE
       INTEGER, INTENT(in) :: length
       LOGICAL, INTENT(in) :: do_collective
       LOGICAL, INTENT(in) :: do_chunk
       INTEGER(KIND=MPI_INTEGER_KIND), INTENT(in) :: mpi_size
       INTEGER(KIND=MPI_INTEGER_KIND), INTENT(in) :: mpi_rank
       INTEGER, INTENT(inout) :: nerrors
     END SUBROUTINE hyper

     SUBROUTINE pmultiple_dset_hyper_rw(do_collective, do_chunk, mpi_size, mpi_rank, nerrors)
       USE MPI
       IMPLICIT NONE
       LOGICAL, INTENT(in) :: do_collective
       LOGICAL, INTENT(in) :: do_chunk
       INTEGER(KIND=MPI_INTEGER_KIND), INTENT(in) :: mpi_size
       INTEGER(KIND=MPI_INTEGER_KIND), INTENT(in) :: mpi_rank
       INTEGER, INTENT(inout) :: nerrors
     END SUBROUTINE pmultiple_dset_hyper_rw

     SUBROUTINE multiple_dset_write(length, do_collective, do_chunk, mpi_size, mpi_rank, nerrors)
       USE MPI
       IMPLICIT NONE
       INTEGER, INTENT(in) :: length
       LOGICAL, INTENT(in) :: do_collective
       LOGICAL, INTENT(in) :: do_chunk
       INTEGER(KIND=MPI_INTEGER_KIND), INTENT(in) :: mpi_size
       INTEGER(KIND=MPI_INTEGER_KIND), INTENT(in) :: mpi_rank
       INTEGER, INTENT(inout) :: nerrors
     END SUBROUTINE multiple_dset_write

  END INTERFACE

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

  IF(mpi_rank==0) CALL write_test_header("COMPREHENSIVE PARALLEL FORTRAN TESTS")

  ret_total_error = 0
  CALL mpi_param_03(ret_total_error)
  IF(mpi_rank==0) CALL write_test_status(ret_total_error, &
       'Testing MPI communicator and info (F03)', total_error)

  ret_total_error = 0
  CALL mpi_param_08(ret_total_error)
  IF(mpi_rank==0) CALL write_test_status(ret_total_error, &
       'Testing MPI communicator and info (F08)', total_error)

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
  ! test write/read multiple hyperslab datasets
  !
  DO i = 1, 2
     DO j = 1, 2
        ret_total_error = 0
        CALL pmultiple_dset_hyper_rw(do_collective(j), do_chunk(i), mpi_size, mpi_rank, ret_total_error)
        IF(mpi_rank==0) CALL write_test_status(ret_total_error, &
             "Writing/reading multiple datasets by hyperslab ("//TRIM(chr_chunk(i))//" layout, "&
             //TRIM(chr_collective(j))//" MPI I/O)", total_error)
     ENDDO
  ENDDO
  !
  ! close HDF5 interface
  !
  CALL h5close_f(hdferror)

  IF(h5_sizeof(total_error).EQ.8_size_t)THEN
     mpi_int_type=MPI_INTEGER8
  ELSE
     mpi_int_type=MPI_INTEGER4
  ENDIF

  CALL MPI_ALLREDUCE(total_error, sum, 1_MPI_INTEGER_KIND, mpi_int_type, MPI_SUM, MPI_COMM_WORLD, mpierror)

  IF(mpi_rank==0) CALL write_test_footer()

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
     CALL mpi_abort(MPI_COMM_WORLD, 1_MPI_INTEGER_KIND, mpierror)
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_ABORT  *FAILED* Process = ", mpi_rank
     ENDIF
  ENDIF
  !
  ! end main program
  !
END PROGRAM parallel_test
