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

MODULE posix
  USE, INTRINSIC :: iso_c_binding, ONLY: C_INT, C_INT32_T
  IMPLICIT NONE

  INTERFACE

     ! sleep - suspend execution for second intervals
     INTEGER(C_INT) FUNCTION c_sleep(seconds) BIND(C, name='sleep')
       IMPORT :: C_INT
       INTEGER(kind=C_INT), VALUE :: seconds
     END FUNCTION c_sleep

     ! usleep - suspend execution for microsecond intervals
     INTEGER(C_INT) FUNCTION c_usleep(usec) bind(c, name='usleep')
       IMPORT :: C_INT, C_INT32_T
       INTEGER(KIND=C_INT32_T), VALUE :: usec
     END FUNCTION c_usleep

  END INTERFACE

END MODULE posix

!
! The main program for async HDF5 Fortran tests
!
PROGRAM async_test
  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT64_T
  USE HDF5
  USE MPI
  USE TH5_MISC
  USE TH5_MISC_GEN
  USE POSIX

  IMPLICIT NONE

  INTEGER :: total_error = 0        ! sum of the number of errors
  INTEGER :: mpierror               ! MPI hdferror flag
  INTEGER :: mpi_size               ! number of processes in the group of communicator
  INTEGER :: mpi_rank               ! rank of the calling process in the communicator
  INTEGER :: required, provided

  INTEGER(HID_T) :: vol_id
  INTEGER :: hdferror
  LOGICAL :: registered
  INTEGER :: sum
  INTEGER :: nerrors = 0

  INTEGER(HID_T) :: fapl_id
  INTEGER(HID_T) :: file_id
  INTEGER(HID_T) :: es_id
  CHARACTER(len=80) :: filename = "async.h5"
  INTEGER(SIZE_T) :: num_in_progress
  LOGICAL         :: err_occurred
  INTEGER(C_INT)  :: c_status
  INTEGER(SIZE_T) :: count
  INTEGER(C_INT64_T) :: counter

  ! Set pseudo debugging options
  CHARACTER(LEN=*), PARAMETER :: pseudo_file = "foo"
  CHARACTER(LEN=*), PARAMETER :: pseudo_func = "bar"
  INTEGER         , PARAMETER :: pseudo_line = 42

  INTEGER(hsize_t), DIMENSION(1) :: dims            ! dataset dimensions
  INTEGER(hsize_t), DIMENSION(1) :: cdims           ! chunk dimensions
  INTEGER, ALLOCATABLE :: wbuf(:)                   ! write buffer
  INTEGER, ALLOCATABLE :: rbuf(:)                   ! read buffer
  INTEGER(hsize_t), DIMENSION(1) :: counti          ! hyperslab selection
  INTEGER(hsize_t), DIMENSION(1) :: start           ! hyperslab selection
  INTEGER(hid_t) :: dxpl_id                         ! dataset transfer property list
  INTEGER(hid_t) :: dcpl_id                         ! dataset creation property list
  INTEGER(hid_t) :: dset_id                         ! dataset identifier
  INTEGER(hid_t) :: fspace_id                       ! file space identifier
  INTEGER(hid_t) :: mspace_id                       ! memory space identifier
  INTEGER        :: istart                          ! start position in array
  INTEGER        :: iend                            ! end position in array
  INTEGER        :: icount                          ! number of elements in array
  INTEGER        :: i
  !
  ! initialize MPI
  !
  required = MPI_THREAD_MULTIPLE
  CALL mpi_init_thread(required, provided, mpierror)
  IF (mpierror .NE. MPI_SUCCESS) THEN
     WRITE(*,*) "MPI_INIT_THREAD  *FAILED*"
     nerrors = nerrors + 1
  ENDIF
  IF (provided .NE. required) THEN
     total_error = -1 ! skip test
     IF(mpi_rank==0) CALL write_test_status(total_error, &
          "MPI doesn't support MPI_Init_thread with MPI_THREAD_MULTIPLET", total_error)
     STOP
  ENDIF
  CALL mpi_comm_rank( MPI_COMM_WORLD, mpi_rank, mpierror )
  IF (mpierror .NE. MPI_SUCCESS) THEN
     WRITE(*,*) "MPI_COMM_RANK  *FAILED* Process = ", mpi_rank
     nerrors = nerrors + 1
  ENDIF
  CALL mpi_comm_size( MPI_COMM_WORLD, mpi_size, mpierror )
  IF (mpierror .NE. MPI_SUCCESS) THEN
     WRITE(*,*) "MPI_COMM_SIZE  *FAILED* Process = ", mpi_rank
     nerrors = nerrors + 1
  ENDIF

  IF(nerrors.NE.0)THEN
     IF(mpi_rank==0) CALL write_test_status(sum, &
          'Testing Initializing mpi_init_thread', total_error)
     CALL MPI_Barrier(MPI_COMM_WORLD, mpierror)
     CALL mpi_abort(MPI_COMM_WORLD, 1, mpierror)
  ENDIF

  !
  ! Initialize the HDF5 fortran interface
  !
  CALL h5open_f(hdferror)

  ! Check if ASYNC VOL is available
  CALL H5VLis_connector_registered_by_name_f("async", registered,  hdferror)
  CALL check("H5VLis_connector_registered_by_name_f", hdferror, total_error)

  IF(.NOT.registered)THEN

     ! check if the DAOS VOL is available
     CALL H5VLis_connector_registered_by_name_f("daos", registered,  hdferror)
     CALL check("H5VLis_connector_registered_by_name_f", hdferror, total_error)

     IF(.NOT.registered)THEN
        ! No async compatible VOL found, skipping test
        total_error = -1
     ELSE
        CALL H5Vlregister_connector_by_name_f("daos", vol_id, hdferror)
        CALL check("H5Vlregister_connector_by_name_f", hdferror, total_error)
     ENDIF

  ELSE
     CALL H5Vlregister_connector_by_name_f("async", vol_id, hdferror)
     CALL check("H5Vlregister_connector_by_name_f", hdferror, total_error)
  ENDIF

  IF(total_error.LT.0)THEN
     IF(mpi_rank==0) CALL write_test_status(total_error, &
          'Testing async APIs', total_error)
     STOP
  ENDIF

  ! Test H5ES routines
  CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)

  CALL h5pset_fapl_mpio_f(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL, hdferror)
  CALL check("h5pset_fapl_mpio_f", hdferror, nerrors)

  CALL H5EScreate_f(es_id, hdferror)
  CALL check("H5EScreate_f", hdferror, nerrors)

  CALL H5ESget_count_f(es_id, count, hdferror)
  CALL check("H5ESget_count_f", hdferror, nerrors)
  CALL VERIFY("H5ESget_count_f", count, 0_SIZE_T,total_error)

  CALL H5Fcreate_async_f(filename, H5F_ACC_TRUNC_F, file_id, es_id, hdferror, &
       access_prp = fapl_id)
  CALL check("h5fcreate_f", hdferror, nerrors)

  CALL H5ESget_count_f(es_id, count, hdferror)
  CALL check("H5ESget_count_f", hdferror, nerrors)
  CALL VERIFY("H5ESget_count_f", count, 2_SIZE_T,total_error)

  CALL H5ESget_op_counter_f(es_id, counter, hdferror)
  CALL check("H5ESget_op_counter_f", hdferror, nerrors)
  CALL VERIFY("H5ESget_op_counter_f", counter, 2_C_INT64_T, total_error)

  CALL H5Pclose_f(fapl_id, hdferror)
  CALL check("h5pclose_f", hdferror, nerrors)

  CALL H5Fclose_async_f(file_id, es_id, hdferror)
  CALL check("h5fclose_f", hdferror, nerrors)

  CALL H5ESget_count_f(es_id, count, hdferror)
  CALL check("H5ESget_count_f", hdferror, nerrors)
  CALL VERIFY("H5ESget_count_f", count, 3_SIZE_T,total_error)

  CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror);
  CALL check("H5ESwait_f", hdferror, nerrors)
  CALL verify("H5ESwait_f", err_occurred, .FALSE., total_error)

  CALL H5ESget_count_f(es_id, count, hdferror)
  CALL check("H5ESget_count_f", hdferror, nerrors)
  CALL VERIFY("H5ESget_count_f", count, 0_SIZE_T,total_error)

  ! Test ASYNC APIs
  CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)

  CALL h5pset_fapl_mpio_f(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL, hdferror)
  CALL check("h5pset_fapl_mpio_f", hdferror, nerrors)

  CALL H5EScreate_f(es_id, hdferror)
  CALL check("H5EScreate_f", hdferror, nerrors)

  CALL H5ESget_count_f(es_id, count, hdferror)
  CALL check("H5ESget_count_f", hdferror, nerrors)
  CALL VERIFY("H5ESget_count_f", count, 0_SIZE_T,total_error)

  CALL H5Fcreate_async_f(filename, H5F_ACC_TRUNC_F, file_id, es_id, hdferror, &
       access_prp = fapl_id, file=pseudo_file, func=pseudo_func, line=pseudo_line)
  CALL check("h5fcreate_f", hdferror, nerrors)

  CALL H5Fclose_async_f(file_id, es_id, hdferror)
  CALL check("h5fclose_f", hdferror, nerrors)

  CALL H5ESget_count_f(es_id, count, hdferror)
  CALL check("H5ESget_count_f", hdferror, nerrors)
  CALL VERIFY("H5ESget_count_f", count, 3_SIZE_T,total_error)

  CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror);
  CALL check("H5ESwait_f", hdferror, nerrors)
  CALL verify("H5ESwait_f", err_occurred, .FALSE., total_error)

  CALL H5ESget_count_f(es_id, count, hdferror)
  CALL check("H5ESget_count_f", hdferror, nerrors)
  CALL VERIFY("H5ESget_count_f", count, 0_SIZE_T,total_error)

  CALL H5ESclose_f(es_id, hdferror)
  CALL check("H5ESclose_f", hdferror, nerrors)

  CALL MPI_REDUCE(nerrors, sum, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, mpierror)
  IF(mpi_rank==0) CALL write_test_status(sum, &
       'Testing H5Async APIs', total_error)

  CALL H5VLclose_f(vol_id, hdferror)
  CALL check("H5VLclose_f", hdferror, total_error)

  !
  ! close HDF5 interface
  !
  CALL h5close_f(hdferror)

  CALL MPI_ALLREDUCE(total_error, sum, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, mpierror)

  !
  ! close MPI
  !
  IF (sum == 0) THEN
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

END PROGRAM async_test
