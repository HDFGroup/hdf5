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
! main program for subfiling HDF5 Fortran tests
!

#include <H5config_f.inc>

PROGRAM subfiling_test
  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT64_T
  USE HDF5
  USE MPI
  USE TH5_MISC

  IMPLICIT NONE

  INTEGER :: total_error = 0        ! sum of the number of errors
  INTEGER :: mpierror               ! MPI hdferror flag
  INTEGER :: mpi_rank               ! rank of the calling process in the communicator

#ifdef H5_HAVE_SUBFILING_VFD

  CHARACTER(LEN=7), PARAMETER :: filename = "subf.h5"

  INTEGER :: hdferror               ! HDF hdferror flag
  INTEGER :: mpi_size, mpi_size_ret ! number of processes in the group of communicator
  INTEGER :: required, provided
  LOGICAL :: file_exists

  INTEGER(HID_T) :: fapl_id
  INTEGER(HID_T) :: file_id
  INTEGER :: comm, comm_ret
  INTEGER :: info, info_ret
  CHARACTER(LEN=3) :: info_val
  CHARACTER(LEN=180) :: subfname
  INTEGER :: i, sum
  INTEGER(C_INT64_T) inode
  TYPE(H5FD_subfiling_config_t) :: vfd_config
  TYPE(H5FD_ioc_config_t)       :: vfd_config_ioc
  LOGICAL :: flag

  INTEGER :: nerrors = 0

  INTEGER(HID_T) :: driver_id

  CHARACTER(len=8) :: hex1, hex2
  CHARACTER(len=1) :: arg

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
     WRITE(*,*) "MPI doesn't support MPI_Init_thread with MPI_THREAD_MULTIPLE *FAILED*"
     nerrors = nerrors + 1
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
  ! initialize the HDF5 fortran interface
  !
  CALL h5open_f(hdferror)

  IF(mpi_rank==0) CALL write_test_header("SUBFILING FORTRAN TESTING")

  ! ***********************************
  ! Test H5Pset/get_mpi_params_f APIs
  ! ***********************************
  nerrors = 0
  IF(mpi_size.GT.2)THEN

     IF (mpi_rank.LE.1)THEN
        CALL MPI_Comm_split(MPI_COMM_WORLD, 1, mpi_rank, comm, mpierror)
     ELSE
        CALL MPI_Comm_split(MPI_COMM_WORLD, 0, mpi_rank, comm, mpierror)
     ENDIF

     CALL MPI_Info_create(info, mpierror)
     CALL MPI_Info_set( info, "foo", "bar", mpierror)

     IF (mpi_rank.LE.1)THEN

        CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, hdferror)
        CALL check("h5pcreate_f", hdferror, nerrors)

        CALL H5Pset_mpi_params_f(fapl_id, comm, info, hdferror)
        CALL check("H5Pset_mpi_params_f", hdferror, nerrors)

        CALL H5Pget_mpi_params_f(fapl_id, comm_ret, info_ret, hdferror)
        CALL check("H5Pget_mpi_params_f", hdferror, nerrors)

        CALL mpi_comm_size(comm_ret, mpi_size_ret, mpierror)

        IF(mpi_size_ret.NE.2)THEN
           IF(mpi_rank.EQ.0) &
                WRITE(*,*) "Failed H5Pset_mpi_params_f and H5Pget_mpi_params_f sequence"
           nerrors = nerrors + 1
        ENDIF

        CALL mpi_info_get(info_ret,"foo", 3, info_val, flag, mpierror)
        IF(flag .EQV. .TRUE.)THEN
           IF(info_val.NE."bar")THEN
              IF(mpi_rank.EQ.0) &
                   WRITE(*,*) "Failed H5Pset_mpi_params_f and H5Pget_mpi_params_f sequence"
              nerrors = nerrors + 1
           ENDIF
        ELSE
           IF(mpi_rank.EQ.0) &
                WRITE(*,*) "Failed to find info value with mpi_info_get"
           nerrors = nerrors + 1
        ENDIF
        CALL h5pclose_f(fapl_id, hdferror)
     ENDIF

     CALL MPI_Comm_free(comm, mpierror)
     CALL MPI_Info_free(info, mpierror)

  ENDIF

  CALL MPI_REDUCE(nerrors, sum, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, mpierror)
  IF(mpi_rank==0) CALL write_test_status(sum, &
       'Testing H5Pset/get_mpi_params_f', total_error)

  ! *********************************************************
  ! Setup file access property list with subfiling I/O access
  ! *********************************************************

  nerrors = 0
  CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)

  CALL H5Pset_mpi_params_f(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL, hdferror)
  CALL check("H5Pset_mpi_params_f", hdferror, nerrors)

  CALL H5Pget_mpi_params_f(fapl_id, comm, info, hdferror)
  CALL check("H5Pset_mpi_params_f", hdferror, nerrors)

  CALL mpi_comm_size(comm, mpi_size_ret, mpierror)
  IF(mpi_size_ret.NE.mpi_size)THEN
     IF(mpi_rank.EQ.0) &
          WRITE(*,*) "Failed H5Pset_mpi_params_f and H5Pget_mpi_params_f sequence"
     nerrors = nerrors + 1
  ENDIF

  IF(mpi_rank==0) CALL write_test_status(nerrors, &
       'Testing H5Pset/get_mpi_params_f with defaults ', total_error)

  ! Verify no new enum parameters have been added in C and not updated in Fortran
  IF( IOC_SELECTION_OPTIONS_F .NE. 4)THEN
     IF(mpi_rank.EQ.0) &
          WRITE(*,*) "Mismatch between Fortran and C H5FD_subfiling_ioc_select_t definitions"
     nerrors = nerrors + 1
  ENDIF

  IF(mpi_rank==0) CALL write_test_status(nerrors, &
       'Testing Subfiling FD is registered', total_error)

  ! *********************************************************
  ! Check the default subfiling parameters
  ! *********************************************************
  nerrors = 0
  CALL h5pset_fapl_subfiling_f(fapl_id, hdferror)
  CALL check("h5pset_fapl_subfiling_f", hdferror, nerrors)

  CALL h5pget_driver_f(fapl_id, driver_id, hdferror)
  CALL check("h5pget_driver_f", hdferror, nerrors)

  IF( driver_id .NE. H5FD_SUBFILING_F) THEN
     WRITE(*,*) "Wrong file driver type returned"
     nerrors = nerrors + 1
  ENDIF

  ! *********************************************************
  ! Check the default parameters for subfiling and ioc
  ! *********************************************************

  CALL h5pget_fapl_subfiling_f(fapl_id, vfd_config, hdferror)
  CALL check("h5pget_fapl_subfiling_f", hdferror, nerrors)

  CALL h5pset_fapl_ioc_f(vfd_config%ioc_fapl_id, hdferror)
  CALL check("h5pset_fapl_ioc_f", hdferror, nerrors)

  CALL h5pget_fapl_ioc_f(vfd_config%ioc_fapl_id, vfd_config_ioc, hdferror)
  CALL check("h5pget_fapl_ioc_f", hdferror, nerrors)

  WRITE(hex1,'(z8)') H5FD_SUBFILING_FAPL_MAGIC_F
  WRITE(hex2,'(z8)') vfd_config%magic

  IF(hex1 .NE. hex2 .OR. &
     vfd_config%version .NE. H5FD_SUBFILING_CURR_FAPL_VERSION_F .OR. &
     .NOT.vfd_config%require_ioc .OR. &
     vfd_config%shared_cfg%ioc_selection .NE. SELECT_IOC_ONE_PER_NODE_F .OR. &
     vfd_config%shared_cfg%stripe_size .NE. H5FD_SUBFILING_DEFAULT_STRIPE_SIZE_F .OR. &
     vfd_config%shared_cfg%stripe_count .NE. H5FD_SUBFILING_DEFAULT_STRIPE_COUNT_F &
     )THEN
     IF(mpi_rank.EQ.0) &
          WRITE(*,*) "Failed h5pget_fapl_subfiling_f"
     nerrors = nerrors + 1
  ENDIF

  IF(mpi_rank==0) CALL write_test_status(nerrors, &
       'Testing H5Pset/get_fapl_subfiling_f with defaults', total_error)

  WRITE(hex1,'(z8)') H5FD_IOC_FAPL_MAGIC_F
  WRITE(hex2,'(z8)') vfd_config_ioc%magic

  nerrors = 0
  IF(hex1 .NE. hex2 .OR. &
     vfd_config_ioc%version .NE. H5FD_IOC_CURR_FAPL_VERSION_F .OR. &
     vfd_config_ioc%thread_pool_size .NE. H5FD_IOC_DEFAULT_THREAD_POOL_SIZE_F &
     )THEN
     IF(mpi_rank.EQ.0) &
          WRITE(*,*) "Failed h5pget_fapl_ioc_f"
     nerrors = nerrors + 1
  ENDIF

  IF(mpi_rank==0) CALL write_test_status(nerrors, &
       'Testing H5Pset/get_fapl_ioc_f with defaults', total_error)

  ! *********************************************************
  ! Testing creating a file with subfiling, default settings
  ! *********************************************************

  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, hdferror, access_prp = fapl_id)
  CALL check("h5fcreate_f", hdferror, nerrors)

  CALL h5fclose_f(file_id, hdferror)
  CALL check("h5fclose_f", hdferror, nerrors)

  IF(mpi_rank==0) CALL write_test_status(nerrors, &
       'Testing H5Fcreate with subfiling with default settings', total_error)

  ! *********************************************************
  ! Testing creating a file with subfiling, modified settings
  ! *********************************************************

  ! Testing modifying defaults for subfiling FD

  vfd_config%magic = H5FD_SUBFILING_FAPL_MAGIC_F
  vfd_config%version = H5FD_SUBFILING_CURR_FAPL_VERSION_F
  vfd_config%require_ioc = .TRUE.
  vfd_config%shared_cfg%ioc_selection = SELECT_IOC_ONE_PER_NODE_F
  vfd_config%shared_cfg%stripe_size = 16*1024*1024
  vfd_config%shared_cfg%stripe_count = 3

  nerrors = 0
  CALL h5pset_fapl_subfiling_f(fapl_id, hdferror, vfd_config)
  CALL check("h5pset_fapl_ioc_f", hdferror, nerrors)

  CALL h5pget_fapl_subfiling_f(fapl_id, vfd_config, hdferror)
  CALL check("h5pget_fapl_ioc_f", hdferror, nerrors)

  WRITE(hex1,'(z8)') H5FD_SUBFILING_FAPL_MAGIC_F
  WRITE(hex2,'(z8)') vfd_config%magic

  IF(hex1 .NE. hex2 .OR. &
     vfd_config%version .NE. H5FD_SUBFILING_CURR_FAPL_VERSION_F .OR. &
     .NOT.vfd_config%require_ioc .OR. &
     vfd_config%shared_cfg%ioc_selection .NE. SELECT_IOC_ONE_PER_NODE_F .OR. &
     vfd_config%shared_cfg%stripe_size .NE. 16*1024*1024 .OR. &
     vfd_config%shared_cfg%stripe_count .NE. 3 &
     )THEN
     IF(mpi_rank.EQ.0) &
          WRITE(*,*) "Failed h5pget_fapl_subfiling_f"
     nerrors = nerrors + 1
  ENDIF

  IF(mpi_rank==0) CALL write_test_status(nerrors, &
       'Testing H5Pset/get_fapl_subfiling_f with custom settings', total_error)

  vfd_config_ioc%magic   = H5FD_IOC_FAPL_MAGIC_F
  vfd_config_ioc%version = H5FD_IOC_CURR_FAPL_VERSION_F
  vfd_config_ioc%thread_pool_size = 2

  nerrors = 0
  CALL h5pset_fapl_ioc_f(vfd_config%ioc_fapl_id, hdferror, vfd_config_ioc)
  CALL check("h5pset_fapl_ioc_f", hdferror, nerrors)

  CALL h5pget_fapl_ioc_f(vfd_config%ioc_fapl_id, vfd_config_ioc, hdferror)
  CALL check("h5pget_fapl_ioc_f", hdferror, nerrors)

  IF(& !vfd_config_ioc%magic .NE. H5FD_IOC_FAPL_MAGIC_F .OR. &
     vfd_config_ioc%version .NE. H5FD_IOC_CURR_FAPL_VERSION_F .OR. &
     vfd_config_ioc%thread_pool_size .NE. 2 &
     )THEN
     IF(mpi_rank.EQ.0) &
          WRITE(*,*) "Failed h5pget_fapl_ioc_f"
     nerrors = nerrors + 1
  ENDIF
  IF(mpi_rank==0) CALL write_test_status(nerrors, &
       'Testing H5Pset/get_fapl_ioc_f with custom settings', total_error)

  ! *********************************************************
  ! Testing creating a file with subfiling, custom settings
  ! *********************************************************

  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, hdferror, access_prp = fapl_id)
  CALL check("h5pcreate_f", hdferror, nerrors)

  CALL h5fclose_f(file_id, hdferror)
  CALL check("h5fclose_f", hdferror, nerrors)

  IF(mpi_rank.EQ.0)THEN
     INQUIRE(FILE=filename, EXIST=file_exists)
     IF(.NOT.file_exists)THEN
        WRITE(*,"(A,A)") "Failed to find the stub subfile ",TRIM(filename)
        nerrors = nerrors + 1
     ENDIF
#ifdef H5_HAVE_DARWIN
     arg(1:1)="f"
#else
     arg(1:1)="c"
#endif
     CALL EXECUTE_COMMAND_LINE("stat -"//arg(1:1)//" %i "//filename//" >> tmp_inode", EXITSTAT=i)
     IF(i.ne.0)THEN
        WRITE(*,"(A,A)") "Failed to stat the stub subfile ",TRIM(filename)
        nerrors = nerrors + 1
     ENDIF

     OPEN(11,FILE="tmp_inode")
     READ(11,*) inode
     CLOSE(11,STATUS="delete")

     DO i = 1, vfd_config%shared_cfg%stripe_count
        WRITE(subfname,'(A,".subfile_",I0,"_",I0,"_of_",I0)') filename,inode,i,vfd_config%shared_cfg%stripe_count
        INQUIRE(FILE=subfname, EXIST=file_exists)
        IF(.NOT.file_exists)THEN
           WRITE(*,"(A,A)") "Failed to create the subfile ",TRIM(subfname)
           nerrors = nerrors + 1
        ENDIF
     ENDDO

  ENDIF

  CALL h5pclose_f(fapl_id, hdferror)
  CALL check("h5pclose_f", hdferror, nerrors)

  IF(mpi_rank==0) CALL write_test_status(nerrors, &
       'Testing H5Fcreate with subfiling with custom settings', total_error)

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

  IF(mpi_rank==0) CALL write_test_footer()

  !
  ! end main program
  !

#else

  CALL mpi_init(mpierror)
  CALL mpi_comm_rank(MPI_COMM_WORLD, mpi_rank, mpierror)

  IF(mpi_rank==0) THEN
     CALL write_test_header("SUBFILING FORTRAN TESTING")
     CALL write_test_status( -1, 'Subfiling not enabled', total_error)
     CALL write_test_footer()
  ENDIF

  CALL mpi_finalize(mpierror)

#endif

END PROGRAM subfiling_test
