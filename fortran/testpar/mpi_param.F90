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

#include <H5config_f.inc>

!
! writes/reads dataset by hyperslabs
!

SUBROUTINE mpi_param_03(nerrors)

  USE MPI
  USE HDF5
  USE TH5_MISC
  USE TH5_MISC_GEN

  IMPLICIT NONE
  INTEGER, INTENT(inout) :: nerrors                 ! number of errors

  INTEGER :: hdferror                               ! HDF hdferror flag
  INTEGER(hid_t) :: fapl_id                         ! file access identifier
  INTEGER :: mpi_size, mpi_size_ret ! number of processes in the group of communicator
  INTEGER :: mpierror               ! MPI hdferror flag
  INTEGER :: mpi_rank               ! rank of the calling process in the communicator

  INTEGER :: info, info_ret
  INTEGER :: comm, comm_ret
  INTEGER :: nkeys
  LOGICAL :: flag
  INTEGER :: iconfig
  CHARACTER(LEN=4) , PARAMETER :: in_key="host"
  CHARACTER(LEN=10), PARAMETER :: in_value="myhost.org"

  CHARACTER(LEN=MPI_MAX_INFO_KEY) :: key, value

  ! Get the original sizes
  CALL mpi_comm_rank( MPI_COMM_WORLD, mpi_rank, mpierror )
  IF (mpierror .NE. MPI_SUCCESS) THEN
     WRITE(*,*) "MPI_COMM_RANK  *FAILED*"
     nerrors = nerrors + 1
  ENDIF
  CALL mpi_comm_size( MPI_COMM_WORLD, mpi_size, mpierror )
  IF (mpierror .NE. MPI_SUCCESS) THEN
     WRITE(*,*) "MPI_COMM_SIZE *FAILED* Process = ", mpi_rank
     nerrors = nerrors + 1
  ENDIF

  DO iconfig = 1, 2

     ! Create the file access property
     CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, hdferror)
     CALL check("h5pcreate_f", hdferror, nerrors)

     ! Split the communicator
     IF(mpi_rank.EQ.0)THEN
        CALL MPI_Comm_split(MPI_COMM_WORLD, 1, mpi_rank, comm, mpierror)
        IF (mpierror .NE. MPI_SUCCESS) THEN
           WRITE(*,*) "MPI_COMM_SPLIT *FAILED* Process = ", mpi_rank
           nerrors = nerrors + 1
        ENDIF
     ELSE
        CALL MPI_Comm_split(MPI_COMM_WORLD, 0, mpi_rank, comm, mpierror)
        IF (mpierror .NE. MPI_SUCCESS) THEN
           WRITE(*,*) "MPI_COMM_SPLIT *FAILED* Process = ", mpi_rank
           nerrors = nerrors + 1
        ENDIF
     ENDIF

     ! Create and set an MPI INFO parameter

     CALL MPI_Info_create(info, mpierror)
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_INFO_CREATE *FAILED* Process = ", mpi_rank
        nerrors = nerrors + 1
     ENDIF
     CALL MPI_Info_set(info, in_key, in_value, mpierror )
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_INFO_SET *FAILED* Process = ", mpi_rank
        nerrors = nerrors + 1
     ENDIF

     IF(iconfig.EQ.1)THEN
        ! Set and get the MPI parameters
        CALL h5pset_fapl_mpio_f(fapl_id, comm, info, hdferror)
        CALL check("h5pset_fapl_mpio_f", hdferror, nerrors)

        CALL h5pget_fapl_mpio_f(fapl_id, comm_ret, info_ret, hdferror)
        CALL check("h5pget_fapl_mpio_f", hdferror, nerrors)
     ELSE
        CALL h5pset_mpi_params_f(fapl_id, comm, info, hdferror)
        CALL check("h5pset_mpi_params_f", hdferror, nerrors)

        CALL h5pget_mpi_params_f(fapl_id, comm_ret, info_ret, hdferror)
        CALL check("h5pget_mpi_params_f", hdferror, nerrors)
     ENDIF


     ! Check comm returned
     CALL mpi_comm_size(comm_ret, mpi_size_ret, mpierror)
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_COMM_SIZE *FAILED* Process = ", mpi_rank
        nerrors = nerrors + 1
     ENDIF
     IF (mpi_rank.EQ.0)THEN
        CALL VERIFY("h5pget_fapl_mpio_f", mpi_size_ret, 1, hdferror)
     ELSE
        CALL VERIFY("h5pget_fapl_mpio_f", mpi_size_ret, mpi_size-1, hdferror)
     ENDIF

     ! Check info returned
     CALL MPI_info_get_nkeys( info_ret, nkeys, mpierror)
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_INFO_GET_NKEYS *FAILED* Process = ", mpi_rank
        nerrors = nerrors + 1
     ENDIF
     CALL VERIFY("h5pget_fapl_mpio_f", nkeys, 1, hdferror)

     CALL MPI_Info_get_nthkey(info_ret, 0, key, mpierror)
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_INFO_GET_NTHKEY *FAILED* Process = ", mpi_rank
        nerrors = nerrors + 1
     ENDIF
     CALL VERIFY("h5pget_fapl_mpio_f", TRIM(key), in_key, hdferror)

     CALL MPI_Info_get(info, key, MPI_MAX_INFO_KEY, value, flag, mpierror)
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_INFO_GET *FAILED* Process = ", mpi_rank
        nerrors = nerrors + 1
     ENDIF
     CALL VERIFY("h5pget_fapl_mpio_f", flag, .TRUE., hdferror)
     CALL VERIFY("h5pget_fapl_mpio_f", TRIM(value), in_value, hdferror)

     ! Free the MPI resources
     CALL MPI_info_free(info_ret, mpierror)
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_INFO_FREE *FAILED* Process = ", mpi_rank
        nerrors = nerrors + 1
     ENDIF
     CALL MPI_comm_free(comm_ret, mpierror)
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_COMM_FREE *FAILED* Process = ", mpi_rank
        nerrors = nerrors + 1
     ENDIF
     CALL MPI_info_free(info, mpierror)
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_INFO_FREE *FAILED* Process = ", mpi_rank
        nerrors = nerrors + 1
     ENDIF
     CALL MPI_comm_free(comm, mpierror)
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_COMM_FREE *FAILED* Process = ", mpi_rank
        nerrors = nerrors + 1
     ENDIF

     CALL h5pclose_f(fapl_id, hdferror)
     CALL check("h5pclose_f", hdferror, nerrors)
  ENDDO

END SUBROUTINE mpi_param_03

SUBROUTINE mpi_param_08(nerrors)

#ifdef H5_HAVE_MPI_F08

  USE MPI_F08
  USE HDF5
  USE TH5_MISC
  USE TH5_MISC_GEN

  IMPLICIT NONE
  INTEGER, INTENT(inout) :: nerrors                 ! number of errors

  INTEGER :: hdferror                               ! HDF hdferror flag
  INTEGER(hid_t) :: fapl_id                         ! file access identifier
  INTEGER :: mpi_size, mpi_size_ret ! number of processes in the group of communicator
  INTEGER :: mpierror               ! MPI hdferror flag
  INTEGER :: mpi_rank               ! rank of the calling process in the communicator

  TYPE(MPI_INFO) :: info, info_ret
  TYPE(MPI_COMM) :: comm, comm_ret
  INTEGER :: nkeys
  LOGICAL :: flag
  INTEGER :: iconfig
  CHARACTER(LEN=4) , PARAMETER :: in_key="host"
  CHARACTER(LEN=10), PARAMETER :: in_value="myhost.org"

  CHARACTER(LEN=MPI_MAX_INFO_KEY) :: key, value

  ! Get the original sizes
  CALL mpi_comm_rank( MPI_COMM_WORLD, mpi_rank, mpierror )
  IF (mpierror .NE. MPI_SUCCESS) THEN
     WRITE(*,*) "MPI_COMM_RANK  *FAILED*"
     nerrors = nerrors + 1
  ENDIF
  CALL mpi_comm_size( MPI_COMM_WORLD, mpi_size, mpierror )
  IF (mpierror .NE. MPI_SUCCESS) THEN
     WRITE(*,*) "MPI_COMM_SIZE *FAILED* Process = ", mpi_rank
     nerrors = nerrors + 1
  ENDIF

  DO iconfig = 1, 2

     ! Create the file access property
     CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, hdferror)
     CALL check("h5pcreate_f", hdferror, nerrors)

     ! Split the communicator
     IF(mpi_rank.EQ.0)THEN
        CALL MPI_Comm_split(MPI_COMM_WORLD, 1, mpi_rank, comm, mpierror)
        IF (mpierror .NE. MPI_SUCCESS) THEN
           WRITE(*,*) "MPI_COMM_SPLIT *FAILED* Process = ", mpi_rank
           nerrors = nerrors + 1
        ENDIF
     ELSE
        CALL MPI_Comm_split(MPI_COMM_WORLD, 0, mpi_rank, comm, mpierror)
        IF (mpierror .NE. MPI_SUCCESS) THEN
           WRITE(*,*) "MPI_COMM_SPLIT *FAILED* Process = ", mpi_rank
           nerrors = nerrors + 1
        ENDIF
     ENDIF

     ! Create and set an MPI INFO parameter

     CALL MPI_Info_create(info, mpierror)
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_INFO_CREATE *FAILED* Process = ", mpi_rank
        nerrors = nerrors + 1
     ENDIF
     CALL MPI_Info_set(info, in_key, in_value, mpierror )
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_INFO_SET *FAILED* Process = ", mpi_rank
        nerrors = nerrors + 1
     ENDIF

     IF(iconfig.EQ.1)THEN
        ! Set and get the MPI parameters
        CALL h5pset_fapl_mpio_f(fapl_id, comm, info, hdferror)
        CALL check("h5pset_fapl_mpio_f", hdferror, nerrors)

        CALL h5pget_fapl_mpio_f(fapl_id, comm_ret, info_ret, hdferror)
        CALL check("h5pget_fapl_mpio_f", hdferror, nerrors)
     ELSE
        CALL h5pset_mpi_params_f(fapl_id, comm, info, hdferror)
        CALL check("h5pset_mpi_params_f", hdferror, nerrors)

        CALL h5pget_mpi_params_f(fapl_id, comm_ret, info_ret, hdferror)
        CALL check("h5pget_mpi_params_f", hdferror, nerrors)
     ENDIF


     ! Check comm returned
     CALL mpi_comm_size(comm_ret, mpi_size_ret, mpierror)
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_COMM_SIZE *FAILED* Process = ", mpi_rank
        nerrors = nerrors + 1
     ENDIF
     IF (mpi_rank.EQ.0)THEN
        CALL VERIFY("h5pget_fapl_mpio_f", mpi_size_ret, 1, hdferror)
     ELSE
        CALL VERIFY("h5pget_fapl_mpio_f", mpi_size_ret, mpi_size-1, hdferror)
     ENDIF

     ! Check info returned
     CALL MPI_info_get_nkeys( info_ret, nkeys, mpierror)
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_INFO_GET_NKEYS *FAILED* Process = ", mpi_rank
        nerrors = nerrors + 1
     ENDIF
     CALL VERIFY("h5pget_fapl_mpio_f", nkeys, 1, hdferror)

     CALL MPI_Info_get_nthkey(info_ret, 0, key, mpierror)
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_INFO_GET_NTHKEY *FAILED* Process = ", mpi_rank
        nerrors = nerrors + 1
     ENDIF
     CALL VERIFY("h5pget_fapl_mpio_f", TRIM(key), in_key, hdferror)

     CALL MPI_Info_get(info, key, MPI_MAX_INFO_KEY, value, flag, mpierror)
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_INFO_GET *FAILED* Process = ", mpi_rank
        nerrors = nerrors + 1
     ENDIF
     CALL VERIFY("h5pget_fapl_mpio_f", flag, .TRUE., hdferror)
     CALL VERIFY("h5pget_fapl_mpio_f", TRIM(value), in_value, hdferror)

     ! Free the MPI resources
     CALL MPI_info_free(info_ret, mpierror)
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_INFO_FREE *FAILED* Process = ", mpi_rank
        nerrors = nerrors + 1
     ENDIF
     CALL MPI_comm_free(comm_ret, mpierror)
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_COMM_FREE *FAILED* Process = ", mpi_rank
        nerrors = nerrors + 1
     ENDIF
     CALL MPI_info_free(info, mpierror)
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_INFO_FREE *FAILED* Process = ", mpi_rank
        nerrors = nerrors + 1
     ENDIF
     CALL MPI_comm_free(comm, mpierror)
     IF (mpierror .NE. MPI_SUCCESS) THEN
        WRITE(*,*) "MPI_COMM_FREE *FAILED* Process = ", mpi_rank
        nerrors = nerrors + 1
     ENDIF

     CALL h5pclose_f(fapl_id, hdferror)
     CALL check("h5pclose_f", hdferror, nerrors)
  ENDDO
#else
  INTEGER, INTENT(inout) :: nerrors ! number of errors
  nerrors = -1 ! Skip test
#endif

END SUBROUTINE mpi_param_08

