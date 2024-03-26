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
! Tests async Fortran wrappers. It needs an async VOL. It will skip the tests if
! HDF5_VOL_CONNECTOR is not set or is set to a non-supporting async VOL.
!
#include <H5config_f.inc>

MODULE test_async_APIs

#ifdef H5_HAVE_MPI_F08
  USE MPI_F08
#else
  USE MPI
#endif
  USE HDF5
  USE TH5_MISC
  USE TH5_MISC_GEN

  INTEGER(C_INT), PARAMETER :: op_data_type = 200
  INTEGER(C_INT), PARAMETER :: op_data_command = 99

  LOGICAL :: async_enabled = .TRUE.
  LOGICAL :: mpi_thread_mult = .TRUE.

  LOGICAL(C_BOOL), PARAMETER :: logical_true = .TRUE.
  LOGICAL(C_BOOL), PARAMETER :: logical_false = .FALSE.

  ! Custom group iteration callback data
  TYPE, bind(c) ::  iter_info
     CHARACTER(KIND=C_CHAR), DIMENSION(1:12) :: name !  The name of the object
     INTEGER(c_int) :: TYPE    !  The TYPE of the object
     INTEGER(c_int) :: command ! The TYPE of RETURN value
  END TYPE iter_info

  CHARACTER(LEN=10), TARGET :: app_file = "async.F90"//C_NULL_CHAR
  CHARACTER(LEN=10), TARGET :: app_func = "func_name"//C_NULL_CHAR
  INTEGER :: app_line = 42

  INTEGER :: mpi_ikind = MPI_INTEGER_KIND

CONTAINS

  INTEGER(KIND=C_INT) FUNCTION liter_cb(group, name, link_info, op_data) bind(C)

    IMPLICIT NONE

    INTEGER(HID_T), VALUE :: group
    CHARACTER(LEN=1), DIMENSION(1:12) :: name
    TYPE (H5L_info_t) :: link_info
    TYPE(iter_info) :: op_data

    liter_cb = 0

    op_data%name(1:12) = name(1:12)

    SELECT CASE (op_data%command)

    CASE(0)
       liter_cb = 0
    CASE(2)
       liter_cb = op_data%command*10_C_INT
    END SELECT
    op_data%command = op_data_command
    op_data%type = op_data_type

  END FUNCTION liter_cb

  SUBROUTINE H5ES_tests(cleanup, total_error)
    !
    ! Test H5ES routines
    !
    IMPLICIT NONE
    LOGICAL, INTENT(IN)  :: cleanup
    INTEGER, INTENT(INOUT) :: total_error

    INTEGER :: nerrors = 0
    INTEGER(HID_T) :: fapl_id
    INTEGER(HID_T) :: file_id
    CHARACTER(len=80) :: filename = "h5es_tests.h5"
    INTEGER :: hdferror

    INTEGER(HID_T)  :: es_id
    INTEGER(SIZE_T)    :: count
    INTEGER(C_INT64_T) :: counter
    INTEGER(SIZE_T) :: num_not_canceled
    INTEGER(SIZE_T) :: num_in_progress
    LOGICAL         :: err_occurred

    CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, hdferror)
    CALL check("h5pcreate_f", hdferror, nerrors)

    CALL h5pset_fapl_mpio_f(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL, hdferror)
    CALL check("h5pset_fapl_mpio_f", hdferror, nerrors)

    CALL H5EScreate_f(es_id, hdferror)
    CALL check("H5EScreate_f", hdferror, nerrors)

    CALL H5ESget_count_f(es_id, count, hdferror)
    CALL check("H5ESget_count_f", hdferror, nerrors)
    CALL VERIFY("H5ESget_count_f", count, 0_SIZE_T,total_error)

    CALL H5EScancel_f(es_id, num_not_canceled, err_occurred, hdferror)
    CALL check("H5EScancel_f", hdferror, nerrors)
    CALL VERIFY("H5EScancel_f", num_not_canceled, 0_size_t, total_error)
    CALL VERIFY("H5EScancel_f", err_occurred, .FALSE., total_error)

    CALL H5Fcreate_async_f(filename, H5F_ACC_TRUNC_F, file_id, es_id, hdferror, access_prp = fapl_id)
    CALL check("h5fcreate_f", hdferror, nerrors)

    CALL H5ESget_count_f(es_id, count, hdferror)
    CALL check("H5ESget_count_f", hdferror, nerrors)
    IF(async_enabled)THEN
       CALL VERIFY("H5ESget_count_f", count, 2_SIZE_T,total_error)
    ELSE
       CALL VERIFY("H5ESget_count_f", count, 0_SIZE_T,total_error)
    ENDIF

    CALL H5ESget_op_counter_f(es_id, counter, hdferror)
    CALL check("H5ESget_op_counter_f", hdferror, nerrors)
    IF(async_enabled)THEN
       CALL VERIFY("H5ESget_op_counter_f", counter, 2_C_INT64_T, total_error)
    ELSE
       CALL VERIFY("H5ESget_op_counter_f", counter, 0_C_INT64_T, total_error)
    ENDIF

    CALL H5Pclose_f(fapl_id, hdferror)
    CALL check("h5pclose_f", hdferror, nerrors)

    CALL H5Fclose_async_f(file_id, es_id, hdferror)
    CALL check("h5fclose_f", hdferror, nerrors)
    CALL H5ESget_count_f(es_id, count, hdferror)
    CALL check("H5ESget_count_f", hdferror, nerrors)
    IF(async_enabled)THEN
       CALL VERIFY("H5ESget_count_f", count, 3_SIZE_T,total_error)
    ELSE
       CALL VERIFY("H5ESget_count_f", count, 0_SIZE_T,total_error)
    ENDIF

    CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror);
    CALL check("H5ESwait_f", hdferror, nerrors)
    CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)

    CALL H5ESget_count_f(es_id, count, hdferror)
    CALL check("H5ESget_count_f", hdferror, nerrors)
    CALL VERIFY("H5ESget_count_f", count, 0_SIZE_T,total_error)

    CALL H5ESclose_f(es_id, hdferror)
    CALL check("H5ESclose_f", hdferror, nerrors)

  END SUBROUTINE H5ES_tests

  SUBROUTINE H5A_async_tests(cleanup, total_error)
    !
    ! Test H5A async routines
    !
    IMPLICIT NONE
    LOGICAL, INTENT(IN)  :: cleanup
    INTEGER, INTENT(INOUT) :: total_error

    INTEGER(HID_T) :: fapl_id
    INTEGER(HID_T) :: file_id
    CHARACTER(len=80) :: filename = "h5a_tests.h5"
    INTEGER :: hdferror
    INTEGER(HID_T)  :: es_id
    INTEGER(SIZE_T) :: num_in_progress
    LOGICAL         :: err_occurred

    CHARACTER(LEN=4), PARAMETER  :: attr_name = "ATTR"
    INTEGER, TARGET :: attr_data0 = 100
    INTEGER, TARGET :: attr_data1 = 101
    INTEGER, TARGET :: attr_data2 = 101
    INTEGER, TARGET :: attr_rdata0
    INTEGER, TARGET :: attr_rdata1
    INTEGER, TARGET :: attr_rdata2
    INTEGER(HID_T) :: space_id
    INTEGER(HID_T) :: attr_id0, attr_id1, attr_id2
    LOGICAL :: exists
    LOGICAL(C_BOOL), TARGET :: exists0=logical_false, exists1=logical_false, exists2=logical_false, exists3=logical_false
    TYPE(C_PTR) :: f_ptr, f_ptr1, f_ptr2

    CALL H5EScreate_f(es_id, hdferror)
    CALL check("H5EScreate_f", hdferror, total_error)
    !
    ! Create the file.
    !
    CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, hdferror)
    CALL check("h5pcreate_f", hdferror, total_error)

    CALL h5pset_fapl_mpio_f(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL, hdferror)
    CALL check("h5pset_fapl_mpio_f", hdferror, total_error)

    CALL h5fcreate_async_f(filename, H5F_ACC_TRUNC_F, file_id, es_id, hdferror, access_prp = fapl_id )
    CALL check("h5fcreate_f",hdferror, total_error)

    CALL H5Screate_f(H5S_SCALAR_F, space_id, hdferror)
    CALL check("H5Screate_f", hdferror, total_error)

    f_ptr1 = C_LOC(app_file(1:1))
    f_ptr2 = C_LOC(app_func(1:1))
    CALL h5acreate_async_f(file_id, attr_name, H5T_NATIVE_INTEGER, space_id, attr_id0, es_id, hdferror, &
         file=f_ptr1, func=f_ptr2, line=app_line)
    CALL check("h5acreate_f",hdferror,total_error)

    f_ptr = C_LOC(attr_data0)
    CALL H5Awrite_async_f(attr_id0, H5T_NATIVE_INTEGER, f_ptr, es_id, hdferror)
    CALL check("H5Awrite_async_f",hdferror,total_error)

    CALL H5Aclose_async_f(attr_id0, es_id, hdferror)
    CALL check("H5Aclose_async_f",hdferror,total_error)
    
    CALL h5acreate_by_name_async_f(file_id, "/", TRIM(attr_name)//"00", &
         H5T_NATIVE_INTEGER, space_id, attr_id1, es_id, hdferror)
    CALL check("h5acreate_by_name_async_f",hdferror,total_error)

    CALL h5acreate_by_name_async_f(file_id, "/", TRIM(attr_name)//"01", &
         H5T_NATIVE_INTEGER, space_id, attr_id2, es_id, hdferror)
    CALL check("h5acreate_by_name_async_f",hdferror,total_error)

    f_ptr = C_LOC(attr_data1)
    CALL H5Awrite_async_f(attr_id1, H5T_NATIVE_INTEGER, f_ptr, es_id, hdferror)
    CALL check("H5Awrite_async_f",hdferror,total_error)

    CALL H5Aclose_async_f(attr_id1, es_id, hdferror)
    CALL check("H5Aclose_async_f",hdferror,total_error)

    f_ptr = C_LOC(attr_data2)
    CALL H5Awrite_async_f(attr_id2, H5T_NATIVE_INTEGER, f_ptr, es_id, hdferror)
    CALL check("H5Awrite_async_f",hdferror,total_error)

    CALL H5Aclose_async_f(attr_id2, es_id, hdferror)
    CALL check("H5Aclose_async_f",hdferror,total_error)

    CALL H5Sclose_f(space_id, hdferror)
    CALL check("H5Sclose_f",hdferror,total_error)
    CALL H5Fclose_async_f(file_id, es_id, hdferror)
    CALL check("H5Fclose_async_f",hdferror, total_error)

    CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror)
    CALL check("H5ESwait_f", hdferror, total_error)
    CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)

    CALL h5fopen_async_f(filename, H5F_ACC_RDWR_F, file_id, es_id, hdferror, access_prp = fapl_id )
    CALL check("h5fopen_async_f",hdferror, total_error)

    f_ptr = C_LOC(exists0)
    CALL H5Aexists_async_f(file_id, attr_name, f_ptr, es_id, hdferror)
    CALL check("H5Aexists_async_f",hdferror, total_error)

    f_ptr = C_LOC(exists1)
    CALL H5Aexists_async_f(file_id, TRIM(attr_name)//"00", f_ptr, es_id, hdferror)
    CALL check("H5Aexists_async_f",hdferror, total_error)

    f_ptr = C_LOC(exists2)
    CALL H5Aexists_by_name_async_f(file_id, "/", attr_name, f_ptr, es_id, hdferror)
    CALL check("H5Aexists_by_name_async_f",hdferror, total_error)

    f_ptr = C_LOC(exists3)
    CALL H5Aexists_by_name_async_f(file_id, "/", TRIM(attr_name)//"00", f_ptr, es_id, hdferror)
    CALL check("H5Aexists_by_name_async_f",hdferror, total_error)

    CALL H5Aopen_async_f(file_id, attr_name, attr_id0, es_id, hdferror)
    CALL check("H5Aopen_async_f", hdferror, total_error)

    f_ptr = C_LOC(attr_rdata0)
    CALL H5Aread_async_f(attr_id0, H5T_NATIVE_INTEGER, f_ptr, es_id, hdferror)
    CALL check("H5Aread_async_f", hdferror, total_error)

    CALL H5Aclose_async_f(attr_id0, es_id, hdferror)
    CALL check("H5Aclose_async_f",hdferror,total_error)

    CALL H5Aopen_by_name_async_f(file_id, "/", TRIM(attr_name)//"00", attr_id1, es_id, hdferror)
    CALL check("H5Aopen_by_name_async_f", hdferror, total_error)

    f_ptr = C_LOC(attr_rdata1)
    CALL H5Aread_async_f(attr_id1, H5T_NATIVE_INTEGER, f_ptr, es_id, hdferror)
    CALL check("H5Aread_async_f", hdferror, total_error)

    CALL H5Aclose_async_f(attr_id1, es_id, hdferror)
    CALL check("H5Aclose_async_f",hdferror,total_error)

    CALL H5Aopen_by_idx_async_f(file_id, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_INC_F, INT(2,HSIZE_T), attr_id2, es_id, hdferror)
    CALL check("H5Aopen_by_idx_async_f", hdferror, total_error)

    f_ptr = C_LOC(attr_rdata2)
    CALL H5Aread_async_f(attr_id2, H5T_NATIVE_INTEGER, f_ptr, es_id, hdferror)
    CALL check("H5Aread_async_f", hdferror, total_error)

    CALL H5Aclose_async_f(attr_id2, es_id, hdferror)
    CALL check("H5Aclose_async_f",hdferror,total_error)

    CALL H5Arename_async_f(file_id, TRIM(attr_name)//"00", TRIM(attr_name)//"05", es_id, hdferror)
    CALL check("H5Arename_async_f",hdferror,total_error)

    CALL H5Arename_by_name_async_f(file_id, ".", TRIM(attr_name)//"01", TRIM(attr_name)//"06", es_id, hdferror)
    CALL check("H5Arename_by_name_async_f",hdferror,total_error)

    CALL H5Fclose_async_f(file_id, es_id, hdferror)
    CALL check("H5Fclose_async_f",hdferror,total_error)

    CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror)
    CALL check("H5ESwait_f", hdferror, total_error)
    CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)

    CALL VERIFY("H5Aexists_async_f", exists0, logical_true, total_error)
    CALL VERIFY("H5Aexists_async_f", exists1, logical_true, total_error)
    CALL VERIFY("H5Aexists_by_name_async_f", exists2, logical_true, total_error)
    CALL VERIFY("H5Aexists_by_name_async_f", exists3, logical_true, total_error)

    CALL VERIFY("H5Aread_async_f", attr_rdata0, attr_data0, total_error)
    CALL VERIFY("H5Aread_async_f", attr_rdata1, attr_data1, total_error)
    CALL VERIFY("H5Aread_async_f", attr_rdata2, attr_data2, total_error)

    CALL h5fopen_f(filename, H5F_ACC_RDWR_F, file_id, hdferror, access_prp = fapl_id )
    CALL check("h5fopen_f",hdferror, total_error)

    CALL H5Aexists_f(file_id, TRIM(attr_name)//"05", exists, hdferror)
    CALL check("H5Aexist_f",hdferror, total_error)
    CALL VERIFY("H5Arename_async_f", exists, .TRUE., total_error)

    CALL H5Aexists_f(file_id, TRIM(attr_name)//"06", exists, hdferror)
    CALL check("H5Aexist_f",hdferror, total_error)
    CALL VERIFY("H5Arename_by_name_async_f", exists, .TRUE., total_error)

    CALL H5Aexists_f(file_id, TRIM(attr_name)//"01", exists, hdferror)
    CALL check("H5Aexist_f",hdferror, total_error)
    CALL VERIFY("H5Arename_async_f", exists, .FALSE., total_error)

    CALL H5Aexists_f(file_id, TRIM(attr_name)//"02", exists, hdferror)
    CALL check("H5Aexist_f",hdferror, total_error)
    CALL VERIFY("H5Arename_by_name_async_f", exists, .FALSE., total_error)

    CALL H5Fclose_f(file_id, hdferror)
    CALL check("H5Fclose_f",hdferror,total_error)

    CALL H5Pclose_f(fapl_id, hdferror)
    CALL check(" H5Pclose_f",hdferror, total_error)

    CALL H5ESclose_f(es_id, hdferror)
    CALL check("H5ESclose_f", hdferror, total_error)

  END SUBROUTINE H5A_async_tests

  SUBROUTINE H5D_async_tests(cleanup, total_error)
    !
    ! Test H5D async routines
    !
    IMPLICIT NONE
    LOGICAL, INTENT(IN)  :: cleanup
    INTEGER, INTENT(INOUT) :: total_error

    INTEGER(HID_T) :: fapl_id
    INTEGER(HID_T) :: file_id
    CHARACTER(len=80) :: filename = "h5d_tests.h5"
    INTEGER :: hdferror
    INTEGER(HID_T)  :: es_id
    INTEGER(SIZE_T) :: num_in_progress
    LOGICAL         :: err_occurred

    CHARACTER(LEN=8), PARAMETER :: dsetname = "IntArray"
    INTEGER(HID_T) :: crp_list       ! File identifier
    INTEGER(HID_T) :: dset_id      ! Dataset identifier

    INTEGER(HID_T) :: filespace     ! Dataspace identifier in file
    INTEGER(HID_T) :: memspace      ! Dataspace identifier in memory
    INTEGER(HID_T) :: xfer_prp       ! Property list identifier
    TYPE(C_PTR) :: f_ptr

    INTEGER(HSIZE_T), DIMENSION(1) :: dims = (/4/)
    INTEGER(HSIZE_T), DIMENSION(1) :: dimsf

    INTEGER(HSIZE_T), DIMENSION(1) :: count
    INTEGER(HSSIZE_T), DIMENSION(1) :: offset
    INTEGER, ALLOCATABLE, DIMENSION(:), TARGET :: idata
    INTEGER, ALLOCATABLE, DIMENSION(:), TARGET :: rdata
    INTEGER(HSIZE_T), DIMENSION(1) :: idims, imaxdims
    INTEGER(HSIZE_T), DIMENSION(1) :: maxdims
    INTEGER(HSIZE_T) :: i
    INTEGER(HSIZE_T), DIMENSION(1) :: extend_dim
    INTEGER, TARGET :: fillvalue = 99

    INTEGER :: error  ! Error flags
    INTEGER(KIND=MPI_INTEGER_KIND) :: mpierror       ! MPI error flag
#ifdef H5_HAVE_MPI_F08
    TYPE(MPI_COMM) :: comm
    TYPE(MPI_INFO) :: info
#else
    INTEGER(KIND=MPI_INTEGER_KIND) :: comm, info
#endif
    INTEGER(KIND=MPI_INTEGER_KIND) :: mpi_size, mpi_rank

    comm = MPI_COMM_WORLD
    info = MPI_INFO_NULL

    CALL MPI_COMM_SIZE(comm, mpi_size, mpierror)
    CALL MPI_COMM_RANK(comm, mpi_rank, mpierror)

    CALL H5EScreate_f(es_id, hdferror)
    CALL check("H5EScreate_f", hdferror, total_error)
    !
    ! Create the file.
    !
    CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, hdferror)
    CALL check("h5pcreate_f", hdferror, total_error)

    CALL h5pset_fapl_mpio_f(fapl_id, comm, info, hdferror)
    CALL check("h5pset_fapl_mpio_f", hdferror, total_error)

    CALL h5fcreate_async_f(filename, H5F_ACC_TRUNC_F, file_id, es_id, error, access_prp = fapl_id )
    CALL check("h5fcreate_f",hdferror, total_error)

    dimsf(1) = dims(1)*mpi_size
    ALLOCATE(idata(1:dims(1)))

    idata(:) = mpi_rank

    CALL h5pcreate_f(H5P_DATASET_CREATE_F, crp_list, hdferror)
    CALL h5pset_chunk_f(crp_list, 1, dims, error)
    f_ptr = C_LOC(fillvalue)
    CALL h5pset_fill_value_f(crp_list, H5T_NATIVE_INTEGER, f_ptr, hdferror)

    !
    ! Create data space for the dataset.
    !
    maxdims(1) = H5S_UNLIMITED_F
    CALL h5screate_simple_f(1, dimsf, filespace, hdferror, maxdims)
    CALL check("h5screate_simple_f", hdferror, total_error)

    !
    ! create contiguous dataset in the file.
    CALL h5dcreate_async_f(file_id, dsetname, H5T_NATIVE_INTEGER, filespace, &
         dset_id, es_id, hdferror, crp_list)
    CALL check("h5dcreate_async_f", hdferror, total_error)

    COUNT(1)  = dims(1)
    offset(1) = mpi_rank * COUNT(1)
    CALL h5screate_simple_f(1, dims(1), memspace, hdferror)
    CALL check("h5screate_simple_f", hdferror, total_error)

    CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, offset, count, hdferror)
    CALL check("h5sselect_hyperslab_f", hdferror, total_error)

    CALL h5pcreate_f(H5P_DATASET_XFER_F, xfer_prp, error)
    CALL h5pset_dxpl_mpio_f(xfer_prp, H5FD_MPIO_COLLECTIVE_F, error)

    !
    ! Write data to the dataset
    !
    f_ptr = C_LOC(idata)
    CALL h5dwrite_async_f(dset_id, H5T_NATIVE_INTEGER, f_ptr, es_id, hdferror, &
         mem_space_id = memspace, file_space_id = filespace, xfer_prp = xfer_prp)
    CALL check("h5dwrite_async_f", hdferror, total_error)
    !
    ! Terminate access to the dataset.
    !
    CALL h5dclose_async_f(dset_id, es_id, error)
    CALL check("h5dclose_f",error,total_error)

    !
    ! Close dataspaces.
    !
    CALL h5sclose_f(filespace, hdferror)
    CALL check("h5sclose_f",hdferror,total_error)
    CALL h5sclose_f(memspace, error)
    CALL check("h5sclose_f",hdferror,total_error)
    CALL h5pclose_f(crp_list, hdferror)
    CALL check("h5pclose_f",hdferror,total_error)

    !
    ! Close the file.
    !
    CALL h5fclose_async_f(file_id, es_id, hdferror)
    CALL check("h5fclose_async_f",hdferror,total_error)

    ! Complete the operations
    CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror);
    CALL check("H5ESwait_f", hdferror, total_error)
    CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)
    CALL VERIFY("H5ESwait_f", num_in_progress, 0_size_t , total_error)

    CALL h5fopen_async_f(filename, H5F_ACC_RDWR_F, file_id, es_id, hdferror, access_prp = fapl_id)
    CALL check("h5fopen_async_f",hdferror,total_error)

    CALL h5dopen_async_f(file_id, dsetname, dset_id, es_id, hdferror)
    CALL check("h5dopen_async_f",hdferror,total_error)

    CALL H5Dget_space_async_f(dset_id, filespace, es_id, hdferror)
    CALL check("h5dopen_async_f",hdferror,total_error)

    CALL h5sget_simple_extent_dims_f(filespace, idims, imaxdims, hdferror)
    CALL check("h5sget_simple_extent_dims_f", hdferror, total_error)
    CALL VERIFY("h5sget_simple_extent_dims_f", idims(1), dimsf(1), total_error)
    CALL VERIFY("h5sget_simple_extent_dims_f", imaxdims(1), H5S_UNLIMITED_F, total_error)

    ! Check reading the data back
    ALLOCATE(rdata(1:dims(1)))

    CALL h5screate_simple_f(1, dims(1), memspace, hdferror)
    CALL check("h5screate_simple_f", hdferror, total_error)

    CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, offset, count, hdferror)
    CALL check("h5sselect_hyperslab_f", hdferror, total_error)

    f_ptr = C_LOC(rdata)
    CALL h5dread_async_f(dset_id, H5T_NATIVE_INTEGER, f_ptr, es_id, hdferror, &
         mem_space_id = memspace, file_space_id = filespace, xfer_prp = xfer_prp)
    CALL check("h5dread_async_f", hdferror, total_error)

    CALL h5sclose_f(filespace, hdferror)
    CALL check("h5sclose_f",hdferror,total_error)

    CALL h5sclose_f(memspace, hdferror)
    CALL check("h5sclose_f",hdferror,total_error)

    ! Extend the dataset
    extend_dim(1) = dimsf(1)*2
    CALL H5Dset_extent_async_f(dset_id, extend_dim, es_id, hdferror)
    CALL check("H5Dset_extent_async_f", error, total_error)

    CALL h5dclose_async_f(dset_id, es_id, error)
    CALL check("h5dclose_async_f",error,total_error)

    CALL h5fclose_async_f(file_id, es_id, hdferror)
    CALL check("h5fclose_async_f",hdferror,total_error)

    CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror)
    CALL check("H5ESwait_f", hdferror, total_error)
    CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)

    ! Verify the data read
    DO i = 1, dims(1)
       CALL VERIFY("h5dread_f", idata(i), rdata(i), total_error)
    ENDDO

    CALL h5fopen_f(filename, H5F_ACC_RDWR_F, file_id, hdferror, access_prp = fapl_id )
    CALL check("h5fopen_f",error,total_error)

    CALL h5dopen_f(file_id, dsetname, dset_id, hdferror)
    CALL check("h5dopen_async_f",hdferror,total_error)

    CALL H5Dget_space_f(dset_id, filespace, hdferror)
    CALL check("h5dopen_async_f",hdferror,total_error)

    CALL H5Sget_simple_extent_dims_f(filespace, idims, imaxdims, hdferror)
    CALL check("h5sget_simple_extent_dims_f", hdferror, total_error)
    CALL VERIFY("h5sget_simple_extent_dims_f", idims(1), extend_dim(1), total_error)
    CALL VERIFY("h5sget_simple_extent_dims_f", imaxdims(1), H5S_UNLIMITED_F, total_error)

    CALL h5sclose_f(filespace, hdferror)
    CALL check("h5sclose_f",hdferror,total_error)

    CALL h5dclose_f(dset_id, error)
    CALL check("h5dclose_f",error,total_error)

    CALL h5fclose_f(file_id, hdferror)
    CALL check("h5fclose_f",hdferror,total_error)

  END SUBROUTINE H5D_async_tests


  SUBROUTINE H5G_async_tests(cleanup, total_error)
    !
    ! Test H5G async routines
    !
    IMPLICIT NONE
    LOGICAL, INTENT(IN)  :: cleanup
    INTEGER, INTENT(INOUT) :: total_error

    INTEGER(HID_T) :: fapl_id
    INTEGER(HID_T) :: file_id
    CHARACTER(len=80) :: filename = "h5g_tests.h5"
    INTEGER :: hdferror
    INTEGER(HID_T)  :: es_id
    INTEGER(SIZE_T) :: num_in_progress
    LOGICAL         :: err_occurred

    CHARACTER(LEN=6), PARAMETER:: grpname="group1"

    INTEGER(HID_T) :: group_id, group_id1
    INTEGER(HID_T) :: gcpl_id
    CHARACTER(LEN=2) :: chr2
    CHARACTER(LEN=7) :: objname !  Object name
    INTEGER :: v, i

    TYPE(H5G_info_t), DIMENSION(1:3) :: ginfo

    INTEGER :: error
    INTEGER(KIND=MPI_INTEGER_KIND) :: mpierror       ! MPI error flag
#ifdef H5_HAVE_MPI_F08
    TYPE(MPI_COMM) :: comm
    TYPE(MPI_INFO) :: info
#else
    INTEGER(KIND=MPI_INTEGER_KIND) :: comm, info
#endif
    INTEGER(KIND=MPI_INTEGER_KIND) :: mpi_size, mpi_rank

    comm = MPI_COMM_WORLD
    info = MPI_INFO_NULL

    CALL MPI_COMM_SIZE(comm, mpi_size, mpierror)
    CALL MPI_COMM_RANK(comm, mpi_rank, mpierror)

    CALL H5EScreate_f(es_id, hdferror)
    CALL check("H5EScreate_f", hdferror, total_error)
    !
    ! Create the file.
    !
    CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, hdferror)
    CALL check("h5pcreate_f", hdferror, total_error)

    CALL h5pset_fapl_mpio_f(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL, hdferror)
    CALL check("h5pset_fapl_mpio_f", hdferror, total_error)

    CALL h5fcreate_async_f(filename, H5F_ACC_TRUNC_F, file_id, es_id, error, access_prp = fapl_id )
    CALL check("h5fcreate_f",hdferror, total_error)

    ! Test group API
    CALL H5Pcreate_f(H5P_GROUP_CREATE_F, gcpl_id, hdferror )
    CALL check("H5Pcreate_f", hdferror, total_error)

    CALL H5Pset_link_creation_order_f(gcpl_id, IOR(H5P_CRT_ORDER_TRACKED_F, H5P_CRT_ORDER_INDEXED_F), hdferror)
    CALL check("H5Pset_link_creation_order_f", hdferror, total_error)

    CALL H5Gcreate_async_f (file_id, grpname, group_id, es_id, hdferror, gcpl_id=gcpl_id)
    CALL check("H5Gcreate_async_f", hdferror, total_error)

    !  Create objects in new group created
    DO v = 0, 2
       !  Make name for link
       WRITE(chr2,'(I2.2)') v
       objname = 'fill '//chr2

       !  Create hard link, with group object
       CALL H5Gcreate_async_f(group_id, objname, group_id1, es_id, hdferror, gcpl_id=gcpl_id)
       CALL check("H5Gcreate_async_f", hdferror, total_error)

       !  Close group created
       CALL H5Gclose_async_f(group_id1, es_id, hdferror)
       CALL check("H5Gclose_async_f", hdferror, total_error)
    ENDDO

    CALL H5Pclose_f(gcpl_id, hdferror)
    CALL check("H5Pclose_f", hdferror, total_error)

    CALL H5Gclose_async_f(group_id, es_id, hdferror)
    CALL check("H5Gclose_async_f", hdferror, total_error)

    !
    ! Close the file.
    !
    CALL h5fclose_async_f(file_id, es_id, hdferror)
    CALL check("h5fclose_async_f",hdferror,total_error)

    ! Complete the operations
    CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror);
    CALL check("H5ESwait_f", hdferror, total_error)
    CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)
    CALL VERIFY("H5ESwait_f", num_in_progress, 0_size_t , total_error)

    CALL h5fopen_async_f(filename, H5F_ACC_RDWR_F, file_id, es_id, hdferror, access_prp = fapl_id )
    CALL check("h5fopen_async_f",hdferror,total_error)

    CALL h5gopen_async_f(file_id, grpname, group_id, es_id, hdferror)
    CALL check("h5gopen_async_f",hdferror,total_error)

    CALL h5gget_info_async_f(group_id, ginfo(1), es_id, hdferror)
    CALL check("H5Gget_info_async_f", hdferror, total_error)

    CALL H5Gget_info_by_name_async_f(group_id, ".", ginfo(2), es_id, hdferror)
    CALL check("H5Gget_info_by_name_async_f", hdferror, total_error)

    CALL H5Gget_info_by_idx_async_f(group_id, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_INC_F, &
         INT(0,HSIZE_T), ginfo(3), es_id, error)
    CALL check("H5Gget_info_by_idx_async_f", hdferror, total_error)

    CALL H5Gclose_async_f(group_id, es_id, hdferror)
    CALL check("H5Gclose_async_f", hdferror, total_error)

    CALL h5fclose_async_f(file_id, es_id, hdferror)
    CALL check("h5fclose_async_f",hdferror,total_error)

    CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror)
    CALL check("H5ESwait_f", hdferror, total_error)
    CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)

    ! Verify the group APIs
    DO i = 1, 2
       CALL VERIFY("H5Gget_info_by_name_f.storage_type", &
            ginfo(i)%storage_type, INT(H5G_STORAGE_TYPE_COMPACT_F, C_INT), total_error)
       CALL VERIFY("H5Gget_info_by_name_f.max_corder", ginfo(i)%max_corder, 3_C_INT64_T, total_error)
       CALL VERIFY("H5Gget_info_by_name_f.nlinks", ginfo(i)%nlinks, 3_HSIZE_T, total_error)
       CALL VERIFY("H5Gget_info_f.mounted", LOGICAL(ginfo(i)%mounted),.FALSE.,total_error)
    ENDDO
    CALL VERIFY("H5Gget_info_by_name_f.storage_type", &
         ginfo(3)%storage_type, INT(H5G_STORAGE_TYPE_COMPACT_F, C_INT), total_error)
    CALL VERIFY("H5Gget_info_by_name_f.max_corder", ginfo(3)%max_corder, 0_C_INT64_T, total_error)
    CALL VERIFY("H5Gget_info_by_name_f.nlinks", ginfo(3)%nlinks, 0_HSIZE_T, total_error)
    CALL VERIFY("H5Gget_info_f.mounted", LOGICAL(ginfo(3)%mounted),.FALSE.,total_error)

  END SUBROUTINE H5G_async_tests

  SUBROUTINE H5F_async_tests(cleanup, total_error)
    !
    ! Test H5F async routines
    !
    IMPLICIT NONE
    LOGICAL, INTENT(IN)  :: cleanup
    INTEGER, INTENT(INOUT) :: total_error

    INTEGER(HID_T) :: fapl_id
    INTEGER(HID_T) :: file_id
    CHARACTER(len=80) :: filename = "h5f_tests.h5"
    INTEGER :: hdferror
    INTEGER(HID_T)  :: es_id
    INTEGER(SIZE_T) :: num_in_progress
    LOGICAL         :: err_occurred

    INTEGER(HID_T) :: ret_file_id

    INTEGER :: error  ! Error flags
    INTEGER(KIND=MPI_INTEGER_KIND) :: mpierror       ! MPI error flag
#ifdef H5_HAVE_MPI_F08
    TYPE(MPI_COMM) :: comm
    TYPE(MPI_INFO) :: info
#else
    INTEGER(KIND=MPI_INTEGER_KIND) :: comm, info
#endif
    INTEGER(KIND=MPI_INTEGER_KIND) :: mpi_size, mpi_rank

    comm = MPI_COMM_WORLD
    info = MPI_INFO_NULL

    CALL MPI_COMM_SIZE(comm, mpi_size, mpierror)
    CALL MPI_COMM_RANK(comm, mpi_rank, mpierror)

    CALL H5EScreate_f(es_id, hdferror)
    CALL check("H5EScreate_f", hdferror, total_error)
    !
    ! Create the file.
    !
    CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, hdferror)
    CALL check("h5pcreate_f", hdferror, total_error)

    CALL h5pset_fapl_mpio_f(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL, hdferror)
    CALL check("h5pset_fapl_mpio_f", hdferror, total_error)

    CALL h5fcreate_async_f(filename, H5F_ACC_TRUNC_F, file_id, es_id, error, access_prp = fapl_id )
    CALL check("h5fcreate_f",hdferror, total_error)

    !
    ! Close the file.
    !
    CALL h5fclose_async_f(file_id, es_id, hdferror)
    CALL check("h5fclose_async_f",hdferror,total_error)

    ! Complete the operations
    CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror);
    CALL check("H5ESwait_f", hdferror, total_error)
    CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)
    CALL VERIFY("H5ESwait_f", num_in_progress, 0_size_t , total_error)

    CALL H5Fopen_async_f(filename, H5F_ACC_RDWR_F, file_id, es_id, hdferror, access_prp = fapl_id )
    CALL check("h5fopen_async_f",hdferror,total_error)

    CALL H5Freopen_async_f(file_id, ret_file_id, es_id, hdferror)
    CALL check("H5Freopen_async_f", hdferror, total_error)

    CALL H5Fclose_async_f(ret_file_id, es_id, hdferror)
    CALL check("h5fclose_async_f",hdferror,total_error)

    CALL H5Fflush_async_f(file_id, H5F_SCOPE_GLOBAL_F, es_id, hdferror)
    CALL check("h5fflush_async_f",hdferror, total_error)

    CALL H5Fclose_async_f(file_id, es_id, hdferror)
    CALL check("h5fclose_async_f",hdferror,total_error)

    CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror)
    CALL check("H5ESwait_f", hdferror, total_error)
    CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)

  END SUBROUTINE H5F_async_tests

  SUBROUTINE H5L_async_tests(cleanup, total_error)
    !
    ! Test H5L async routines
    !
    IMPLICIT NONE
    LOGICAL, INTENT(IN)  :: cleanup
    INTEGER, INTENT(INOUT) :: total_error

    INTEGER(HID_T) :: fapl_id
    INTEGER(HID_T) :: file_id
    CHARACTER(len=80) :: filename = "h5l_tests.h5"
    INTEGER :: hdferror
    INTEGER(HID_T)  :: es_id
    INTEGER(SIZE_T) :: num_in_progress
    LOGICAL         :: err_occurred

    INTEGER(hid_t)  :: gid = -1, gid2 = -1, gid3 = -1  ! Group IDs
    INTEGER(hid_t)  :: aid = -1, aid2 = -1, aid3 = -1 ! Attribute ID
    INTEGER(hid_t)  :: sid = -1  ! Dataspace ID
    CHARACTER(LEN=12), PARAMETER :: CORDER_GROUP_NAME  = "corder_group"
    CHARACTER(LEN=12), PARAMETER :: CORDER_GROUP_NAME2 = "corder_grp00"
    LOGICAL(C_BOOL), TARGET :: exists1, exists2
    LOGICAL :: exists
    TYPE(C_PTR) :: f_ptr

    INTEGER(HID_T) :: group_id !  Group ID
    INTEGER(HID_T) :: gcpl_id  !  Group creation property list ID

    INTEGER :: idx_type        !  Type of index to operate on
    LOGICAL, DIMENSION(1:2) :: use_index = (/.FALSE.,.TRUE./)
    !  Use index on creation order values
    INTEGER :: max_compact     !  Maximum # of links to store in group compactly
    INTEGER :: min_dense       !  Minimum # of links to store in group "densely"

    CHARACTER(LEN=7) :: objname   !  Object name

    INTEGER :: u !  Local index variable
    INTEGER :: Input1, i
    INTEGER(HID_T) :: group_id2
    INTEGER :: iorder !  Order within in the index
    CHARACTER(LEN=2) :: chr2
    !
    INTEGER(hsize_t) idx               ! Index in the group
    TYPE(iter_info), TARGET :: info
    TYPE(C_FUNPTR) :: f1
    TYPE(C_PTR) :: f2
    INTEGER :: ret_value

    INTEGER :: error  ! Error flags
    INTEGER(KIND=MPI_INTEGER_KIND) :: mpierror       ! MPI error flag
#ifdef H5_HAVE_MPI_F08
    TYPE(MPI_COMM) :: comm
#else
    INTEGER(KIND=MPI_INTEGER_KIND) :: comm
#endif
    INTEGER(KIND=MPI_INTEGER_KIND) :: mpi_size, mpi_rank

    INTEGER(SIZE_T) :: count

    comm = MPI_COMM_WORLD

    CALL MPI_COMM_SIZE(comm, mpi_size, mpierror)
    CALL MPI_COMM_RANK(comm, mpi_rank, mpierror)

    CALL H5EScreate_f(es_id, hdferror)
    CALL check("H5EScreate_f", hdferror, total_error)
    !
    ! Create the file.
    !
    CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, hdferror)
    CALL check("h5pcreate_f", hdferror, total_error)

    CALL h5pset_fapl_mpio_f(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL, hdferror)
    CALL check("h5pset_fapl_mpio_f", hdferror, total_error)

    CALL h5fcreate_async_f(filename, H5F_ACC_TRUNC_F, file_id, es_id, error, access_prp = fapl_id )
    CALL check("h5fcreate_f",hdferror, total_error)

    CALL H5Pcreate_f(H5P_GROUP_CREATE_F, gcpl_id, hdferror )
    CALL check("H5Pcreate_f", hdferror, total_error)

    CALL H5Pset_link_creation_order_f(gcpl_id, IOR(H5P_CRT_ORDER_TRACKED_F, H5P_CRT_ORDER_INDEXED_F), hdferror)
    CALL check("H5Pset_link_creation_order_f", hdferror, total_error)

    !  Create group with creation order tracking on
    CALL H5Gcreate_async_f(file_id, CORDER_GROUP_NAME, gid3, es_id, hdferror, gcpl_id=gcpl_id)
    CALL check("H5Gcreate_f", hdferror, total_error)

    ! Create group
    CALL H5Gcreate_async_f(file_id, "/Group1", gid, es_id, hdferror)
    CALL check("H5Gcreate_async_f",hdferror, total_error)

    ! Create nested group
    CALL H5Gcreate_async_f(gid, "Group2", gid2, es_id, hdferror)
    CALL check("H5Gcreate_async_f",hdferror, total_error)

    CALL H5Screate_f(H5S_SCALAR_F, sid, hdferror)
    CALL check("H5Screate_f",hdferror, total_error)
    CALL H5Acreate_async_f(gid2, "Attr1", H5T_NATIVE_INTEGER, sid, aid, es_id, hdferror)
    CALL check("H5Acreate_async_f",hdferror, total_error)
    CALL H5Acreate_async_f(gid2, "Attr2", H5T_NATIVE_INTEGER, sid, aid2, es_id, hdferror)
    CALL check("H5Acreate_async_f",hdferror, total_error)
    CALL H5Acreate_async_f(gid2, "Attr3", H5T_NATIVE_INTEGER, sid, aid3, es_id, hdferror)
    CALL check("H5Acreate_async_f",hdferror, total_error)
    CALL H5Aclose_async_f(aid, es_id, hdferror)
    CALL check("H5Aclose_async_f",hdferror, total_error)
    CALL H5Aclose_async_f(aid2, es_id, hdferror)
    CALL check("H5Aclose_async_f",hdferror, total_error)
    CALL H5Aclose_async_f(aid3, es_id, hdferror)
    CALL check("H5Aclose_async_f",hdferror, total_error)
    CALL H5Sclose_f(sid,hdferror)
    CALL check("H5Sclose_f",hdferror, total_error)

    ! Close groups
    CALL h5gclose_async_f(gid2, es_id, hdferror)
    CALL check("h5gclose_async_f",hdferror, total_error)
    CALL h5gclose_async_f(gid, es_id, hdferror)
    CALL check("h5gclose_async_f",hdferror, total_error)
    CALL h5gclose_async_f(gid3, es_id, hdferror)
    CALL check("h5gclose_async_f",hdferror, total_error)

    ! Close the group creation property list
    CALL H5Pclose_f(gcpl_id, hdferror)
    CALL check("H5Pclose_f", hdferror, total_error)

    ! Create soft links to groups created
    CALL H5Lcreate_soft_async_f("/Group1", file_id, "/soft_one", es_id, hdferror)
    CALL H5Lcreate_soft_async_f("/Group1/Group2", file_id, "/soft_two", es_id, hdferror)

    ! Create hard links to all groups
    CALL H5Lcreate_hard_async_f(file_id, "/", file_id, "hard_zero", es_id, hdferror)
    CALL check("H5Lcreate_hard_async_f",hdferror, total_error)
    CALL H5Lcreate_hard_async_f(file_id, "/Group1", file_id, "hard_one", es_id, hdferror)
    CALL check("H5Lcreate_hard_async_f",hdferror, total_error)
    CALL H5Lcreate_hard_async_f(file_id, "/Group1/Group2", file_id, "hard_two", es_id, hdferror)
    CALL check("H5Lcreate_hard_async_f",hdferror, total_error)

    !
    ! Close the file.
    !
    CALL h5fclose_async_f(file_id, es_id, hdferror)
    CALL check("h5fclose_async_f",hdferror,total_error)

    ! Complete the operations
    CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror);
    CALL check("H5ESwait_f", hdferror, total_error)
    CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)
    CALL VERIFY("H5ESwait_f", num_in_progress, 0_size_t , total_error)

    CALL H5Fopen_async_f(filename, H5F_ACC_RDWR_F, file_id, es_id, hdferror, access_prp = fapl_id )
    CALL check("h5fopen_async_f",hdferror,total_error)

    exists1 = logical_false
    f_ptr = C_LOC(exists1)
    CALL H5Lexists_async_f(file_id, "hard_zero", f_ptr, es_id, hdferror)
    CALL check("H5Lexists_async_f",hdferror,total_error)

    exists2 = logical_false
    f_ptr = C_LOC(exists2)
    CALL H5Lexists_async_f(file_id, "hard_two", f_ptr, es_id, hdferror)
    CALL check("H5Lexists_async_f",hdferror,total_error)

    CALL H5Ldelete_async_f(file_id, "hard_two", es_id, hdferror)
    CALL check("H5Ldelete_async_f",hdferror,total_error)

    CALL H5Fclose_async_f(file_id, es_id, hdferror)
    CALL check("h5fclose_async_f",hdferror,total_error)

    CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror)
    CALL check("H5ESwait_f", hdferror, total_error)
    CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)

    CALL VERIFY("H5Lexists_async_f", exists1, logical_true, total_error)
    CALL VERIFY("H5Lexists_async_f", exists2, logical_true, total_error)

    CALL h5fopen_f(filename, H5F_ACC_RDWR_F, file_id, hdferror, access_prp = fapl_id )
    CALL check("h5fopen_f",hdferror, total_error)

    ! Verify the link was deleted
    CALL H5Lexists_f(file_id, "hard_two", exists, hdferror)
    CALL check("H5Lexist_f",hdferror, total_error)
    CALL VERIFY("H5Ldelete_async_f", exists, .FALSE., total_error)

    CALL H5Fclose_f(file_id, hdferror)
    CALL check("H5Fclose_f", hdferror,total_error)

    !  Loop over operating on different indices on link fields
    DO idx_type = H5_INDEX_NAME_F, H5_INDEX_CRT_ORDER_F
       !  Loop over operating in different orders
       DO iorder = H5_ITER_INC_F,  H5_ITER_DEC_F
          !  Loop over using index for creation order value
          DO i = 1, 2
             !  Create file
             CALL H5Fcreate_async_f(filename, H5F_ACC_TRUNC_F, file_id, es_id, hdferror, access_prp=fapl_id)
             CALL check("H5Fcreate_async_f", hdferror, total_error)

             !  Create group creation property list
             CALL H5Pcreate_f(H5P_GROUP_CREATE_F, gcpl_id, hdferror )
             CALL check("H5Pcreate_f", hdferror, total_error)

             !  Set creation order tracking & indexing on group
             IF(use_index(i))THEN
                Input1 = H5P_CRT_ORDER_INDEXED_F
             ELSE
                Input1 = 0
             ENDIF

             CALL H5Pset_link_creation_order_f(gcpl_id, IOR(H5P_CRT_ORDER_TRACKED_F, Input1), hdferror)
             CALL check("H5Pset_link_creation_order_f", hdferror, total_error)

             !  Create group with creation order tracking on
             CALL H5Gcreate_async_f(file_id, CORDER_GROUP_NAME2, group_id, es_id, hdferror, gcpl_id=gcpl_id)
             CALL check("H5Gcreate_async_f", hdferror, total_error)

             !  Query the group creation properties
             CALL H5Pget_link_phase_change_f(gcpl_id, max_compact, min_dense, hdferror)
             CALL check("H5Pget_link_phase_change_f", hdferror, total_error)

             !  Create several links, up to limit of compact form
             DO u = 0, max_compact-1
                !  Make name for link
                WRITE(chr2,'(I2.2)') u
                objname = 'fill '//chr2

                !  Create hard link, with group object
                CALL H5Gcreate_async_f(group_id, objname, group_id2, es_id, hdferror)
                CALL check("H5Gcreate_async_f", hdferror, total_error)
                CALL H5Gclose_async_f(group_id2, es_id, hdferror)
                CALL check("H5Gclose_async_f", hdferror, total_error)
             ENDDO

             !  Delete links from compact group
             DO u = 0, (max_compact - 1) -1
                !  Delete first link in appropriate order
                CALL H5Ldelete_by_idx_async_f(group_id, ".", idx_type, iorder, INT(0,HSIZE_T), es_id, hdferror)
                CALL check("H5Ldelete_by_idx_async_f", hdferror, total_error)
             ENDDO

             idx = 0
             info%command = 2
             f1 = C_FUNLOC(liter_cb)
             f2 = C_LOC(info)

             CALL H5Literate_async_f(file_id, H5_INDEX_NAME_F, H5_ITER_INC_F, idx, f1, f2, ret_value, es_id, hdferror)
             CALL check("H5Literate_async_f", error, total_error)

             !  Close the group
             CALL H5Gclose_async_f(group_id, es_id, hdferror)
             CALL check("H5Gclose_async_f", hdferror, total_error)
             ! Close the group creation property list
             CALL H5Pclose_f(gcpl_id, hdferror)
             CALL check("H5Pclose_f", hdferror, total_error)
             ! Close the file
             CALL H5Fclose_async_f(file_id, es_id, hdferror)
             CALL check("H5Fclose_async_f", hdferror, total_error)

             CALL H5ESget_count_f(es_id, count, hdferror)

             ! Complete the operations
             CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror);
             CALL check("H5ESwait_f", hdferror, total_error)
             CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)
             CALL VERIFY("H5ESwait_f", num_in_progress, 0_size_t , total_error)

             ! NOTE: ret_value will not be correct since H5Literate_async is not returning a pointer return value, herr_t.
             CALL VERIFY("H5Literate_async_f", info%type, op_data_type, total_error)
             CALL VERIFY("H5Literate_async_f", info%command, op_data_command, total_error)
             CALL VERIFY("H5Literate_async_f", info%name(1)(1:1), CORDER_GROUP_NAME2(1:1), total_error)

          ENDDO
       ENDDO

    ENDDO

    CALL H5Pclose_f(fapl_id, hdferror)
    CALL check(" H5Pclose_f",hdferror, total_error)

    CALL H5ESclose_f(es_id, hdferror)
    CALL check("H5ESclose_f", hdferror, total_error)


  END SUBROUTINE H5L_async_tests

  SUBROUTINE H5O_async_tests(cleanup, total_error)
    !
    ! Test H5O async routines
    !
    IMPLICIT NONE
    LOGICAL, INTENT(IN)  :: cleanup
    INTEGER, INTENT(INOUT) :: total_error
    INTEGER(HID_T) :: file_id
    INTEGER(HID_T) :: group_id, group_id1, group_id2, group_id3
    INTEGER(HID_T) :: space_id
    INTEGER(HID_T) :: attr_id
    INTEGER(HID_T) :: dset_id
    INTEGER(HID_T) :: fapl_id
    INTEGER(HID_T) :: lcpl_id
    INTEGER(HID_T) :: ocpypl_id
    TYPE(C_H5O_INFO_T), TARGET :: oinfo_f
    TYPE(C_PTR) :: f_ptr
    CHARACTER(len=80) :: filename = "h5o_tests.h5"

    INTEGER ::  hdferror  !  Value returned from API calls

    ! Data for tested h5ocopy_async_f
    CHARACTER(LEN=3) , PARAMETER :: dataset = "DS1"
    INTEGER          , PARAMETER :: dim0     = 4

    INTEGER(HSIZE_T), DIMENSION(1:1)    :: dims2 = (/dim0/) ! size read/write buffer
    INTEGER(C_INT), DIMENSION(1:8) :: atime, btime, ctime, mtime

    INTEGER(HID_T)  :: es_id
    INTEGER(SIZE_T) :: num_in_progress
    LOGICAL         :: err_occurred

    ! Make a FAPL that uses the "use the latest version of the format" bounds
    CALL H5Pcreate_f(H5P_FILE_ACCESS_F,fapl_id,hdferror)
    CALL check("h5Pcreate_f",hdferror,total_error)

    !  Set the "use the latest version of the format" bounds for creating objects in the file
    CALL H5Pset_libver_bounds_f(fapl_id, H5F_LIBVER_LATEST_F, H5F_LIBVER_LATEST_F, hdferror)
    CALL check("H5Pset_libver_bounds_f",hdferror, total_error)

    CALL h5pset_fapl_mpio_f(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL, hdferror)

    CALL H5EScreate_f(es_id, hdferror)
    CALL check("H5EScreate_f", hdferror, total_error)

    !  Create a new HDF5 file
    CALL H5Fcreate_async_f(filename, H5F_ACC_TRUNC_F, file_id, es_id, hdferror, H5P_DEFAULT_F, fapl_id)
    CALL check("H5Fcreate_f", hdferror, total_error)

    !  Close the FAPL
    CALL h5pclose_f(fapl_id, hdferror)
    CALL check("h5pclose_f",hdferror,total_error)

    !
    ! Create dataspace.  Setting size to be the current size.
    !
    CALL h5screate_simple_f(1, dims2, space_id, hdferror)
    CALL check("h5screate_simple_f", hdferror, total_error)
    !
    ! Create intermediate groups
    !
    CALL h5gcreate_async_f(file_id,"/G1",group_id1,es_id,hdferror)
    CALL check("h5gcreate_f", hdferror, total_error)
    CALL h5gcreate_async_f(file_id,"/G1/G2",group_id2,es_id,hdferror)
    CALL check("h5gcreate_f", hdferror, total_error)
    CALL h5gcreate_async_f(file_id,"/G1/G2/G3",group_id3,es_id,hdferror)
    CALL check("h5gcreate_f", hdferror, total_error)

    !
    ! Create the dataset
    !
    CALL h5dcreate_async_f(group_id3, dataset, H5T_STD_I32LE, space_id, dset_id, es_id, hdferror)
    CALL check("h5dcreate_f", hdferror, total_error)

    ! Create a soft link to /G1
    CALL h5lcreate_soft_async_f("/G1", file_id, "/G1_LINK", es_id, hdferror)
    CALL check("h5lcreate_soft_f", hdferror, total_error)

    ! Create a soft link to /G1000, does not exist
    CALL h5lcreate_soft_async_f("/G1000", file_id, "/G1_FALSE", es_id, hdferror)
    CALL check("h5lcreate_soft_f", hdferror, total_error)

    ! Create a soft link to /G1_LINK
    CALL h5lcreate_soft_async_f("/G1_FALSE", file_id, "/G2_FALSE", es_id, hdferror)
    CALL check("h5lcreate_soft_f", hdferror, total_error)
    !
    ! Close and release resources.
    !
    CALL h5dclose_async_f(dset_id, es_id, hdferror)
    CALL check(" h5dclose_f", hdferror, total_error)
    CALL h5sclose_f(space_id, hdferror)
    CALL check("h5sclose_f", hdferror, total_error)
    CALL h5gclose_async_f(group_id1, es_id, hdferror)
    CALL check("h5gclose_async_f", hdferror, total_error)
    CALL h5gclose_async_f(group_id2, es_id, hdferror)
    CALL check("h5gclose_async_f", hdferror, total_error)
    CALL h5gclose_async_f(group_id3, es_id, hdferror)
    CALL check("h5gclose_async_f", hdferror, total_error)

    ! Test opening an object by index
    CALL h5oopen_by_idx_async_f(file_id, "/G1/G2/G3", H5_INDEX_NAME_F, H5_ITER_INC_F, 0_hsize_t, group_id, es_id, hdferror)
    CALL check("h5oopen_by_idx_f", hdferror, total_error)

    CALL h5oclose_async_f(group_id, es_id, hdferror)
    CALL check("h5gclose_f", hdferror, total_error)

    ! Test opening an object
    CALL h5oopen_async_f(file_id, "/G1/G2/G3", group_id, es_id, hdferror)
    CALL check("h5oopen_by_idx_f", hdferror, total_error)

    CALL H5Screate_f(H5S_SCALAR_F, space_id, hdferror)
    CALL check("H5Screate_f", hdferror, total_error)

    CALL h5acreate_async_f(group_id, "ATTR", H5T_NATIVE_INTEGER, space_id, attr_id, es_id, hdferror)
    CALL check("h5acreate_f",hdferror,total_error)

    CALL H5Aclose_async_f(attr_id, es_id, hdferror)
    CALL check("H5Aclose_async_f",hdferror,total_error)

    CALL h5oclose_async_f(group_id, es_id, hdferror)
    CALL check("h5gclose_f", hdferror, total_error)

    f_ptr = C_LOC(oinfo_f)
    CALL H5Oget_info_by_name_async_f(file_id, "/G1/G2/G3", f_ptr, es_id, hdferror, fields=H5O_INFO_ALL_F)
    CALL check("H5Oget_info_by_name_async_f", hdferror, total_error)
    !
    ! create property to pass copy options
    !
    CALL h5pcreate_f(H5P_LINK_CREATE_F, lcpl_id, hdferror)
    CALL check("h5Pcreate_f", hdferror, total_error)

    CALL h5pset_create_inter_group_f(lcpl_id, 1, hdferror)
    CALL check("H5Pset_create_inter_group_f", hdferror, total_error)
    !
    ! Check optional parameter lcpl_id, this would fail if lcpl_id was not specified
    !
    CALL h5ocopy_async_f(file_id, "/G1/G2/G3/DS1", file_id, "/G1/G_cp1/DS2", es_id, hdferror, lcpl_id=lcpl_id)
    CALL check("h5ocopy_f -- W/ OPTION: lcpl_id", hdferror ,total_error)

    CALL h5pclose_f(lcpl_id, hdferror)
    CALL check("h5pclose_f",hdferror,total_error)

    CALL h5pcreate_f(H5P_OBJECT_COPY_F, ocpypl_id, hdferror)
    CALL check("h5Pcreate_f",hdferror,total_error)

    CALL h5pset_copy_object_f(ocpypl_id, H5O_COPY_SHALLOW_HIERARCHY_F, hdferror)
    CALL check("H5Pset_copy_object_f",hdferror,total_error)

    CALL h5ocopy_async_f(file_id, "/G1/G2", file_id, "/G1/G_cp2", es_id, hdferror, ocpypl_id=ocpypl_id)
    CALL check("h5ocopy_f",hdferror,total_error)

    CALL h5pclose_f(ocpypl_id, hdferror)
    CALL check("h5pclose_f",hdferror,total_error)

    CALL h5fclose_async_f(file_id, es_id, hdferror)
    CALL check("h5fclose_f",hdferror,total_error)

    CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror)
    CALL check("H5ESwait_f", hdferror, total_error)
    CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)

    IF( oinfo_f%fileno.LE.0 )THEN
       hdferror = -1
       CALL check("H5Oget_info_by_name_async_f", hdferror, total_error)
    ENDIF

    atime(1:8) = INT(h5gmtime(oinfo_f%atime),C_INT)
    btime(1:8) = INT(h5gmtime(oinfo_f%btime),C_INT)
    ctime(1:8) = INT(h5gmtime(oinfo_f%ctime),C_INT)
    mtime(1:8) = INT(h5gmtime(oinfo_f%mtime),C_INT)

    IF( atime(1) .LT. 2021 .OR. &
         btime(1).LT. 2021  .OR. &
         ctime(1) .LT. 2021 .OR. &
         mtime(1) .LT. 2021 )THEN
       hdferror = -1
    ENDIF
    CALL check("H5Oget_info_by_name_async_f", hdferror, total_error)

    CALL VERIFY("H5Oget_info_by_name_async_f", oinfo_f%num_attrs, 1_HSIZE_T, total_error)
    CALL VERIFY("H5Oget_info_by_name_async_f", oinfo_f%type, INT(H5G_GROUP_F, C_INT), total_error)

    CALL H5ESclose_f(es_id, hdferror)
    CALL check("H5ESclose_f", hdferror, total_error)

  END SUBROUTINE H5O_async_tests

END MODULE test_async_APIs

!
! The main program for async HDF5 Fortran tests
!
PROGRAM async_test
  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT64_T
  USE test_async_APIs

  IMPLICIT NONE

  INTEGER :: total_error = 0        ! sum of the number of errors
  INTEGER(KIND=MPI_INTEGER_KIND) :: mpierror               ! MPI hdferror flag
  INTEGER(KIND=MPI_INTEGER_KIND) :: mpi_size               ! number of processes in the group of communicator
  INTEGER(KIND=MPI_INTEGER_KIND) :: mpi_rank               ! rank of the calling process in the communicator
  INTEGER(KIND=MPI_INTEGER_KIND) :: required, provided
#ifdef H5_HAVE_MPI_F08
  TYPE(MPI_DATATYPE) :: mpi_int_type
#else
  INTEGER(KIND=MPI_INTEGER_KIND) :: mpi_int_type
#endif

  INTEGER(HID_T) :: vol_id
  INTEGER :: hdferror
  LOGICAL :: registered
  INTEGER :: sum
  INTEGER :: nerrors = 0
  INTEGER :: len, idx

  CHARACTER(LEN=255) :: vol_connector_string, vol_connector_name
  INTEGER(C_INT64_T) :: cap_flags
  INTEGER(HID_T) :: plist_id
  LOGICAL          :: cleanup
  INTEGER          :: ret_total_error = 0

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
     mpi_thread_mult = .FALSE.
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
     CALL mpi_abort(MPI_COMM_WORLD, 1_MPI_INTEGER_KIND, mpierror)
  ENDIF

  IF(mpi_rank==0) CALL write_test_header("ASYNC FORTRAN TESTING")

  !
  ! Initialize the HDF5 fortran interface
  !
  CALL h5open_f(hdferror)

  ! CHECK ASYNC VOLS AVAILABILITY
  !

  IF(mpi_rank==0) WRITE(*,'(A)', ADVANCE="NO") "VOL CONNECTOR TESTED: "

  CALL get_environment_variable("HDF5_VOL_CONNECTOR", VALUE=vol_connector_string, LENGTH=len)
  IF(len .EQ. 0)THEN

     ! No VOL connector set; using native VOL connector
     async_enabled = .FALSE.
     IF(mpi_rank==0) WRITE(*,'(A,/)') "NATIVE"

  ELSE

     idx = INDEX(vol_connector_string, " ")
     vol_connector_name = vol_connector_string(1:idx-1)

     ! (1) Check if the VOL is available
     CALL H5VLis_connector_registered_by_name_f(TRIM(vol_connector_name), registered,  hdferror)
     CALL check("H5VLis_connector_registered_by_name_f", hdferror, total_error)

     IF(.NOT.registered)THEN
        ! No VOL found registered
        async_enabled = .FALSE.
        IF(mpi_rank==0) WRITE(*,'(A)') "NATIVE"
     ELSE
        ! (2) Check if the VOL is async compatible
        CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, hdferror)
        CALL check("h5pcreate_f", hdferror, total_error)
        CALL h5pget_vol_cap_flags_f(plist_id, cap_flags, hdferror)
        CALL check("h5pget_vol_cap_flags_f", hdferror, total_error)
        CALL h5pclose_f(plist_id, hdferror)
        CALL check("h5pcreate_f", hdferror, total_error)
        IF(IAND(cap_flags,H5VL_CAP_FLAG_ASYNC_F).EQ.0_C_INT64_T) async_enabled = .FALSE.
        IF(async_enabled .EQV. .FALSE.)THEN
           ! No async compatible VOL found
           IF(mpi_rank==0) WRITE(*,'(A)') "NATIVE"
        ELSE
           IF(mpi_rank==0) WRITE(*,'(A)') TRIM(vol_connector_name)
           CALL H5Vlregister_connector_by_name_f(TRIM(vol_connector_name), vol_id, hdferror)
           CALL check("H5Vlregister_connector_by_name_f", hdferror, total_error)
        ENDIF
     ENDIF

     IF ( (async_enabled .EQV. .TRUE.) .AND. (mpi_thread_mult .EQV. .FALSE.) ) THEN
        total_error = -1 ! Skip test
        IF(mpi_rank==0) CALL write_test_status(total_error, &
             "No MPI_Init_thread support for MPI_THREAD_MULTIPLE", total_error)
        CALL MPI_Barrier(MPI_COMM_WORLD, mpierror)
        CALL MPI_Finalize(mpierror)
        STOP
     ENDIF
  ENDIF

  IF(mpi_rank==0) WRITE(*,'(A,L1,/)') "VOL SUPPORTS ASYNC OPERATIONS: ", async_enabled

  ! H5ES API TESTING
  ret_total_error = 0
  CALL H5ES_tests(cleanup,  ret_total_error)
  IF(mpi_rank==0) CALL write_test_status(ret_total_error, &
       'H5ES API tests', total_error)

  ! H5A ASYNC API TESTING
  ret_total_error = 0
  CALL H5A_async_tests(cleanup,  ret_total_error)
  IF(mpi_rank==0) CALL write_test_status(ret_total_error, &
       'H5A async API tests', total_error)

  ! H5D ASYNC API TESTING
  ret_total_error = 0
  CALL H5D_async_tests(cleanup,  ret_total_error)
  IF(mpi_rank==0) CALL write_test_status(ret_total_error, &
       'H5D async API tests', total_error)

  ! H5G ASYNC API TESTING
  ret_total_error = 0
  CALL H5G_async_tests(cleanup,  ret_total_error)
  IF(mpi_rank==0) CALL write_test_status(ret_total_error, &
       'H5G async API tests', total_error)

  ! H5F ASYNC API TESTING
  ret_total_error = 0
  CALL H5F_async_tests(cleanup,  ret_total_error)
  IF(mpi_rank==0) CALL write_test_status(ret_total_error, &
       'H5F async API tests', total_error)

  ! H5L ASYNC API TESTING
  ret_total_error = 0
  CALL H5L_async_tests(cleanup,  ret_total_error)
  IF(mpi_rank==0) CALL write_test_status(ret_total_error, &
       'H5L async API tests', total_error)

  ! H5O ASYNC API TESTING
  ret_total_error = 0
  CALL H5O_async_tests(cleanup,  ret_total_error)
  IF(mpi_rank==0) CALL write_test_status(ret_total_error, &
       'H5O async API tests', total_error)

  IF(async_enabled)THEN
     CALL H5VLclose_f(vol_id, hdferror)
     CALL check("H5VLclose_f", hdferror, total_error)
  ENDIF

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
  IF (sum == 0) THEN
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

END PROGRAM async_test
