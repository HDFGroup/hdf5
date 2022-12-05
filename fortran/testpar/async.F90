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

MODULE test_async_APIs

  USE MPI
  USE HDF5 ! This module contains all necessary modules
  USE TH5_MISC
  USE TH5_MISC_GEN

CONTAINS

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

    CALL H5Fcreate_async_f(filename, H5F_ACC_TRUNC_F, file_id, es_id, hdferror, access_prp = fapl_id)
    CALL check("h5fcreate_f", hdferror, nerrors)

    CALL H5ESget_count_f(es_id, count, hdferror)
    CALL check("H5ESget_count_f", hdferror, nerrors)
    CALL VERIFY("H5ESget_count_f", count, 2_SIZE_T,total_error)

    CALL H5ESget_op_counter_f(es_id, counter, hdferror)
    CALL check("H5ESget_op_counter_f", hdferror, nerrors)
    CALL VERIFY("H5ESget_op_counter_f", counter, 2_C_INT64_T, total_error) ! FIXME: I think this is wrong, it should be 3

    CALL H5Pclose_f(fapl_id, hdferror)
    CALL check("h5pclose_f", hdferror, nerrors)

    CALL H5Fclose_async_f(file_id, es_id, hdferror)
    CALL check("h5fclose_f", hdferror, nerrors)

    CALL H5ESget_count_f(es_id, count, hdferror)
    CALL check("H5ESget_count_f", hdferror, nerrors)
    CALL VERIFY("H5ESget_count_f", count, 3_SIZE_T,total_error)

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
    ! Test H5ES routines
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
     INTEGER, TARGET :: attr_data = 99
     INTEGER, TARGET :: attr_data1 = 100
     INTEGER(HID_T) :: space_id
     INTEGER(HID_T) :: attr_id, attr_id1
     LOGICAL :: exists, exists1, exists2, exists3
     TYPE(C_PTR) :: f_ptr

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

     CALL h5acreate_async_f(file_id, attr_name, H5T_NATIVE_INTEGER, space_id, attr_id, es_id, hdferror)
     CALL check("h5acreate_f",hdferror,total_error)

     f_ptr = C_LOC(attr_data)
     CALL H5Awrite_async_f(attr_id, H5T_NATIVE_INTEGER, f_ptr, es_id, hdferror)
     CALL check("H5Awrite_async_f",hdferror,total_error)

     CALL H5Aclose_async_f(attr_id, es_id, hdferror)
     CALL check("H5Aclose_async_f",hdferror,total_error)

     CALL h5acreate_by_name_async_f(file_id, "/", TRIM(attr_name)//"00", &
          H5T_NATIVE_INTEGER, space_id, attr_id1, es_id, hdferror)
     CALL check("h5acreate_by_name_async_f",hdferror,total_error)

     f_ptr = C_LOC(attr_data1)
     CALL H5Awrite_async_f(attr_id1, H5T_NATIVE_INTEGER, f_ptr, es_id, hdferror)
     CALL check("H5Awrite_async_f",hdferror,total_error)

     CALL H5Aclose_async_f(attr_id1, es_id, hdferror)
     CALL check("H5Aclose_async_f",hdferror,total_error)

     CALL H5Sclose_f(space_id, hdferror)
     CALL check("H5Sclose_f",hdferror,total_error)
     CALL H5Fclose_async_f(file_id, es_id, hdferror)
     CALL check("H5Fclose_async_f",hdferror,total_error)

     CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror)
     CALL check("H5ESwait_f", hdferror, total_error)
     CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)

     CALL h5fopen_async_f(filename, H5F_ACC_RDWR_F, file_id, es_id, hdferror, access_prp = fapl_id )
     CALL check("h5fopen_async_f",hdferror,total_error)

     CALL H5Aexists_async_f(file_id, attr_name, exists, es_id, hdferror)
     CALL check("H5Aexists_async_f",hdferror,total_error)
     !CALL H5Aexists_async_f(file_id, TRIM(attr_name)//"00", exists1, es_id, hdferror)
     !CALL check("H5Aexists_async_f",hdferror,total_error)

     CALL H5Aexists_by_name_async_f(file_id, "/", attr_name, exists2, es_id, hdferror)
     CALL check("H5Aexists_by_name_async_f",hdferror,total_error)
   !  PRINT*,exists2

     !CALL H5Aexists_by_name_async_f(file_id, "/", TRIM(attr_name)//"00", exists3, es_id, hdferror)
     !CALL check("H5Aexists_by_name_async_f",hdferror,total_error)

     CALL H5Fclose_async_f(file_id, es_id, hdferror)
     CALL check("H5Fclose_async_f",hdferror,total_error)

     CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror)
     CALL check("H5ESwait_f", hdferror, total_error)
     CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)

     !CALL VERIFY("H5Aexists_async_f", exists, .TRUE., total_error)
     !CALL VERIFY("H5Aexists_async_f", exists1, .TRUE., total_error)
     !CALL VERIFY("H5Aexists_by_name_async_f", exists2, .TRUE., total_error)

     CALL H5Pclose_f(fapl_id, hdferror)
     CALL check(" H5Pclose_f",hdferror, total_error)

     CALL H5ESclose_f(es_id, hdferror)
     CALL check("H5ESclose_f", hdferror, total_error)

  END SUBROUTINE H5A_async_tests

  SUBROUTINE H5D_async_tests(cleanup, total_error)
    !
    ! Test H5ES routines
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
    INTEGER :: mpierror       ! MPI error flag
    INTEGER :: comm, info
    INTEGER :: mpi_size, mpi_rank

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

    CALL h5fopen_async_f(filename, H5F_ACC_RDWR_F, file_id, es_id, hdferror, access_prp = fapl_id )
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
    ! Test H5ES routines
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
     INTEGER :: mpierror       ! MPI error flag
     INTEGER :: comm, info
     INTEGER :: mpi_size, mpi_rank

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
    ! Test H5ES routines
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
    INTEGER :: mpierror       ! MPI error flag
    INTEGER :: comm, info
    INTEGER :: mpi_size, mpi_rank

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
    ! Test H5ES routines
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

    INTEGER :: error  ! Error flags
    INTEGER :: mpierror       ! MPI error flag
    INTEGER :: comm, info
    INTEGER :: mpi_size, mpi_rank

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

     CALL H5Fclose_async_f(file_id, es_id, hdferror)
     CALL check("h5fclose_async_f",hdferror,total_error)

     CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror)
     CALL check("H5ESwait_f", hdferror, total_error)
     CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)

  END SUBROUTINE H5L_async_tests


  SUBROUTINE H5O_async_tests(cleanup, total_error)
    !
    ! Test H5ES routines
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

    INTEGER :: error  ! Error flags
    INTEGER :: mpierror       ! MPI error flag
    INTEGER :: comm, info
    INTEGER :: mpi_size, mpi_rank

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

    CALL H5Fclose_async_f(file_id, es_id, hdferror)
    CALL check("h5fclose_async_f",hdferror,total_error)

    CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror)
    CALL check("H5ESwait_f", hdferror, total_error)
    CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)

  END SUBROUTINE H5O_async_tests

END MODULE test_async_APIs

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
  USE test_async_APIs

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
     total_error = -1 ! skip test
     IF(mpi_rank==0) CALL write_test_status(total_error, &
          "MPI doesn't support MPI_Init_thread with MPI_THREAD_MULTIPLE", total_error)
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

  ! CHECK ASYNC VOLS AVAILABILITY
  !
  ! (1) Check if ASYNC VOL is available
  CALL H5VLis_connector_registered_by_name_f("async", registered,  hdferror)
  CALL check("H5VLis_connector_registered_by_name_f", hdferror, total_error)

  IF(.NOT.registered)THEN

     ! (2) check if the DAOS VOL is available
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
