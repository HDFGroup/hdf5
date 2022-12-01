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

  SUBROUTINE async_api_tests(cleanup, total_error)

    IMPLICIT NONE
     LOGICAL, INTENT(IN)  :: cleanup
     INTEGER, INTENT(INOUT) :: total_error

     CHARACTER(LEN=14), PARAMETER :: filename = "atest_async.h5"    !File name
     CHARACTER(LEN=8), PARAMETER :: dsetname = "IntArray" ! Dataset name
     CHARACTER(LEN=6), PARAMETER:: grpname="group1"

     INTEGER(HID_T) :: file_id, crp_list       ! File identifier
     INTEGER(HID_T) :: dset_id, dataspace_id       ! Dataset identifier
     INTEGER(HID_T) :: group_id, group_id1
     INTEGER(HID_T) :: gcpl_id
     CHARACTER(LEN=2) :: chr2
     CHARACTER(LEN=7) :: objname !  Object name
     INTEGER :: v

     INTEGER(HID_T) :: filespace     ! Dataspace identifier in file 
     INTEGER(HID_T) :: memspace      ! Dataspace identifier in memory
     INTEGER(HID_T) :: fapl_id, xfer_prp       ! Property list identifier
     INTEGER(HID_T) :: ret_file_id
     INTEGER(HID_T) :: es_id
     INTEGER(SIZE_T) :: num_in_progress
     LOGICAL         :: err_occurred
     TYPE(C_PTR) :: f_ptr
     INTEGER hdferror
     INTEGER nerrors

     INTEGER(HSIZE_T), DIMENSION(1) :: dims = (/4/)
     INTEGER(HSIZE_T), DIMENSION(1) :: dimsf

     INTEGER(HSIZE_T), DIMENSION(1) :: count
     INTEGER(HSSIZE_T), DIMENSION(1) :: offset
     INTEGER, ALLOCATABLE, DIMENSION(:), TARGET :: idata
     INTEGER, ALLOCATABLE, DIMENSION(:), TARGET :: rdata
     INTEGER :: rank = 1 ! Dataset rank
     INTEGER(HSIZE_T), DIMENSION(1) :: idims, imaxdims
     INTEGER(HSIZE_T), DIMENSION(1) :: maxdims
     INTEGER :: i
     INTEGER(HSIZE_T), DIMENSION(1) :: extend_dim
     INTEGER, TARGET :: fillvalue = 99
     TYPE(H5G_info_t), DIMENSION(1:3) :: ginfo
     INTEGER :: order

     INTEGER :: error, error_n  ! Error flags
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

     count(1)  = dims(1)
     offset(1) = mpi_rank * count(1)
     CALL h5screate_simple_f(1, dims(1), memspace, hdferror)
     CALL check("h5screate_simple_f", hdferror, total_error)

     CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, offset, count, hdferror)
     CALL check("h5sselect_hyperslab_f", hdferror, total_error)

     CALL h5pcreate_f(H5P_DATASET_XFER_F, xfer_prp, error)
     CALL h5pset_dxpl_mpio_f(xfer_prp, H5FD_MPIO_COLLECTIVE_F, error)

     !
     ! Write data_in to the dataset
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




     ! Complete the operations
  !   CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror)
  !   CALL check("H5ESwait_f", hdferror, total_error)
  !   CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)

   !  PRINT*, num_in_progress

   !  CALL VERIFY("H5ESwait_f", num_in_progress, 0_size_t , total_error)
     !
     ! Reopen dataset
     !
#if 0
     CALL h5dopen_async_f(file_id, dsetname, dset_id, es_id, hdferror)
     CALL check("h5dopen_async_f",hdferror,total_error)

     CALL H5Dget_space_async_f(dset_id, dataspace_id, es_id, hdferror)
     CALL check("h5dopen_async_f",hdferror,total_error)

     CALL h5sget_simple_extent_dims_f(dataspace_id, idims, imaxdims, hdferror)
     CALL check("h5sget_simple_extent_dims_f", hdferror, total_error)
     CALL VERIFY("h5sget_simple_extent_dims_f", idims(1), dimsf(1), total_error)
     CALL VERIFY("h5sget_simple_extent_dims_f", imaxdims(1), dimsf(1), total_error)

#if 0
     ALLOCATE(rdata(1:dims(1)))

     f_ptr = C_LOC(rdata)
     CALL h5dread_async_f(dset_id, H5T_NATIVE_INTEGER, f_ptr, es_id, hdferror, &
          mem_space_id = memspace, file_space_id = filespace, xfer_prp = xfer_prp)
     CALL check("h5dread_async_f", hdferror, total_error)
#endif
  !   CALL h5sclose_f(dataspace_id, hdferror)
  !   CALL check("h5sclose_f",hdferror,total_error)
#if 0
     CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror)

     DO i = 1, dims(1)
        CALL VERIFY("h5dread_f", idata(i), rdata(i), total_error)
     ENDDO
#endif
#endif

#if 0
     ! TODO: Extend the dataset, not supported with the async vol, needs to be verified with the DAOS VOL.
     extend_dim(1) = dimsf(1)*2
     CALL H5Dset_extent_async_f(dset_id, extend_dim, es_id, hdferror)
     CALL check("H5Dset_extent_async_f", error, total_error)

     CALL h5dclose_async_f(dset_id, es_id, error)
     CALL check("h5dclose_f",error,total_error)

     CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror)
     CALL check("H5ESwait_f", hdferror, total_error)
     CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)
#endif


#if 0
     !
     ! Reopen extended dataset
     !
     CALL h5dopen_async_f(file_id, dsetname, dset_id, es_id, hdferror)
     CALL check("h5dopen_async_f",hdferror,total_error)

     CALL H5Dget_space_async_f(dset_id, dataspace_id, es_id, hdferror)
     CALL check("h5dopen_async_f",hdferror,total_error)

     CALL h5sget_simple_extent_dims_f(dataspace_id, idims, imaxdims, hdferror)
     CALL check("h5sget_simple_extent_dims_f", hdferror, total_error)
     CALL VERIFY("h5sget_simple_extent_dims_f", idims(1), dimsf(1)*2, total_error)
     CALL VERIFY("h5sget_simple_extent_dims_f", imaxdims(1), dimsf(1)*2, total_error)

     CALL h5sclose_f(dataspace_id, hdferror)
     CALL check("h5sclose_f",hdferror,total_error)

     CALL h5dclose_async_f(dset_id, es_id, error)
     CALL check("h5dclose_async_f",error,total_error)
#endif

     ! Complete the operations
     !!CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror)
     !CALL check("H5ESwait_f", hdferror, total_error)
     !CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)
     !CALL VERIFY("H5ESwait_f", num_in_progress, 0_size_t , total_error)
     !
     ! Close dataspaces.
     !
     CALL h5sclose_f(filespace, hdferror)
     CALL check("h5sclose_f",hdferror,total_error)
     CALL h5sclose_f(memspace, error)
     CALL check("h5sclose_f",hdferror,total_error)
     CALL h5pclose_f(crp_list, hdferror)
     CALL check("h5pclose_f",hdferror,total_error)

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

     CALL H5Freopen_async_f(file_id, ret_file_id, es_id, hdferror)
     CALL check("H5Freopen_async_f", hdferror, total_error)

     CALL h5fclose_async_f(ret_file_id, es_id, hdferror)
     CALL check("h5fclose_async_f",hdferror,total_error)

     CALL h5fflush_async_f(file_id, H5F_SCOPE_GLOBAL_F, es_id, hdferror)
     CALL check("h5fflush_async_f",hdferror, total_error)

     CALL h5fclose_async_f(file_id, es_id, hdferror)
     CALL check("h5fclose_async_f",hdferror,total_error)

     IF(mpi_rank==0) CALL write_test_status(total_error, &
          'Testing async H5F APIs', total_error)

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

     IF(mpi_rank==0) CALL write_test_status(total_error, &
          'Testing async H5G APIs', total_error)

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

     IF(mpi_rank==0) CALL write_test_status(total_error, &
          'Testing async H5D APIs', total_error)

     CALL H5Pclose_f(fapl_id, hdferror)
     CALL check(" H5Pclose_f",hdferror, total_error)
     CALL H5Pclose_f(xfer_prp, hdferror)
     CALL check("H5Pclose_f",hdferror,total_error)

     CALL H5ESclose_f(es_id, hdferror)
     CALL check("H5ESclose_f", hdferror, total_error)

     DEALLOCATE( rdata, idata)

   END SUBROUTINE async_api_tests

#if 0
  SUBROUTINE async_attribute_test(cleanup, total_error)

    IMPLICIT NONE
     LOGICAL, INTENT(IN)  :: cleanup
     INTEGER, INTENT(INOUT) :: total_error

     CHARACTER(LEN=14), PARAMETER :: filename = "atest_async.h5"    !File name
     CHARACTER(LEN=9), PARAMETER :: dsetname = "atestdset"        !Dataset name
     CHARACTER(LEN=11), PARAMETER :: aname = "attr_string"   !String Attribute name
     CHARACTER(LEN=14), PARAMETER :: aname2 = "attr_character"!Character Attribute name
     CHARACTER(LEN=11), PARAMETER :: aname3 = "attr_double"   !DOuble Attribute name
     CHARACTER(LEN=9), PARAMETER :: aname4 = "attr_real"      !Real Attribute name
     CHARACTER(LEN=12), PARAMETER :: aname5 = "attr_integer"  !Integer Attribute name
     CHARACTER(LEN=9), PARAMETER :: aname6 = "attr_null"     !Null Attribute name

     !
     !data space rank and dimensions
     !
     INTEGER, PARAMETER :: RANK = 2
     INTEGER, PARAMETER :: NX = 4
     INTEGER, PARAMETER :: NY = 5

     INTEGER(HID_T) :: file_id       ! File identifier
     INTEGER(HID_T) :: fapl_id
     INTEGER(HID_T) :: dset_id       ! Dataset identifier
     INTEGER(HID_T) :: es_id
     INTEGER(HID_T) :: dataspace     ! Dataspace identifier for dataset

     INTEGER(HID_T) :: attr_id        !String Attribute identifier
     INTEGER(HID_T) :: attr2_id       !Character Attribute identifier
     INTEGER(HID_T) :: attr3_id       !Double Attribute identifier
     INTEGER(HID_T) :: attr4_id       !Real Attribute identifier
     INTEGER(HID_T) :: attr5_id       !Integer Attribute identifier
     INTEGER(HID_T) :: attr6_id       !Null Attribute identifier
     INTEGER(HID_T) :: aspace_id      !String Attribute Dataspace identifier
     INTEGER(HID_T) :: aspace2_id     !Character Attribute Dataspace identifier
     INTEGER(HID_T) :: aspace6_id     !Null Attribute Dataspace identifier
     INTEGER(HID_T) :: atype_id       !String Attribute Datatype identifier
     INTEGER(HID_T) :: atype2_id      !Character Attribute Datatype identifier
     INTEGER(HID_T) :: atype3_id      !Double Attribute Datatype identifier
     INTEGER(HID_T) :: atype4_id      !Real Attribute Datatype identifier
     INTEGER(HID_T) :: atype5_id      !Integer Attribute Datatype identifier
     INTEGER(HSIZE_T), DIMENSION(1) :: adims = (/2/) ! Attribute dimension
     INTEGER(HSIZE_T), DIMENSION(1) :: adims2 = (/1/) ! Attribute dimension
     INTEGER     ::   arank = 1                      ! Attribute rank
     INTEGER(SIZE_T) :: attrlen    ! Length of the attribute string

     INTEGER(HID_T) :: attr_space     !Returned String Attribute Space identifier
     INTEGER(HID_T) :: attr2_space    !Returned other Attribute Space identifier
     INTEGER(HID_T) :: attr_type      !Returned Attribute Datatype identifier
     INTEGER(HID_T) :: attr2_type      !Returned CHARACTER Attribute Datatype identifier
     INTEGER(HID_T) :: attr3_type      !Returned DOUBLE Attribute Datatype identifier
     INTEGER(HID_T) :: attr4_type      !Returned REAL Attribute Datatype identifier
     INTEGER(HID_T) :: attr5_type      !Returned INTEGER Attribute Datatype identifier
     INTEGER(HID_T) :: attr6_type      !Returned NULL Attribute Datatype identifier
     INTEGER        :: num_attrs      !number of attributes
     INTEGER(HSIZE_T) :: attr_storage   ! attributes storage requirements .MSB.
     CHARACTER(LEN=256) :: attr_name    !buffer to put attr_name
     INTEGER(SIZE_T)    ::  name_size = 80 !attribute name length

     CHARACTER(LEN=35), DIMENSION(2) ::  attr_data  ! String attribute data
     CHARACTER(LEN=35), DIMENSION(2) ::  aread_data ! Buffer to put read back
                                               ! string attr data
     CHARACTER ::  attr_character_data = 'A'
     REAL(KIND=Fortran_DOUBLE),  DIMENSION(1) ::  attr_double_data = 3.459D0
     REAL,         DIMENSION(1) ::  attr_real_data = 4.0
     INTEGER,      DIMENSION(1) ::  attr_integer_data = 5
     INTEGER(HSIZE_T), DIMENSION(7) :: data_dims


     CHARACTER :: aread_character_data ! variable to put read back Character attr data
     INTEGER, DIMENSION(1)  :: aread_integer_data ! variable to put read back integer attr data
     INTEGER, DIMENSION(1)  :: aread_null_data = 7 ! variable to put read back null attr data
     REAL(KIND=Fortran_DOUBLE), DIMENSION(1)   :: aread_double_data ! variable to put read back double attr data
     REAL, DIMENSION(1)  :: aread_real_data ! variable to put read back real attr data

     !
     !general purpose integer
     !
     INTEGER     ::   i, j

     !
     !The dimensions for the dataset.
     !
     INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/NX,NY/)

     !
     !data buffers
     !
     INTEGER, DIMENSION(:) :: data_in

     !
     !Initialize data_in buffer
     !
     DO i = 1, NX
        DO j = 1, NY
           data_in(i,j) =  (i-1) + (j-1)
        END DO
     END DO
     !
     ! Initialize attribute's data
     !
     attr_data(1) = 'Dataset character attribute'
     attr_data(2) = 'Some other string here     '
     attrlen = LEN(attr_data(1))

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
     CALL check("h5fcreate_async_f",hdferror, total_error)

     !
      !Create data space for the dataset.
     !
     CALL h5screate_simple_f(RANK, dims, dataspace, hdferror)
     CALL check("h5screate_simple_f", hdferror, total_error)

     !
     ! create contiguous dataset in the file.
     !
     CALL h5dcreate_async_f(file_id, dsetname, H5T_NATIVE_INTEGER, dataspace, &
               dset_id, es_id, hdferror)
     CALL check("h5dcreate_async_f",hdferror, total_error)
     !
     ! Write data_in to the dataset
     !
     data_dims(1) = NX
     data_dims(2) = NY
     CALL h5dwrite_async_f(dset_id, H5T_NATIVE_INTEGER, data_in, data_dims, es_id, hdferror)
     CALL check("h5dwrite_async_f", hdferror, total_error)
     !
     ! Terminate access to the dataset.
     !
     CALL h5dclose_async_f(dset_id, es_id, error)
     CALL check("h5dclose_async_f",error,total_error)

     ! Complete the operations
     CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror);
     CALL check("H5ESwait_f", hdferror, nerrors)
     CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)
     CALL VERIFY("H5ESwait_f", num_in_progress, 0_size_t , total_error)

     !
     ! Reopen dataset
     !
     CALL h5dopen_async_f(file_id, dsetname, dset_id, error)
     CALL check("h5dopen_async_f",hdferror,total_error)


     CALL h5dclose_async_f(dset_id, es_id, error)
     CALL check("h5dclose_async_f",error,total_error)

     ! Complete the operations
     CALL H5ESwait_f(es_id, H5ES_WAIT_FOREVER_F, num_in_progress, err_occurred, hdferror);
     CALL check("H5ESwait_f", hdferror, nerrors)
     CALL VERIFY("H5ESwait_f", err_occurred, .FALSE., total_error)
     CALL VERIFY("H5ESwait_f", num_in_progress, 0_size_t , total_error)

     !
     ! Create scalar data space for the string attribute.
     !
     CALL h5screate_simple_f(arank, adims, aspace_id, error)
     CALL check("h5screate_simple_f",error,total_error)
     !
     ! Create scalar data space for all other attributes.
     !
     CALL h5screate_simple_f(arank, adims2, aspace2_id, error)
     CALL check("h5screate_simple_f",error,total_error)
     !
     ! Create null data space for null attributes.
     !
     CALL h5screate_f(H5S_NULL_F, aspace6_id, error)
     CALL check("h5screate_f",error,total_error)

     !
     ! Create datatype for the String attribute.
     !
     CALL h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)
     CALL check("h5tcopy_f",error,total_error)

     CALL h5tset_size_f(atype_id, attrlen, error)
     CALL check("h5tset_size_f",error,total_error)

     !
     ! Create datatype for the Character attribute.
     !
     CALL h5tcopy_f(H5T_NATIVE_CHARACTER, atype2_id, error)
     CALL check("h5tcopy_f",error,total_error)
     !
     ! Create datatype for the Double attribute.
     !
     CALL h5tcopy_f(H5T_NATIVE_DOUBLE, atype3_id, error)
     CALL check("h5tcopy_f",error,total_error)
     !
     ! Create datatype for the Real attribute.
     !
     CALL h5tcopy_f(H5T_NATIVE_REAL, atype4_id, error)
     CALL check("h5tcopy_f",error,total_error)
     !
     ! Create datatype for the Integer attribute.
     !
     CALL h5tcopy_f(H5T_NATIVE_INTEGER, atype5_id, error)
     CALL check("h5tcopy_f",error,total_error)


     !
     ! Create dataset String attribute.
     !
     CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, &
                      attr_id, error)
     CALL check("h5acreate_f",error,total_error)


     !
     ! Create dataset CHARACTER attribute.
     !
     CALL h5acreate_f(dset_id, aname2, atype2_id, aspace2_id, &
                      attr2_id, error)
     CALL check("h5acreate_f",error,total_error)


     !
     ! Create dataset DOUBLE attribute.
     !
     CALL h5acreate_f(dset_id, aname3, atype3_id, aspace2_id, &
                      attr3_id, error)
     CALL check("h5acreate_f",error,total_error)
     !
     ! Create dataset REAL attribute.
     !
     CALL h5acreate_f(dset_id, aname4, atype4_id, aspace2_id, &
                      attr4_id, error)
     CALL check("h5acreate_f",error,total_error)
     !
     ! Create dataset INTEGER attribute.
     !
     CALL h5acreate_f(dset_id, aname5, atype5_id, aspace2_id, &
                      attr5_id, error)
     CALL check("h5acreate_f",error,total_error)
     !
     ! Create dataset NULL attribute of INTEGER.
     !

     CALL h5acreate_f(dset_id, aname6, atype5_id, aspace6_id, &
                      attr6_id, error)

     CALL check("h5acreate_f",error,total_error)

     !
     ! Write the String attribute data.
     !
     data_dims(1) = 2
     CALL h5awrite_f(attr_id, atype_id, attr_data, data_dims, error)
     CALL check("h5awrite_f",error,total_error)
      !
     ! Write the Character attribute data.
     !
     CALL h5awrite_f(attr2_id, atype2_id, attr_character_data, data_dims, error)
     CALL check("h5awrite_f",error,total_error)
     !
     ! Write the DOUBLE attribute data.
     !
     data_dims(1) = 1
     CALL h5awrite_f(attr3_id, atype3_id, attr_double_data, data_dims, error)
     CALL check("h5awrite_f",error,total_error)
     !
     ! Write the Real attribute data.
     !
     data_dims(1) = 1
     CALL h5awrite_f(attr4_id, atype4_id, attr_real_data, data_dims, error)
     CALL check("h5awrite_f",error,total_error)

     !
     ! Write the Integer attribute data.
     !
     data_dims(1) = 1
     CALL h5awrite_f(attr5_id, atype5_id, attr_integer_data, data_dims, error)
     CALL check("h5awrite_f",error,total_error)

     !
     ! Write the NULL attribute data(nothing can be written).
     !
     CALL h5awrite_f(attr6_id, atype5_id, attr_integer_data, data_dims, error)
     CALL check("h5awrite_f",error,total_error)

     !
     ! check the amount of storage that is required for the specified attribute .MSB.
     !
     CALL h5aget_storage_size_f(attr_id, attr_storage, error)
     CALL check("h5aget_storage_size_f",error,total_error)
!     CALL verify("h5aget_storage_size_f",attr_storage,*SizeOf(attr_storage),total_error)
     CALL h5aget_storage_size_f(attr2_id, attr_storage, error)
     CALL check("h5aget_storage_size_f",error,total_error)
!     CALL verify("h5aget_storage_size_f",attr_storage,1,total_error)
     CALL h5aget_storage_size_f(attr3_id, attr_storage, error)
     CALL check("h5aget_storage_size_f",error,total_error)
!     CALL verify("h5aget_storage_size_f",attr_storage,8,total_error)
     CALL h5aget_storage_size_f(attr4_id, attr_storage, error)
     CALL check("h5aget_storage_size_f",error,total_error)
!     CALL verify("h5aget_storage_size_f",attr_storage,4,total_error)
     CALL h5aget_storage_size_f(attr5_id, attr_storage, error)
     CALL check("h5aget_storage_size_f",error,total_error)
!     CALL verify("h5aget_storage_size_f",attr_storage,4,total_error)
     CALL h5aget_storage_size_f(attr6_id, attr_storage, error)
     CALL check("h5aget_storage_size_f",error,total_error)
!     CALL verify("h5aget_storage_size_f",attr_storage,0,total_error)


     !
     ! Close the attribute.
     !
     CALL h5aclose_f(attr_id, error)
     CALL check("h5aclose_f",error,total_error)
     CALL h5aclose_f(attr2_id, error)
     CALL check("h5aclose_f",error,total_error)
     CALL h5aclose_f(attr3_id, error)
     CALL check("h5aclose_f",error,total_error)
     CALL h5aclose_f(attr4_id, error)
     CALL check("h5aclose_f",error,total_error)
     CALL h5aclose_f(attr5_id, error)
     CALL check("h5aclose_f",error,total_error)
     CALL h5aclose_f(attr6_id, error)
     CALL check("h5aclose_f",error,total_error)

     CALL h5tclose_f(atype_id, error)
     CALL check("h5tclose_f",error,total_error)
     CALL h5tclose_f(atype2_id, error)
     CALL check("h5tclose_f",error,total_error)
     CALL h5tclose_f(atype3_id, error)
     CALL check("h5tclose_f",error,total_error)
     CALL h5tclose_f(atype4_id, error)
     CALL check("h5tclose_f",error,total_error)
     CALL h5tclose_f(atype5_id, error)
     CALL check("h5tclose_f",error,total_error)

     !
     ! Terminate access to the data space.
     !
     CALL h5sclose_f(aspace_id, error)
     CALL check("h5sclose_f",error,total_error)
     CALL h5sclose_f(aspace2_id, error)
     CALL check("h5sclose_f",error,total_error)
     CALL h5sclose_f(aspace6_id, error)
     CALL check("h5sclose_f",error,total_error)
     !
     ! Terminate access to the dataset.
     !
     CALL h5dclose_f(dset_id, error)
     CALL check("h5dclose_f",error,total_error)
     !
     ! Terminate access to the file.
     !
     CALL h5fclose_f(file_id, error)
     CALL check("h5fclose_f",error,total_error)
     !
     ! Open file
     !
     CALL h5fopen_f(fix_filename, H5F_ACC_RDWR_F, file_id, error)
     CALL check("h5fopen_f",error,total_error)
     !
     ! Reopen dataset
     !
     CALL h5dopen_f(file_id, dsetname, dset_id, error)
     CALL check("h5dopen_f",error,total_error)
     !
     !open the String attribute by name
     !
     CALL h5aopen_name_f(dset_id, aname, attr_id, error)
     CALL check("h5aopen_name_f",error,total_error)

     !
     !open the CHARACTER attribute by name
     !
     CALL h5aopen_name_f(dset_id, aname2, attr2_id, error)
     CALL check("h5aopen_name_f",error,total_error)
      !
     !open the DOUBLE attribute by name
     !
     CALL h5aopen_name_f(dset_id, aname3, attr3_id, error)
     CALL check("h5aopen_name_f",error,total_error)
     !
     !open the REAL attribute by name
     !
     CALL h5aopen_name_f(dset_id, aname4, attr4_id, error)
     CALL check("h5aopen_name_f",error,total_error)

     !
     !open the INTEGER attribute by name
     !
     CALL h5aopen_name_f(dset_id, aname5, attr5_id, error)
     CALL check("h5aopen_name_f",error,total_error)

     !
     !open the NULL attribute by name
     !
     CALL h5aopen_name_f(dset_id, aname6, attr6_id, error)
     CALL check("h5aopen_name_f",error,total_error)

     !
     !get the attribute name
     !
     CALL h5aget_name_f(attr5_id, name_size, attr_name, error)
     CALL check("h5aget_name_f",error,total_error)
     IF (attr_name(1:12) .NE. aname5) THEN
       total_error = total_error + 1
     END IF
     IF (error .NE. 12) THEN
       total_error = total_error + 1
     END IF

     !
     !get the STRING attribute space
     !
     CALL h5aget_space_f(attr_id, attr_space, error)
     CALL check("h5aget_space_f",error,total_error)
     !
     !get other attribute space
     !
     CALL h5aget_space_f(attr2_id, attr2_space, error)
     CALL check("h5aget_space_f",error,total_error)
     !
     !get the string attribute datatype
     !
     CALL h5aget_type_f(attr_id, attr_type, error)
     CALL check("h5aget_type_f",error,total_error)
     !
     !get the character attribute datatype
     !
     CALL h5aget_type_f(attr2_id, attr2_type, error)
     CALL check("h5aget_type_f",error,total_error)
     !
     !get the double attribute datatype
     !
     CALL h5aget_type_f(attr3_id, attr3_type, error)
     CALL check("h5aget_type_f",error,total_error)
     !
     !get the real attribute datatype
     !
     CALL h5aget_type_f(attr4_id, attr4_type, error)
     CALL check("h5aget_type_f",error,total_error)

     !
     !get the integer attribute datatype
     !
     CALL h5aget_type_f(attr5_id, attr5_type, error)
     CALL check("h5aget_type_f",error,total_error)

     !
     !get the null attribute datatype
     !
     CALL h5aget_type_f(attr6_id, attr6_type, error)
     CALL check("h5aget_type_f",error,total_error)

     !
     !get number of attributes
     !
     CALL h5aget_num_attrs_f(dset_id, num_attrs, error)
     CALL check("h5aget_num_attrs_f",error,total_error)
     IF (num_attrs .NE. 6) THEN
       WRITE(*,*) "got number of attributes wrong", num_attrs
       total_error = total_error +1
     END IF

     !
     !set the read back data type's size
     !
     CALL h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)
     CALL check("h5tcopy_f",error,total_error)

     CALL h5tset_size_f(atype_id, attrlen, error)
     CALL check("h5tset_size_f",error,total_error)
     !
     !read the string attribute data back to memory
     !
     data_dims(1) = 2
     CALL h5aread_f(attr_id, atype_id, aread_data, data_dims, error)
     CALL check("h5aread_f",error,total_error)

     IF ( (aread_data(1) .NE. attr_data(1)) .OR. (aread_data(2) .NE. attr_data(2)) ) THEN
         WRITE(*,*) "Read back string attribute is wrong", aread_data(1), aread_data(2)
         total_error = total_error + 1
     END IF

     !
     !read the CHARACTER attribute data back to memory
     !
     CALL h5aread_f(attr2_id, H5T_NATIVE_CHARACTER, aread_character_data, data_dims, error)
     CALL check("h5aread_f",error,total_error)
     IF (aread_character_data .NE. 'A' ) THEN
         WRITE(*,*) "Read back character attribute is wrong ",aread_character_data
         total_error = total_error + 1
     END IF
     !
     !read the double attribute data back to memory
     !
     data_dims(1) = 1
     CALL h5aread_f(attr3_id, H5T_NATIVE_DOUBLE, aread_double_data, data_dims, error)
     CALL check("h5aread_f",error,total_error)
     CALL VERIFY("Read back double attribute is wrong", aread_double_data(1),3.459_Fortran_DOUBLE,total_error)

     !
     !read the real attribute data back to memory
     !
     data_dims(1) = 1
     CALL h5aread_f(attr4_id, H5T_NATIVE_REAL, aread_real_data, data_dims, error)
     CALL check("h5aread_f",error,total_error)
     CALL VERIFY("Read back real attribute is wrong", aread_real_data(1),4.0,total_error)
     !
     !read the Integer attribute data back to memory
     !
     data_dims(1) = 1
     CALL h5aread_f(attr5_id, H5T_NATIVE_INTEGER, aread_integer_data, data_dims, error)
     CALL check("h5aread_f",error,total_error)
     IF (aread_integer_data(1) .NE. 5 ) THEN
         WRITE(*,*) "Read back integer attribute is wrong ", aread_integer_data
         total_error = total_error + 1
     END IF
     !
     !read the null attribute data. nothing can be read.
     !
     data_dims(1) = 1
     CALL h5aread_f(attr6_id, H5T_NATIVE_INTEGER, aread_null_data, data_dims, error)
     CALL check("h5aread_f",error,total_error)
     IF (aread_null_data(1) .NE. 7 ) THEN
         WRITE(*,*) "Read back null attribute is wrong ", aread_null_data
         total_error = total_error + 1
     END IF

     !
     ! Close the attribute.
     !
     CALL h5aclose_f(attr_id, error)
     CALL check("h5aclose_f",error,total_error)
     CALL h5aclose_f(attr2_id, error)
     CALL check("h5aclose_f",error,total_error)
     CALL h5aclose_f(attr3_id, error)
     CALL check("h5aclose_f",error,total_error)
     CALL h5aclose_f(attr4_id, error)
     CALL check("h5aclose_f",error,total_error)
     CALL h5aclose_f(attr5_id, error)
     CALL check("h5aclose_f",error,total_error)
     CALL h5aclose_f(attr6_id, error)
     CALL check("h5aclose_f",error,total_error)

     !
     ! Delete the attribute from the Dataset.
     !
     CALL h5adelete_f(dset_id, aname, error)
     CALL check("h5adelete_f",error,total_error)

     !
     !get number of attributes
     !
     CALL h5aget_num_attrs_f(dset_id, num_attrs, error)
     CALL check("h5aget_num_attrs_f",error,total_error)
     IF (num_attrs .NE. 5) THEN
       WRITE(*,*) "got number of attributes wrong", num_attrs
       total_error = total_error +1
     END IF



     CALL h5sclose_f(attr_space, error)
     CALL check("h5sclose_f",error,total_error)
     CALL h5sclose_f(attr2_space, error)
     CALL check("h5sclose_f",error,total_error)

     !
     ! Terminate access to the data type.
     !
     CALL h5tclose_f(attr_type, error)
     CALL check("h5tclose_f",error,total_error)
     CALL h5tclose_f(attr2_type, error)
     CALL check("h5tclose_f",error,total_error)
     CALL h5tclose_f(attr3_type, error)
     CALL check("h5tclose_f",error,total_error)
     CALL h5tclose_f(attr4_type, error)
     CALL check("h5tclose_f",error,total_error)
     CALL h5tclose_f(attr5_type, error)
     CALL check("h5tclose_f",error,total_error)
     CALL h5tclose_f(attr6_type, error)
     CALL check("h5tclose_f",error,total_error)

     !
     ! End access to the dataset and release resources used by it.
     !
     CALL h5dclose_f(dset_id, error)
     CALL check("h5dclose_f",error,total_error)

     !
     ! Close the file.
     !
     CALL h5fclose_f(file_id, error)
     CALL check("h5fclose_f",error,total_error)
     !
     ! Remove the file
     !
     IF (cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)

     RETURN
   END SUBROUTINE async_attribute_test
#endif

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
#if 0
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
  INTEGER(HSIZE_T) :: i
#endif
  LOGICAL          :: cleanup
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

  CALL H5ESclose_f(es_id, hdferror)
  CALL check("H5ESclose_f", hdferror, nerrors)

  ! Test ASYNC APIs
  CALL async_api_tests(cleanup, total_error)

#if 0
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
#endif

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
