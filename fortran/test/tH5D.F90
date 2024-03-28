!****h* root/fortran/test/tH5D.f90
!
! NAME
!  tH5D.f90
!
! FUNCTION
!  Basic testing of Fortran H5D APIs.
!
! COPYRIGHT
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
! NOTES
!  Tests the H5D APIs functionalities of:
!   h5dcreate_f, h5dopen_f, h5dclose_f, h5dget_space_f, h5dget_type_f,
!   h5dread_f, and h5dwrite_f, h5dget_space_status_f
!
!
! CONTAINS SUBROUTINES
!  datasettest, extenddsettest
!
!*****

#include <H5config_f.inc>

!
MODULE TH5D

  USE HDF5 ! This module contains all necessary modules
  USE TH5_MISC
  USE TH5_MISC_GEN
  USE ISO_C_BINDING

CONTAINS
  SUBROUTINE datasettest(cleanup, total_error)

    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: cleanup
    INTEGER, INTENT(OUT) :: total_error

    CHARACTER(LEN=5), PARAMETER :: filename = "dsetf" ! File name
    CHARACTER(LEN=80) :: fix_filename
    CHARACTER(LEN=4), PARAMETER :: dsetname = "dset"     ! Dataset name
    CHARACTER(LEN=9), PARAMETER :: null_dsetname = "null_dset"     ! Dataset name

    INTEGER(HID_T) :: file_id       ! File identifier
    INTEGER(HID_T) :: dset_id       ! Dataset identifier
    INTEGER(HID_T) :: null_dset     ! Null dataset identifier
    INTEGER(HID_T) :: dspace_id     ! Dataspace identifier
    INTEGER(HID_T) :: null_dspace   ! Null dataspace identifier
    INTEGER(HID_T) :: dtype_id      ! Datatype identifier

    INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/4,6/) ! Dataset dimensions
    INTEGER     ::   rank = 2                        ! Dataset rank

    INTEGER, DIMENSION(4,6) :: dset_data, data_out ! Data buffers
    INTEGER     ::   error ! Error flag

    INTEGER     :: i, j    !general purpose integers
    INTEGER(HSIZE_T), DIMENSION(2) :: data_dims
    INTEGER(HSIZE_T), DIMENSION(1) :: null_data_dim
    INTEGER     ::   null_dset_data = 1              ! null data
    INTEGER :: flag ! Space allocation status

    !
    ! Initialize the dset_data array.
    !
    DO i = 1, 4
       DO j = 1, 6
          dset_data(i,j) = (i-1)*6 + j;
       END DO
    END DO
    !
    ! Create a new file using default properties.
    !
    CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
    IF (error .NE. 0) THEN
       WRITE(*,*) "Cannot modify filename"
       STOP
    ENDIF
    CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
    CALL check("h5fcreate_f", error, total_error)
    !
    ! Create the dataspace.
    !
    CALL h5screate_simple_f(rank, dims, dspace_id, error)
    CALL check("h5screate_simple_f", error, total_error)
    !
    ! Create null dataspace.
    !
    CALL h5screate_f(H5S_NULL_F, null_dspace, error)
    CALL check("h5screate_simple_f", error, total_error)
    !
    ! Create the dataset with default properties.
    !
    CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, dspace_id, &
         dset_id, error)
    CALL check("h5dcreate_f", error, total_error)
    !
    ! Create the null dataset.
    !
    CALL h5dcreate_f(file_id, null_dsetname, H5T_NATIVE_INTEGER, null_dspace, null_dset, error)
    CALL check("h5dcreate_f", error, total_error)
    !
    ! Write the dataset.
    !
    data_dims(1) = 4
    data_dims(2) = 6
    CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, dset_data, data_dims, error)
    CALL check("h5dwrite_f", error, total_error)
    !
    ! Write null dataset.  Nothing can be written.
    !
    null_data_dim(1) = 1
    CALL h5dwrite_f(null_dset, H5T_NATIVE_INTEGER, null_dset_data, null_data_dim, error)
    CALL check("h5dwrite_f", error, total_error)
    !
    ! End access to the dataset and release resources used by it.
    !
    CALL h5dclose_f(dset_id, error)
    CALL check("h5dclose_f", error, total_error)
    CALL h5dclose_f(null_dset, error)
    CALL check("h5dclose_f", error, total_error)
    !
    ! Terminate access to the data space.
    !
    CALL h5sclose_f(dspace_id, error)
    CALL check("h5sclose_f", error, total_error)
    CALL h5sclose_f(null_dspace, error)
    CALL check("h5sclose_f", error, total_error)
    !
    ! Close the file.
    !
    CALL h5fclose_f(file_id, error)
    CALL check("h5fclose_f", error, total_error)
    !
    ! Open the existing file.
    !
    CALL h5fopen_f (fix_filename, H5F_ACC_RDWR_F, file_id, error)
    CALL check("h5fopen_f", error, total_error)
    !
    ! Open the existing dataset.
    !
    CALL h5dopen_f(file_id, dsetname, dset_id, error)
    CALL check("h5dopen_f", error, total_error)
    CALL h5dopen_f(file_id, null_dsetname, null_dset, error)
    CALL check("h5dopen_f", error, total_error)

    ! Test whether space has been allocated for a dataset
    CALL h5dget_space_status_f(dset_id, flag, error)
    CALL check("h5dget_space_status_f",error, total_error)
    CALL VERIFY("h5dget_space_status_f", flag, H5D_SPACE_STS_ALLOCATED_F, total_error)

    CALL h5dget_space_status_f(null_dset, flag, error)
    CALL check("h5dget_space_status_f",error, total_error)
    CALL VERIFY("h5dget_space_status_f", flag, H5D_SPACE_STS_NOT_ALLOCATED_F, total_error)
    !
    ! Get the dataset type.
    !
    CALL h5dget_type_f(dset_id, dtype_id, error)
    CALL check("h5dget_type_f", error, total_error)
    !
    ! Get the data space.
    !
    CALL h5dget_space_f(dset_id, dspace_id, error)
    CALL check("h5dget_space_f", error, total_error)
    !
    ! Read the dataset.
    !
    CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, data_out, data_dims, error)
    CALL check("h5dread_f", error, total_error)
    !
    ! Read the null dataset.  Nothing should be read.
    !
    CALL h5dread_f(null_dset, H5T_NATIVE_INTEGER, null_dset_data, null_data_dim, error)
    CALL check("h5dread_f", error, total_error)
    !
    !Compare the data.
    !
    DO i = 1, 4
       DO j = 1, 6
          IF (data_out(i,j) .NE. dset_data(i, j)) THEN
             WRITE(*, *) "dataset test error occurred"
             WRITE(*,*) "data read is not the same as the data written"
          END IF
       END DO
    END DO
    !
    ! Check if no change to null_dset_data
    !
    IF (null_dset_data .NE. 1) THEN
       WRITE(*, *) "null dataset test error occurred"
    END IF
    !
    ! End access to the dataset and release resources used by it.
    !
    CALL h5dclose_f(dset_id, error)
    CALL check("h5dclose_f", error, total_error)
    CALL h5dclose_f(null_dset, error)
    CALL check("h5dclose_f", error, total_error)
    !
    ! Terminate access to the data space.
    !
    CALL h5sclose_f(dspace_id, error)
    CALL check("h5sclose_f", error, total_error)

    !
    ! Terminate access to the data type.
    !
    CALL h5tclose_f(dtype_id, error)
    CALL check("h5tclose_f", error, total_error)
    !
    ! Close the file.
    !
    CALL h5fclose_f(file_id, error)
    CALL check("h5fclose_f", error, total_error)
    IF(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
    CALL check("h5_cleanup_f", error, total_error)

    RETURN
  END SUBROUTINE datasettest

!
!the following subroutine tests h5dextend_f functionality
!

  SUBROUTINE extenddsettest(cleanup, total_error)

    IMPLICIT NONE

    LOGICAL, INTENT(IN)  :: cleanup
    INTEGER, INTENT(OUT) :: total_error

    !
    !the dataset is stored in file "extf.h5"
    !
    CHARACTER(LEN=4), PARAMETER :: filename = "extf"
    CHARACTER(LEN=80) :: fix_filename

    !
    !dataset name is "ExtendibleArray"
    !
    CHARACTER(LEN=15), PARAMETER :: dsetname = "ExtendibleArray"

    !
    !dataset rank is 2
    !
    INTEGER :: RANK = 2

    INTEGER(HID_T) :: file_id       ! File identifier
    INTEGER(HID_T) :: dset_id       ! Dataset identifier
    INTEGER(HID_T) :: dataspace     ! Dataspace identifier
    INTEGER(HID_T) :: memspace      ! memory Dataspace identifier
    INTEGER(HID_T) :: crp_list        ! dataset creation property identifier

    !
    !dataset dimensions at creation time
    !
    INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/3,3/)

    !
    !data dimensions
    !
    INTEGER(HSIZE_T), DIMENSION(2) :: dims1 = (/10,3/)

    !
    !Maximum dimensions
    !
    INTEGER(HSIZE_T), DIMENSION(2) :: maxdims

    !
    !data arrays for reading and writing
    !
    INTEGER, DIMENSION(10,3) :: data_in, data_out

    !
    !Size of data in the file
    !
    INTEGER(HSIZE_T), DIMENSION(2) :: size

    !
    !general purpose integer
    !
    INTEGER :: i, j
    INTEGER(HSIZE_T) :: ih, jh

    !
    !flag to check operation success
    !
    INTEGER :: error

    !
    !Variables used in reading data back
    !
    INTEGER(HSIZE_T), DIMENSION(2) :: dimsr, maxdimsr
    INTEGER :: rankr
    INTEGER(HSIZE_T), DIMENSION(2) :: data_dims

    !
    !data initialization
    !
    DO i = 1, 10
       DO j = 1, 3
          data_in(i,j) = 2
       END DO
    END DO

    !
    !Initialize FORTRAN predefined datatypes
    !
!          CALL h5init_types_f(error)
!               CALL check("h5init_types_f",error,total_error)

    !
    !Create a new file using default properties.
    !
    CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
    IF (error .NE. 0) THEN
       WRITE(*,*) "Cannot modify filename"
       STOP
    ENDIF
    CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
    CALL check("h5fcreate_f",error,total_error)

    !
    !Create the data space with unlimited dimensions.
    !
    maxdims(1:2) = H5S_UNLIMITED_F

    CALL h5screate_simple_f(RANK, dims, dataspace, error, maxdims)
    CALL check("h5screate_simple_f",error,total_error)

    !
    !Modify dataset creation properties, i.e. enable chunking
    !
    CALL h5pcreate_f(H5P_DATASET_CREATE_F, crp_list, error)
    CALL check("h5pcreate_f",error,total_error)

    CALL h5pset_chunk_f(crp_list, RANK, dims1, error)
    CALL check("h5pset_chunk_f",error,total_error)

    !
    !Create a dataset with 3X3 dimensions using cparms creation properties.
    !
    CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, dataspace, dset_id, error, crp_list )
    CALL check("h5dcreate_f",error,total_error)

    !
    !Extend the dataset. This call assures that dataset is 3 x 3.
    !
    SIZE(1) = 3
    SIZE(2) = 3
    CALL h5dextend_f(dset_id, size, error)
    CALL check("h5dextend_f",error,total_error)


    !
    !Extend the dataset. Dataset becomes 10 x 3.
    !
    SIZE(1)   = 10;
    SIZE(2)   = 3;
    CALL h5dextend_f(dset_id, size, error)
    CALL check("h5dextend_f",error,total_error)

    !
    !Write the data of size 10X3 to the extended dataset.
    !
    data_dims(1) = 10
    data_dims(2) = 3
    CALL H5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data_in, data_dims, error)
    CALL check("h5dwrite_f",error,total_error)

    !
    !Close the dataspace for the dataset.
    !
    CALL h5sclose_f(dataspace, error)
    CALL check("h5sclose_f",error,total_error)

    !
    !Close the property list.
    !
    CALL h5pclose_f(crp_list, error)
    CALL check("h5pclose_f",error,total_error)
    !
    !Close the dataset.
    !
    CALL h5dclose_f(dset_id, error)
    CALL check("h5dclose_f",error,total_error)

    !
    !Close the file.
    !
    CALL h5fclose_f(file_id, error)
    CALL check("h5fclose_f",error,total_error)

    !
    !read the data back
    !
    !Open the file.
    !
    CALL h5fopen_f (fix_filename, H5F_ACC_RDONLY_F, file_id, error)
    CALL check("hfopen_f",error,total_error)

    !
    !Open the  dataset.
    !
    CALL h5dopen_f(file_id, dsetname, dset_id, error)
    CALL check("h5dopen_f",error,total_error)

    !
    !Get dataset's dataspace handle.
    !
    CALL h5dget_space_f(dset_id, dataspace, error)
    CALL check("h5dget_space_f",error,total_error)

    !
    !Get dataspace's rank.
    !
    CALL h5sget_simple_extent_ndims_f(dataspace, rankr, error)
    CALL check("h5sget_simple_extent_ndims_f",error,total_error)
    IF (rankr .NE. RANK) THEN
       WRITE(*,*) "dataset rank error occurred"
       STOP
    END IF

    !
    !Get dataspace's dimensinons.
    !
    CALL h5sget_simple_extent_dims_f(dataspace, dimsr, maxdimsr, error)
    CALL check("h5sget_simple_extent_dims_f",error,total_error)
    IF ((dimsr(1) .NE. dims1(1)) .OR. (dimsr(2) .NE. dims1(2))) THEN
       WRITE(*,*) "dataset dimensions error occurred"
       STOP
    END IF

    !
    !Get creation property list.
    !
    CALL h5dget_create_plist_f(dset_id, crp_list, error)
    CALL check("h5dget_create_plist_f",error,total_error)


    !
    !create memory dataspace.
    !
    CALL h5screate_simple_f(rankr, dimsr, memspace, error)
    CALL check("h5screate_simple_f",error,total_error)

    !
    !Read data
    !
    CALL H5dread_f(dset_id, H5T_NATIVE_INTEGER, data_out, data_dims, error, memspace, dataspace)
    CALL check("h5dread_f",error,total_error)


    !
    !Compare the data.
    !
    DO ih = 1, dims1(1)
       DO jh = 1, dims1(2)
          IF (data_out(ih,jh) .NE. data_in(ih, jh)) THEN
             WRITE(*, *) "extend dataset test error occurred"
             WRITE(*, *) "read value is not the same as the written values"
          END IF
       END DO
    END DO

    !
    !Close the dataspace for the dataset.
    !
    CALL h5sclose_f(dataspace, error)
    CALL check("h5sclose_f",error,total_error)

    !
    !Close the memspace for the dataset.
    !
    CALL h5sclose_f(memspace, error)
    CALL check("h5sclose_f",error,total_error)

    !
    !Close the property list.
    !
    CALL h5pclose_f(crp_list, error)
    CALL check("h5pclose_f",error,total_error)

    !
    !Close the dataset.
    !
    CALL h5dclose_f(dset_id, error)
    CALL check("h5dclose_f",error,total_error)

    !
    !Close the file.
    !
    CALL h5fclose_f(file_id, error)
    CALL check("h5fclose_f",error,total_error)
    IF(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
    CALL check("h5_cleanup_f", error, total_error)

    RETURN
  END SUBROUTINE extenddsettest

!
! The following subroutine tests h5dget_offset_f functionality
!

  SUBROUTINE test_userblock_offset(cleanup, total_error)

    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: cleanup
    INTEGER, INTENT(OUT) :: total_error
    !
    !the dataset is stored in file "offset.h5"
    !
    INTEGER, PARAMETER :: dset_dim1=2, dset_dim2=10
    CHARACTER(LEN=6), PARAMETER :: filename = "offset"
    CHARACTER(LEN=80) :: fix_filename

    INTEGER(hid_t) :: file, fcpl, dataset, space
    INTEGER :: i, j, n, ios
    INTEGER(hsize_t), DIMENSION(1:2) :: dims
    INTEGER(haddr_t) :: offset
    INTEGER, DIMENSION(1:dset_dim1,1:dset_dim2), TARGET :: rdata, data_in
    INTEGER :: error
    TYPE(C_PTR) :: f_ptr
    !
    !Create a new file using default properties.
    !
    CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
    IF (error .NE. 0) THEN
       WRITE(*,*) "Cannot modify filename"
       STOP
    ENDIF

    CALL h5pcreate_f(H5P_FILE_CREATE_F, fcpl, error)
    CALL check("h5pcreate_f",error,total_error)

    ! Initialize the dataset
    n = 0
    DO i = 1, dset_dim1
       DO j = 1, dset_dim2
          n = n + 1
          data_in(i,j) = n
       END DO
    END DO
    CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file, error, fcpl)
    CALL check("h5fcreate_f",error,total_error)

    ! Create the data space
    dims(1:2) = (/dset_dim1,dset_dim2/)

    CALL h5screate_simple_f(2, dims, space, error)
    CALL check("h5screate_simple_f",error,total_error)

    ! Create the dataset
    CALL h5dcreate_f(file, "dset1", H5T_NATIVE_INTEGER, space, dataset, error)
    CALL check("h5dcreate_f", error, total_error)

    ! Test dataset address.  Should be undefined.
    CALL h5dget_offset_f(dataset, offset, error)
    CALL VERIFY("h5dget_offset_f",offset, HADDR_UNDEF_F, total_error)

    ! Write the data to the dataset
    f_ptr = C_LOC(data_in(1,1))
    CALL h5dwrite_f(dataset, H5T_NATIVE_INTEGER, f_ptr, error)
    CALL check("h5dwrite_f", error, total_error)

    ! Test dataset address in file. Open the same file as a C file, seek
    ! the data position as H5Dget_offset points to, read the dataset, and
    ! compare it with the data written in.
    CALL h5dget_offset_f(dataset, offset, error)
    CALL check("h5dget_offset_f", error, total_error)
    IF(offset.EQ.HADDR_UNDEF_F)THEN
       total_error = total_error + 1
    ENDIF

    CALL h5dclose_f(dataset, error)
    CALL check("h5dclose_f", error, total_error)
    CALL h5fclose_f(file, error)
    CALL check("h5fclose_f", error, total_error)

    IF(total_error.NE.0) RETURN

    OPEN(10,FILE=fix_filename, ACCESS="STREAM", IOSTAT=ios)
    IF(ios.NE.0)THEN
       WRITE(*,'(A)') "Failed to open file "//TRIM(fix_filename)
       total_error = total_error + 1
       RETURN
    ENDIF
    ! The pos= specifier illustrates that positions are in bytes,
    ! starting from byte 1 (as opposed to C, where they start from byte 0)
    READ(10, POS=offset+1, IOSTAT=ios) rdata
    IF(ios.NE.0)THEN
       WRITE(*,'(A)') "Failed to read data from stream I/O "
       total_error = total_error + 1
       CLOSE(10)
       RETURN
    ENDIF

    ! Check that the values read are the same as the values written
    DO i = 1, dset_dim1
       DO j = 1, dset_dim2
          CALL VERIFY("h5dget_offset_f",rdata(i,j), data_in(i,j), total_error)
          IF(total_error.NE.0)THEN
             WRITE(*,'(A)') "    Read different values than written."
             WRITE(*,'(2(A,I0))') "    At index ",i,",",j
             CLOSE(10)
             RETURN
          ENDIF
       END DO
    END DO

    CLOSE(10)

    IF(cleanup) CALL h5_cleanup_f(fix_filename, H5P_DEFAULT_F, error)
    CALL check("h5_cleanup_f", error, total_error)
    IF(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
    CALL check("h5_cleanup_f", error, total_error)

  END SUBROUTINE test_userblock_offset

  SUBROUTINE test_dset_fill(cleanup, total_error)

    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: cleanup
    INTEGER, INTENT(OUT) :: total_error

    INTEGER, PARAMETER :: DIM0=10
    INTEGER, PARAMETER :: int_kind_1  = SELECTED_INT_KIND(2)  !should map to INTEGER*1 on most modern processors
    INTEGER, PARAMETER :: int_kind_4  = SELECTED_INT_KIND(4)  !should map to INTEGER*2 on most modern processors
    INTEGER, PARAMETER :: int_kind_16 = SELECTED_INT_KIND(18) !should map to INTEGER*8 on most modern processors
    INTEGER(KIND=int_kind_1) , DIMENSION(1:DIM0), TARGET :: data_i1
    INTEGER(KIND=int_kind_4) , DIMENSION(1:DIM0), TARGET :: data_i4
    INTEGER(KIND=int_kind_16), DIMENSION(1:DIM0), TARGET :: data_i16
    INTEGER(KIND=int_kind_1) , TARGET :: data0_i1 = 4
    INTEGER(KIND=int_kind_4) , TARGET :: data0_i4 = 4
    INTEGER(KIND=int_kind_16), TARGET :: data0_i16 = 4
    INTEGER, DIMENSION(1:DIM0) :: data_int
    INTEGER, TARGET :: data0_int = 4
#if H5_HAVE_Fortran_INTEGER_SIZEOF_16!=0
    INTEGER, PARAMETER :: int_kind_32 = SELECTED_INT_KIND(36) !should map to INTEGER*16 on most modern processors
    INTEGER(KIND=int_kind_32), DIMENSION(1:DIM0), TARGET :: data_i32
    INTEGER(KIND=int_kind_32), TARGET :: data0_i32 = 4
#endif
    INTEGER, PARAMETER :: real_kind_4  = C_FLOAT
    INTEGER, PARAMETER :: real_kind_8 = C_DOUBLE
    REAL(KIND=real_kind_4) , DIMENSION(1:DIM0), TARGET :: data_r4
    REAL(KIND=real_kind_8), DIMENSION(1:DIM0), TARGET :: data_r8
    REAL(KIND=real_kind_4) , TARGET :: data0_r4 = 4.0
    REAL(KIND=real_kind_8), TARGET :: data0_r8 = 4.0
#if H5_FORTRAN_C_LONG_DOUBLE_IS_UNIQUE!=0
    INTEGER, PARAMETER :: real_kind_16 = C_LONG_DOUBLE
    REAL(KIND=real_kind_16) , DIMENSION(1:DIM0), TARGET :: data_r16
    REAL(KIND=real_kind_16) , TARGET :: data0_r16 = 4.0
#endif

    INTEGER :: i
    CHARACTER       , DIMENSION(1:DIM0), TARGET :: data_chr
    CHARACTER       , TARGET :: data0_chr = "h"
    INTEGER(hsize_t), DIMENSION(1:1) :: dims
    INTEGER(HID_T) :: space_id
    INTEGER(HID_T) :: fill_type_id
    INTEGER(HID_T) :: buf_type_id
    INTEGER(hssize_t), DIMENSION(1:1) :: ioffset
    INTEGER(hsize_t), DIMENSION(1:1) ::  icount
    INTEGER :: error
    TYPE(C_PTR) :: f_ptr1, f_ptr2

    ! Initialize memory buffer
    data_i1  = -2
    data_i4  = -2
    data_i16 = -2
    data_int = -2
#if H5_HAVE_Fortran_INTEGER_SIZEOF_16!=0
    data_i32 = -2
#endif
    data_r4  = -2.0_real_kind_4
    data_r8 = -2.0_real_kind_8
#if H5_FORTRAN_C_LONG_DOUBLE_IS_UNIQUE!=0
    data_r16 = -2.0_real_kind_16
#endif
    data_chr = "H"

    dims(1) = DIM0
    ioffset(1) = 0
    icount(1) = DIM0/2

    CALL h5screate_simple_f(1, dims, space_id, error)
    CALL check("h5screate_simple_f",error,total_error)

    CALL h5sselect_hyperslab_f(space_id, H5S_SELECT_SET_F, ioffset, icount, error)
    CALL check("h5sselect_hyperslab_f", error, total_error)

    !*********************************************************
    ! TEST LEGACY H5Dfill_f APIs
    !*********************************************************

    CALL h5dfill_f(data0_int, space_id, data_int, error)
    CALL check("h5dfill_f", error, total_error)

    DO i = 1, DIM0
       IF(i.LE. DIM0/2)THEN
          CALL VERIFY("h5dfill_f", data0_int, data_int(i), total_error)
       ELSE
          CALL VERIFY("h5dfill_f", -2, data_int(i), total_error)
       ENDIF
       IF(total_error.NE.0)THEN
          WRITE(*,'(A)')    "    Incorrect h5dfill value (INT)."
          WRITE(*,'(A,I0)') "    At index ",i
          RETURN
       ENDIF
    ENDDO

    CALL h5dfill_f(data0_r4, space_id, data_r4, error)
    CALL check("h5dfill_f", error, total_error)

    DO i = 1, DIM0
       IF(i.LE. DIM0/2)THEN
          CALL VERIFY("h5dfill_f", data0_r4, data_r4(i), total_error)
       ELSE
          CALL VERIFY("h5dfill_f", -2.0_real_kind_4, data_r4(i), total_error)
       ENDIF
       IF(total_error.NE.0)THEN
          WRITE(*,'(A)')    "    Incorrect h5dfill value (R4)."
          WRITE(*,'(A,I0)') "    At index ",i
          RETURN
       ENDIF
    ENDDO

    CALL h5dfill_f(data0_r8, space_id, data_r8, error)
    CALL check("h5dfill_f", error, total_error)

    DO i = 1, DIM0
       IF(i.LE. DIM0/2)THEN
          CALL VERIFY("h5dfill_f", data0_r8, data_r8(i), total_error)
       ELSE
          CALL VERIFY("h5dfill_f", -2.0_real_kind_8, data_r8(i), total_error)
       ENDIF
       IF(total_error.NE.0)THEN
          WRITE(*,'(A)')    "    Incorrect h5dfill value (R4)."
          WRITE(*,'(A,I0)') "    At index ",i
          RETURN
       ENDIF
    ENDDO

    CALL h5dfill_f(data0_chr, space_id, data_chr, error)
    CALL check("h5dfill_f", error, total_error)

    DO i = 1, DIM0
       IF(i.LE. DIM0/2)THEN
          CALL VERIFY("h5dfill_f", data0_chr, data_chr(i), total_error)
       ELSE
          CALL VERIFY("h5dfill_f", "H", data_chr(i), total_error)
       ENDIF
       IF(total_error.NE.0)THEN
          WRITE(*,'(A)')    "    Incorrect h5dfill value (CHR)."
          WRITE(*,'(A,I0)') "    At index ",i
          RETURN
       ENDIF
    ENDDO

#if H5_FORTRAN_C_LONG_DOUBLE_IS_UNIQUE!=0
    CALL h5dfill_f(data0_r16, space_id, data_r16, error)
    CALL check("h5dfill_f", error, total_error)
    DO i = 1, DIM0
       IF(i.LE. DIM0/2)THEN
          CALL VERIFY("h5dfill_f", data0_r16, data_r16(i), total_error)
       ELSE
          CALL VERIFY("h5dfill_f", -2.0_real_kind_16, data_r16(i), total_error)
       ENDIF
       IF(total_error.NE.0)THEN
          WRITE(*,'(A)')    "    Incorrect h5dfill value (R16)."
          WRITE(*,'(A,I0)') "    At index ",i
          RETURN
       ENDIF
    ENDDO
#endif

    !*********************************************************
    ! TEST MODERN H5Dfill_f APIs
    !*********************************************************

    ! Initialize memory buffer
    data_i1  = -2
    data_i4  = -2
    data_i16 = -2
#if H5_HAVE_Fortran_INTEGER_SIZEOF_16!=0
    data_i32 = -2
#endif
    data_r4  = -2.0_real_kind_4
    data_r8 = -2.0_real_kind_8
    data_chr = "H"

    ! Test spectrum of datatype types

    f_ptr1 = C_LOC(data0_i1)
    f_ptr2 = C_LOC(data_i1(1))

    fill_type_id = h5kind_to_type(KIND(data0_i1), H5_INTEGER_KIND)
    buf_type_id  = fill_type_id

    CALL h5dfill_f(f_ptr1, fill_type_id, f_ptr2, buf_type_id, space_id, error)
    CALL check("h5dfill_f", error, total_error)

    DO i = 1, DIM0
       IF(i.LE. DIM0/2)THEN
          CALL VERIFY("h5dfill_f", data0_i1, data_i1(i), total_error)
       ELSE
          CALL VERIFY("h5dfill_f", -2_int_kind_1, data_i1(i), total_error)
       ENDIF
       IF(total_error.NE.0)THEN
          WRITE(*,'(A)')    "    Incorrect h5dfill value (I1)."
          WRITE(*,'(A,I0)') "    At index ",i
          RETURN
       ENDIF
    ENDDO

    f_ptr1 = C_LOC(data0_i4)
    f_ptr2 = C_LOC(data_i4(1))

    fill_type_id = h5kind_to_type(KIND(data0_i4), H5_INTEGER_KIND)
    buf_type_id  = fill_type_id

    CALL h5dfill_f(f_ptr1, fill_type_id, f_ptr2, buf_type_id, space_id, error)
    CALL check("h5dfill_f", error, total_error)

    DO i = 1, DIM0
       IF(i.LE. DIM0/2)THEN
          CALL VERIFY("h5dfill_f", data0_i4, data_i4(i), total_error)
       ELSE
          CALL VERIFY("h5dfill_f", -2_int_kind_4, data_i4(i), total_error)
       ENDIF
       IF(total_error.NE.0)THEN
          WRITE(*,'(A)')    "    Incorrect h5dfill value (I4)."
          WRITE(*,'(A,I0)') "    At index ",i
          RETURN
       ENDIF
    ENDDO

    f_ptr1 = C_LOC(data0_i16)
    f_ptr2 = C_LOC(data_i16(1))

    fill_type_id = h5kind_to_type(KIND(data0_i16), H5_INTEGER_KIND)
    buf_type_id  = fill_type_id

    CALL h5dfill_f(f_ptr1, fill_type_id, f_ptr2, buf_type_id, space_id, error)
    CALL check("h5dfill_f", error, total_error)

    DO i = 1, DIM0
       IF(i.LE. DIM0/2)THEN
          CALL VERIFY("h5dfill_f", data0_i16, data_i16(i), total_error)
       ELSE
          CALL VERIFY("h5dfill_f", -2_int_kind_16, data_i16(i), total_error)
       ENDIF
       IF(total_error.NE.0)THEN
          WRITE(*,'(A)')    "    Incorrect h5dfill value (I16)."
          WRITE(*,'(A,I0)') "    At index ",i
          RETURN
       ENDIF
    ENDDO

#if H5_HAVE_Fortran_INTEGER_SIZEOF_16!=0

    f_ptr1 = C_LOC(data0_i32)
    f_ptr2 = C_LOC(data_i32(1))

    fill_type_id = h5kind_to_type(KIND(data0_i32), H5_INTEGER_KIND)
    buf_type_id  = fill_type_id

    CALL h5dfill_f(f_ptr1, fill_type_id, f_ptr2, buf_type_id, space_id, error)
    CALL check("h5dfill_f", error, total_error)

    DO i = 1, DIM0
       IF(i.LE. DIM0/2)THEN
          CALL VERIFY("h5dfill_f", data0_i32, data_i32(i), total_error)
       ELSE
          CALL VERIFY("h5dfill_f", -2_int_kind_32, data_i32(i), total_error)
       ENDIF
       IF(total_error.NE.0)THEN
          WRITE(*,'(A)')    "    Incorrect h5dfill value (I32)."
          WRITE(*,'(A,I0)') "    At index ",i
          RETURN
       ENDIF
    ENDDO
#endif

    f_ptr1 = C_LOC(data0_r4)
    f_ptr2 = C_LOC(data_r4(1))

    fill_type_id = h5kind_to_type(KIND(data0_r4), H5_REAL_KIND)
    buf_type_id  = fill_type_id

    CALL h5dfill_f(f_ptr1, fill_type_id, f_ptr2, buf_type_id, space_id, error)
    CALL check("h5dfill_f", error, total_error)

    DO i = 1, DIM0
       IF(i.LE. DIM0/2)THEN
          CALL VERIFY("h5dfill_f", data0_r4, data_r4(i), total_error)
       ELSE
          CALL VERIFY("h5dfill_f", -2.0_real_kind_4, data_r4(i), total_error)
       ENDIF
       IF(total_error.NE.0)THEN
          WRITE(*,'(A)')    "    Incorrect h5dfill value (R4)."
          WRITE(*,'(A,I0)') "    At index ",i
          RETURN
       ENDIF
    ENDDO

    f_ptr1 = C_LOC(data0_r8)
    f_ptr2 = C_LOC(data_r8(1))

    fill_type_id = h5kind_to_type(KIND(data0_r8), H5_REAL_KIND)
    buf_type_id  = fill_type_id

    CALL h5dfill_f(f_ptr1, fill_type_id, f_ptr2, buf_type_id, space_id, error)
    CALL check("h5dfill_f", error, total_error)

    DO i = 1, DIM0
       IF(i.LE. DIM0/2)THEN
          CALL VERIFY("h5dfill_f", data0_r8, data_r8(i), total_error)
       ELSE
          CALL VERIFY("h5dfill_f", -2.0_real_kind_8, data_r8(i), total_error)
       ENDIF
       IF(total_error.NE.0)THEN
          WRITE(*,'(A)')    "    Incorrect h5dfill value (R8)."
          WRITE(*,'(A,I0)') "    At index ",i
          RETURN
       ENDIF
    ENDDO

#if H5_FORTRAN_C_LONG_DOUBLE_IS_UNIQUE!=0
    f_ptr1 = C_LOC(data0_r16)
    f_ptr2 = C_LOC(data_r16(1))

    fill_type_id = h5kind_to_type(KIND(data0_r16), H5_REAL_KIND)
    buf_type_id  = fill_type_id

    CALL h5dfill_f(f_ptr1, fill_type_id, f_ptr2, buf_type_id, space_id, error)
    CALL check("h5dfill_f", error, total_error)

    DO i = 1, DIM0
       IF(i.LE. DIM0/2)THEN
          CALL VERIFY("h5dfill_f", data0_r16, data_r16(i), total_error)
       ELSE
          CALL VERIFY("h5dfill_f", -2.0_real_kind_16, data_r16(i), total_error)
       ENDIF
       IF(total_error.NE.0)THEN
          WRITE(*,'(A)')    "    Incorrect h5dfill value (R16)."
          WRITE(*,'(A,I0)') "    At index ",i
          RETURN
       ENDIF
    ENDDO
#endif

    f_ptr1 = C_LOC(data0_chr)
    f_ptr2 = C_LOC(data_chr(1))

    fill_type_id = H5T_NATIVE_CHARACTER
    buf_type_id  = fill_type_id

    CALL h5dfill_f(f_ptr1, fill_type_id, f_ptr2, buf_type_id, space_id, error)
    CALL check("h5dfill_f", error, total_error)

    DO i = 1, DIM0
       IF(i.LE. DIM0/2)THEN
          CALL VERIFY("h5dfill_f", data0_chr, data_chr(i), total_error)
       ELSE
          CALL VERIFY("h5dfill_f", "H", data_chr(i), total_error)
       ENDIF
       IF(total_error.NE.0)THEN
          WRITE(*,'(A)')    "    Incorrect h5dfill value (CHR)."
          WRITE(*,'(A,I0)') "    At index ",i
          RETURN
       ENDIF
    ENDDO

  END SUBROUTINE test_dset_fill

  SUBROUTINE test_direct_chunk_io(cleanup, total_error)

    IMPLICIT NONE

    LOGICAL, INTENT(IN) :: cleanup
    INTEGER, INTENT(OUT) :: total_error
    CHARACTER(LEN=4), PARAMETER :: filename = "doIO"
    CHARACTER(LEN=80) :: fix_filename

    CHARACTER(LEN=15), PARAMETER :: dsetname   = "dset"

    INTEGER :: RANK = 2

    INTEGER(HID_T) :: file_id     ! File identifier
    INTEGER(HID_T) :: dset_id     ! Dataset identifier
    INTEGER(HID_T) :: dataspace   ! Dataspace identifier
    INTEGER(HID_T) :: dcpl        ! dataset creation property identifier

    !
    !dataset dimensions at creation time
    !
    INTEGER, PARAMETER :: DIM0 = 4
    INTEGER, PARAMETER :: DIM1 = 32
    INTEGER(SIZE_T), PARAMETER :: CHUNK0 = DIM0
    INTEGER(SIZE_T), PARAMETER :: CHUNK1 = DIM1/2
    INTEGER(HSIZE_T), DIMENSION(2) :: offset
    INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/DIM0,DIM1/)
    INTEGER, DIMENSION(CHUNK0,CHUNK1), TARGET :: wdata1, rdata1, wdata2, rdata2
    INTEGER(HSIZE_T), DIMENSION(2) :: chunk = (/CHUNK0, CHUNK1/)
    INTEGER :: i, j, n
    INTEGER :: error
    TYPE(C_PTR) :: f_ptr
    INTEGER :: filters
    INTEGER(SIZE_T) :: sizeINT
    INTEGER(HID_T) :: dxpl

    !
    !Create a new file using default properties.
    !
    CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
    IF (error .NE. 0) THEN
       WRITE(*,*) "Cannot modify filename"
       STOP
    ENDIF

    CALL h5pcreate_f(H5P_DATASET_XFER_F, dxpl, error)
    CALL check("h5pcreate_f",error,total_error)

    CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
    CALL check("h5fcreate_f",error,total_error)

    ! Dataset Fortran

    CALL h5screate_simple_f(RANK, dims, dataspace, error)
    CALL check("h5screate_simple_f",error,total_error)

    CALL h5pcreate_f(H5P_DATASET_CREATE_F, dcpl, error)
    CALL check("h5pcreate_f",error,total_error)

    CALL h5pset_chunk_f(dcpl, RANK, chunk, error)
    CALL check("h5pset_chunk_f",error,total_error)

    CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, dataspace, dset_id, error, dcpl )
    CALL check("h5dcreate_f",error,total_error)

    CALL h5sclose_f(dataspace, error)
    CALL check("h5sclose_f",error,total_error)
    CALL h5pclose_f(dcpl, error)
    CALL check("h5pclose_f",error,total_error)

    n = 0
    DO i = 1, CHUNK0
       DO j = 1, CHUNK1
          n = n + 1
          wdata1(i,j) = n
          wdata2(i,j) = n*10
       END DO
    END DO

#ifdef H5_FORTRAN_HAVE_STORAGE_SIZE
    sizeINT = storage_size(i, KIND=size_t)/storage_size(c_char_'a',c_size_t)
#else
    sizeINT = SIZEOF(i)
#endif

    f_ptr = C_LOC(wdata1)
    offset(1:2) = (/0, 0/)
    CALL H5Dwrite_chunk_f(dset_id, 0, offset, CHUNK0 * CHUNK1 * sizeINT, f_ptr, error)
    CALL check("h5dwrite_f",error,total_error)

    f_ptr = C_LOC(wdata2)
    offset(1:2) = (/0, 16/)
    CALL H5Dwrite_chunk_f(dset_id, 0, offset, CHUNK0 * CHUNK1 * sizeINT, f_ptr, error, dxpl)
    CALL check("h5dwrite_f",error,total_error)

    CALL h5dclose_f(dset_id, error)
    CALL check("h5dclose_f",error,total_error)

    !
    !Close the file.
    !
    CALL h5fclose_f(file_id, error)
    CALL check("h5fclose_f",error,total_error)

    !
    !read the data back
    !
    !Open the file.
    !
    CALL h5fopen_f(fix_filename, H5F_ACC_RDONLY_F, file_id, error)
    CALL check("hfopen_f",error,total_error)

    !
    !Open the  dataset.
    !
    CALL h5dopen_f(file_id, dsetname, dset_id, error)
    CALL check("h5dopen_f",error,total_error)

    f_ptr = C_LOC(rdata1)
    filters = 99
    offset(1:2) = (/0, 0/)
    CALL H5Dread_chunk_f(dset_id, offset, filters, f_ptr, error)
    CALL check("H5Dread_chunk_f",error,total_error)

    ! Verify that the data read was correct.
    DO i = 1, CHUNK0
       DO j = 1, CHUNK1
          CALL VERIFY("H5Dread_chunk_f", rdata1(i,j), wdata1(i,j), total_error)
          IF(total_error.NE.0) EXIT
       ENDDO
    ENDDO

    CALL VERIFY("H5Dread_chunk_f",filters, 0, total_error)

    f_ptr = C_LOC(rdata2)
    offset(1:2) = (/0, 16/)
    CALL H5Dread_chunk_f(dset_id, offset, filters, f_ptr, error, dxpl)
    CALL check("H5Dread_chunk_f",error,total_error)

    ! Verify that the data read was correct.
    DO i = 1, CHUNK0
       DO j = 1, CHUNK1
          CALL VERIFY("H5Dread_chunk_f", rdata2(i,j), wdata2(i,j), total_error)
          IF(total_error.NE.0) EXIT
       ENDDO
    ENDDO

    CALL VERIFY("H5Dread_chunk_f",filters, 0, total_error)

    CALL h5dclose_f(dset_id, error)
    CALL check("h5dclose_f",error,total_error)

    !
    !Close the file.
    !
    CALL h5fclose_f(file_id, error)
    CALL check("h5fclose_f",error,total_error)

    CALL h5pclose_f(dxpl, error)
    CALL check("h5pclose_f",error,total_error)

    IF(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
    CALL check("h5_cleanup_f", error, total_error)

    RETURN
  END SUBROUTINE test_direct_chunk_io

END MODULE TH5D

