! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the LICENSE file, which can be found at the root of the source code       *
!   distribution tree, or in https://www.hdfgroup.org/licenses.               *
!   If you do not have access to either file, you may request a copy from     *
!   help@hdfgroup.org.                                                        *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
!
! This file contains the FORTRAN90 tests for H5DO
!

MODULE TSTDO

  USE TH5_MISC_GEN
  IMPLICIT NONE

CONTAINS

  !-------------------------------------------------------------------------
  ! test_begin
  !-------------------------------------------------------------------------

  SUBROUTINE test_begin(string)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: string
    WRITE(*, fmt = '(14a)', advance = 'no') string
    WRITE(*, fmt = '(40x,a)', advance = 'no') ' '
  END SUBROUTINE test_begin

  !-------------------------------------------------------------------------
  ! passed
  !-------------------------------------------------------------------------

  SUBROUTINE passed()
    IMPLICIT NONE
    WRITE(*, fmt = '(6a)')  'PASSED'
  END SUBROUTINE passed

END MODULE TSTDO

MODULE TSTDO_TESTS

  USE, INTRINSIC :: ISO_C_BINDING
  USE H5DO    ! module of H5DO
  USE HDF5    ! module of HDF5 library
  USE TSTDO   ! module for testing DO support routines
  IMPLICIT NONE

CONTAINS

  !-------------------------------------------------------------------------
  ! test_h5doappend
  !-------------------------------------------------------------------------

  SUBROUTINE test_h5doappend()

    IMPLICIT NONE

    CHARACTER(LEN=11), PARAMETER :: filename = "doappend.h5"  ! File name
    CHARACTER(LEN=5), PARAMETER :: dsetname = "dset1"         ! Dataset name
    INTEGER(HID_T) :: file_id                                 ! File identifier
    INTEGER(HID_T) :: dset_id                                 ! Dataset identifier
    INTEGER(HID_T) :: space_id                                ! Dataspace identifier
    INTEGER(HID_T) :: dcpl_id                                 ! Dataset creation property list
    INTEGER(HID_T) :: fapl_id                                 ! File access property list
    INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/5, 10/)        ! Initial dataset dimensions
    INTEGER(HSIZE_T), DIMENSION(2) :: maxdims                 ! Maximum dataset dimensions
    INTEGER(HSIZE_T), DIMENSION(2) :: chunk_dims = (/5, 5/)  ! Chunk dimensions
    INTEGER(HSIZE_T), DIMENSION(2) :: boundary = (/1, 1/)     ! Boundary for append flush
    INTEGER, DIMENSION(5,10), TARGET :: wdata                 ! Write buffer
    INTEGER, DIMENSION(5,5), TARGET :: wdata_append           ! Append buffer
    INTEGER, DIMENSION(5,15), TARGET :: rdata                 ! Read buffer
    INTEGER :: errcode                                        ! Error flag
    INTEGER :: i, j                                           ! Loop indices
    TYPE(C_PTR) :: f_ptr
    INTEGER(HSIZE_T), DIMENSION(2) :: current_dims            ! Current dataset dimensions
    INTEGER :: axis                                           ! Axis to append to
    INTEGER(SIZE_T) :: extension                              ! Number of elements to append

    CALL test_begin(' H5DOappend test              ')

    !
    ! Initialize the data arrays
    !
    DO i = 1, 5
       DO j = 1, 10
          wdata(i,j) = (i-1)*10 + j
       END DO
    END DO

    DO i = 1, 5
       DO j = 1, 5
          wdata_append(i,j) = 100 + (i-1)*5 + j
       END DO
    END DO

    !
    ! Initialize FORTRAN predefined datatypes
    !
    CALL h5open_f(errcode)

    !
    ! Create file access property list with latest library format
    !
    CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, errcode)
    CALL h5pset_libver_bounds_f(fapl_id, H5F_LIBVER_LATEST_F, H5F_LIBVER_LATEST_F, errcode)

    !
    ! Create a new file
    !
    CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode, access_prp=fapl_id)

    !
    ! Create dataspace with unlimited maximum dimensions
    ! In Fortran dims=(/5,10/), we want to extend second dimension (columns)
    ! In C this is stored as {10,5}, so we make first C dimension (axis=0) unlimited
    !
    maxdims = (/INT(5,HSIZE_T), H5S_UNLIMITED_F/)
    CALL h5screate_simple_f(2, dims, space_id, errcode, maxdims)

    !
    ! Create dataset creation property list and set chunking
    !
    CALL h5pcreate_f(H5P_DATASET_CREATE_F, dcpl_id, errcode)
    CALL h5pset_chunk_f(dcpl_id, 2, chunk_dims, errcode)

    !
    ! Create the dataset
    !
    CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, space_id, dset_id, errcode, dcpl_id)

    !
    ! Write initial data to the dataset
    !
    f_ptr = C_LOC(wdata(1,1))
    CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, f_ptr, errcode)

    !
    ! Append data along dimension 0 (0-based, which extends the first dimension in C order)
    ! This extends the columns in Fortran (second dimension)
    !
    axis = 0
    extension = 5
    f_ptr = C_LOC(wdata_append(1,1))
    CALL h5doappend_f(dset_id, H5P_DEFAULT_F, axis, extension, H5T_NATIVE_INTEGER, f_ptr, errcode)

    IF (errcode .NE. 0) THEN
       PRINT *, 'Error: H5DOappend failed with error code ', errcode
       STOP
    END IF

    !
    ! Verify the dataset was extended correctly
    !
    CALL h5dget_space_f(dset_id, space_id, errcode)
    CALL h5sget_simple_extent_dims_f(space_id, current_dims, maxdims, errcode)

    ! Check dimensions
    IF (current_dims(1) .NE. 5) THEN
       PRINT *, 'Error: dimension 1 should be 5, got ', current_dims(1)
       STOP
    END IF

    IF (current_dims(2) .NE. 15) THEN
       PRINT *, 'Error: dimension 2 should be 15, got ', current_dims(2)
       STOP
    END IF

    !
    ! Read the entire dataset
    !
    f_ptr = C_LOC(rdata(1,1))
    CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, f_ptr, errcode)

    !
    ! Verify the initial data
    !
    DO i = 1, 5
       DO j = 1, 10
          IF (rdata(i,j) .NE. wdata(i,j)) THEN
             PRINT *, 'Error: initial data mismatch at (', i, ',', j, ')'
             PRINT *, 'Expected:', wdata(i,j), ' Got:', rdata(i,j)
             STOP
          END IF
       END DO
    END DO

    !
    ! Verify the appended data
    !
    DO i = 1, 5
       DO j = 1, 5
          IF (rdata(i,j+10) .NE. wdata_append(i,j)) THEN
             PRINT *, 'Error: appended data mismatch at (', i, ',', j+10, ')'
             PRINT *, 'Expected:', wdata_append(i,j), ' Got:', rdata(i,j+10)
             STOP
          END IF
       END DO
    END DO

    !
    ! Close resources
    !
    CALL h5dclose_f(dset_id, errcode)
    CALL h5sclose_f(space_id, errcode)
    CALL h5pclose_f(dcpl_id, errcode)
    CALL h5pclose_f(fapl_id, errcode)
    CALL h5fclose_f(file_id, errcode)

    !
    ! Close FORTRAN predefined datatypes
    !
    CALL h5close_f(errcode)

    CALL passed()

  END SUBROUTINE test_h5doappend

END MODULE TSTDO_TESTS

PROGRAM do_test

  USE TSTDO_TESTS ! module for testing DO routines
  IMPLICIT NONE

  CALL test_h5doappend()

END PROGRAM do_test
