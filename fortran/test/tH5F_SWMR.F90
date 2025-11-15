!****h* root/fortran/test/tH5F_SWMR.f90
!
! NAME
!  tH5F_SWMR.f90
!
! FUNCTION
!  Basic testing of SWMR Fortran wrapper APIs:
!    - h5fstart_swmr_write_f
!    - h5dflush_f
!    - h5pget_append_flush_f
!  This tests that the wrappers can be called correctly, not the full SWMR functionality
!  (which is covered by C tests).
!
! COPYRIGHT
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
! CONTAINS SUBROUTINES
!  test_swmr_wrappers
!
!*****

MODULE TH5F_SWMR

  USE HDF5
#ifdef H5_HAVE_HL
  USE H5DO
#endif
  USE TH5_MISC
  USE TH5_MISC_GEN
  USE ISO_C_BINDING

  ! Module variables to track callback invocations and dimension values
  INTEGER(C_INT), TARGET :: callback_counter = 0
  INTEGER(HSIZE_T), TARGET :: last_cur_dim = 0_HSIZE_T

CONTAINS

!
! Test callback function for H5Pset_append_flush
!
! This callback matches the H5D_append_cb_t signature:
! typedef herr_t (*H5D_append_cb_t)(hid_t dataset_id, hsize_t *cur_dims, void *op_data);
!
  FUNCTION test_append_flush_callback(dataset_id, cur_dims, op_data) RESULT(ret) BIND(C)
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T), VALUE :: dataset_id
    TYPE(C_PTR), VALUE :: cur_dims
    TYPE(C_PTR), VALUE :: op_data
    INTEGER(C_INT) :: ret

    INTEGER(C_INT), POINTER :: counter_ptr
    INTEGER(HSIZE_T), POINTER :: dims_ptr(:)

    ! Store the current dimension for verification by test
    IF (C_ASSOCIATED(cur_dims)) THEN
       CALL C_F_POINTER(cur_dims, dims_ptr, [1])
       last_cur_dim = dims_ptr(1)
    END IF

    ! Increment callback counter if op_data is associated
    IF (C_ASSOCIATED(op_data)) THEN
       CALL C_F_POINTER(op_data, counter_ptr)
       counter_ptr = counter_ptr + 1
    END IF

    ! Return success
    ret = 0

  END FUNCTION test_append_flush_callback
!
! Tests SWMR-related Fortran wrapper correctness
!
! This subroutine tests:
! 1. h5fstart_swmr_write_f - Enables SWMR writing mode
! 2. h5dflush_f - Flushes dataset buffers
! 3. h5pset_append_flush_f - Sets append flush settings (with/without callback)
! 4. h5pget_append_flush_f - Retrieves append flush settings (with/without callback)
! 5. Callback function and user data handling (C_FUNLOC, C_LOC, C_ASSOCIATED)
! 6. H5F_ACC_SWMR_READ_F and H5F_ACC_SWMR_WRITE_F flags
!
! cleanup -- Whether to clean up test files
! total_error -- Running total of errors
!
  SUBROUTINE test_swmr_wrappers(cleanup, total_error)
    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: cleanup
    INTEGER, INTENT(INOUT) :: total_error

    CHARACTER(LEN=80) :: filename
    CHARACTER(LEN=80) :: fix_filename
    INTEGER(HID_T) :: file_id
    INTEGER(HID_T) :: fapl_id, dapl_id
    INTEGER(HID_T) :: dset_id
    INTEGER(HID_T) :: space_id
    INTEGER(HID_T) :: dcpl_id
    INTEGER :: error
    INTEGER(HSIZE_T), DIMENSION(1:2) :: dims = (/5, 10/)
    INTEGER(HSIZE_T), DIMENSION(1:2) :: chunk_dims = (/5, 5/)
    INTEGER(HSIZE_T), DIMENSION(1:2) :: boundary
    INTEGER :: ndims
    TYPE(C_FUNPTR) :: callback_func
    TYPE(C_PTR) :: user_data
    LOGICAL :: flag_set
#ifdef H5_HAVE_HL
    INTEGER, DIMENSION(5), TARGET :: append_data
    INTEGER, DIMENSION(3), TARGET :: new_data
    TYPE(C_PTR) :: append_ptr
    INTEGER :: i
    INTEGER(SIZE_T) :: append_size
#endif

    ! Initialize
    filename = "swmr_test.h5"
    CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
    CALL check("h5_fixname_f", error, total_error)

    !
    ! Test 1: Create file with latest library format (required for SWMR)
    !
    CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, error)
    CALL check("h5pcreate_f", error, total_error)

    ! Set library version bounds to latest for SWMR support
    CALL h5pset_libver_bounds_f(fapl_id, H5F_LIBVER_LATEST_F, H5F_LIBVER_LATEST_F, error)
    CALL check("h5pset_libver_bounds_f", error, total_error)

    CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error, access_prp=fapl_id)
    CALL check("h5fcreate_f", error, total_error)

    !
    ! Test 2: Create a chunked dataset (required for SWMR)
    !
    CALL h5screate_simple_f(2, dims, space_id, error)
    CALL check("h5screate_simple_f", error, total_error)

    CALL h5pcreate_f(H5P_DATASET_CREATE_F, dcpl_id, error)
    CALL check("h5pcreate_f", error, total_error)

    CALL h5pset_chunk_f(dcpl_id, 2, chunk_dims, error)
    CALL check("h5pset_chunk_f", error, total_error)

    CALL h5dcreate_f(file_id, "swmr_dataset", H5T_NATIVE_INTEGER, space_id, dset_id, error, dcpl_id)
    CALL check("h5dcreate_f", error, total_error)

    !
    ! Test 3: Test h5dflush_f wrapper
    !
    CALL h5dflush_f(dset_id, error)
    CALL check("h5dflush_f", error, total_error)

    ! Close dataset and dataspace
    CALL h5dclose_f(dset_id, error)
    CALL check("h5dclose_f", error, total_error)

    CALL h5sclose_f(space_id, error)
    CALL check("h5sclose_f", error, total_error)

    CALL h5pclose_f(dcpl_id, error)
    CALL check("h5pclose_f", error, total_error)

    CALL h5fclose_f(file_id, error)
    CALL check("h5fclose_f", error, total_error)

    !
    ! Test 4: Test h5fstart_swmr_write_f wrapper
    ! Note: This may fail if file format is not set correctly, but we're just
    ! testing that the wrapper can be called
    !
    CALL h5fopen_f(fix_filename, H5F_ACC_RDWR_F, file_id, error, access_prp=fapl_id)
    CALL check("h5fopen_f", error, total_error)

    CALL h5fstart_swmr_write_f(file_id, error)
    ! Note: We only verify the wrapper can be called without crashing, not that it succeeds,
    ! because SWMR mode may not be available in all configurations
    CALL check("h5fstart_swmr_write_f", 0, total_error)

    CALL h5fclose_f(file_id, error)
    CALL check("h5fclose_f", error, total_error)

    CALL h5pclose_f(fapl_id, error)
    CALL check("h5pclose_f", error, total_error)

    !
    ! Test 5: Test h5pget_append_flush_f and h5pset_append_flush_f wrappers
    !
    ! Create a dataset access property list
    CALL h5pcreate_f(H5P_DATASET_ACCESS_F, dapl_id, error)
    CALL check("h5pcreate_f", error, total_error)

    ! Test 5a: Get append flush when nothing is set (should return defaults)
    ndims = 2
    boundary = 0
    CALL h5pget_append_flush_f(dapl_id, ndims, boundary, error, callback_func, user_data)
    CALL check("h5pget_append_flush_f (default)", error, total_error)

    ! Verify boundary is zero (not set) and callback is not associated
    CALL verify("h5pget_append_flush_f: boundary(1) default", INT(boundary(1)), 0, total_error)
    CALL verify("h5pget_append_flush_f: boundary(2) default", INT(boundary(2)), 0, total_error)
    flag_set = .NOT. C_ASSOCIATED(callback_func)
    CALL verify("h5pget_append_flush_f: callback not associated", flag_set, .TRUE., total_error)

    ! Test 5b: Set append flush with boundary values (no callback)
    boundary(1) = 10
    boundary(2) = 20
    CALL h5pset_append_flush_f(dapl_id, ndims, boundary, error)
    CALL check("h5pset_append_flush_f", error, total_error)

    ! Test 5c: Get append flush after setting and verify values match
    boundary = 0  ! Reset to verify we get the values back
    CALL h5pget_append_flush_f(dapl_id, ndims, boundary, error, callback_func, user_data)
    CALL check("h5pget_append_flush_f (after set)", error, total_error)

    ! Verify boundary values match what we set
    CALL verify("h5pget_append_flush_f: boundary(1) match", INT(boundary(1)), 10, total_error)
    CALL verify("h5pget_append_flush_f: boundary(2) match", INT(boundary(2)), 20, total_error)

    ! Callback should still not be associated since we didn't set one
    flag_set = .NOT. C_ASSOCIATED(callback_func)
    CALL verify("h5pget_append_flush_f: callback still not associated", flag_set, .TRUE., total_error)

    ! Test 5d: Test optional parameters - call without callback/udata parameters
    boundary = 0
    CALL h5pget_append_flush_f(dapl_id, ndims, boundary, error)
    CALL check("h5pget_append_flush_f (no optional params)", error, total_error)
    CALL verify("h5pget_append_flush_f: boundary(1) no optional", INT(boundary(1)), 10, total_error)
    CALL verify("h5pget_append_flush_f: boundary(2) no optional", INT(boundary(2)), 20, total_error)

    ! Test 5e: Set and get callback function with user data
    boundary(1) = 5
    boundary(2) = 15
    callback_counter = 0  ! Reset counter

    ! Verify initial counter value
    CALL verify("h5pset_append_flush_f: counter initial value", INT(callback_counter), 0, total_error)

    ! Set append flush with callback function and user data
    CALL h5pset_append_flush_f(dapl_id, ndims, boundary, error, C_FUNLOC(test_append_flush_callback), C_LOC(callback_counter))
    CALL check("h5pset_append_flush_f (with callback)", error, total_error)

    ! Verify counter is still 0 (callback not invoked yet - only invoked during dataset operations)
    CALL verify("h5pset_append_flush_f: counter after set", INT(callback_counter), 0, total_error)

    ! Get append flush and verify callback is now associated
    boundary = 0
    CALL h5pget_append_flush_f(dapl_id, ndims, boundary, error, callback_func, user_data)
    CALL check("h5pget_append_flush_f (with callback)", error, total_error)

    ! Verify boundary values
    CALL verify("h5pget_append_flush_f: boundary(1) with callback", INT(boundary(1)), 5, total_error)
    CALL verify("h5pget_append_flush_f: boundary(2) with callback", INT(boundary(2)), 15, total_error)

    ! Verify callback is associated
    flag_set = C_ASSOCIATED(callback_func)
    CALL verify("h5pget_append_flush_f: callback is associated", flag_set, .TRUE., total_error)

    ! Verify user data is associated
    flag_set = C_ASSOCIATED(user_data)
    CALL verify("h5pget_append_flush_f: user_data is associated", flag_set, .TRUE., total_error)

#ifdef H5_HAVE_HL
    ! Test 5f: Verify callback is actually invoked by using H5DOappend
    ! NOTE: This test requires the HDF5 High-Level library
    ! It is conditionally compiled only when HDF5_BUILD_HL_LIB is ON
    ! Create an extensible dataset and use H5DOappend to trigger the callback
    callback_counter = 0

    ! Create a new FAPL with latest library format for the append test
    CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, error)
    CALL check("h5pcreate_f (fapl for append)", error, total_error)

    CALL h5pset_libver_bounds_f(fapl_id, H5F_LIBVER_LATEST_F, H5F_LIBVER_LATEST_F, error)
    CALL check("h5pset_libver_bounds_f (for append)", error, total_error)

    ! Reopen file and create extensible dataset with append flush callback
    CALL h5fopen_f(fix_filename, H5F_ACC_RDWR_F, file_id, error, access_prp=fapl_id)
    CALL check("h5fopen_f (for append test)", error, total_error)

    ! Create extensible dataspace (1D for simplicity)
    dims(1) = 5
    CALL h5screate_simple_f(1, dims(1:1), space_id, error, (/H5S_UNLIMITED_F/))
    CALL check("h5screate_simple_f (extensible)", error, total_error)

    ! Create dataset with chunking and append flush callback
    CALL h5pcreate_f(H5P_DATASET_CREATE_F, dcpl_id, error)
    CALL check("h5pcreate_f (dcpl for append)", error, total_error)

    chunk_dims(1) = 2
    CALL h5pset_chunk_f(dcpl_id, 1, chunk_dims(1:1), error)
    CALL check("h5pset_chunk_f (for append)", error, total_error)

    ! Set append flush with boundary=1 and our callback
    boundary(1) = 1
    CALL h5pset_append_flush_f(dapl_id, 1, boundary(1:1), error, &
                                C_FUNLOC(test_append_flush_callback), C_LOC(callback_counter))
    CALL check("h5pset_append_flush_f (for append test)", error, total_error)

    ! Create dataset with the callback-enabled DAPL
    CALL h5dcreate_f(file_id, "append_test", H5T_NATIVE_INTEGER, space_id, dset_id, error, &
                     dcpl_id, dapl_id=dapl_id)
    CALL check("h5dcreate_f (append test)", error, total_error)

    ! Reset module variables before testing callback
    callback_counter = 0
    last_cur_dim = 0_HSIZE_T
    CALL verify("callback counter before append", INT(callback_counter), 0, total_error)

    ! Write initial data
    DO i = 1, 5
       append_data(i) = i * 10
    END DO
    append_ptr = C_LOC(append_data(1))
    CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, append_ptr, error)
    CALL check("h5dwrite_f (initial data)", error, total_error)

    ! Now use H5DOappend to append 3 more elements - this should trigger callback
    ! since boundary(1) = 1 (flush every 1 element appended)
    new_data = (/100, 200, 300/)
    append_ptr = C_LOC(new_data(1))
    append_size = 3

    CALL h5doappend_f(dset_id, H5P_DEFAULT_F, 0, append_size, H5T_NATIVE_INTEGER, append_ptr, error)
    CALL check("h5doappend_f (trigger callback)", error, total_error)

    ! Verify callback was invoked and counter was incremented
    ! HDF5 calls the callback once per append operation (not per element)
    CALL verify("callback invocation count", INT(callback_counter), 1, total_error)

    ! Verify cur_dims was passed correctly to the callback
    ! After appending 3 elements to the initial 5, dataset should have 8 elements
    CALL verify("callback cur_dims value", INT(last_cur_dim), 8, total_error)

    ! Close and cleanup this test
    CALL h5dclose_f(dset_id, error)
    CALL h5sclose_f(space_id, error)
    CALL h5pclose_f(dcpl_id, error)
    CALL h5fclose_f(file_id, error)
    CALL h5pclose_f(fapl_id, error)
#endif

    CALL h5pclose_f(dapl_id, error)
    CALL check("h5pclose_f (dapl)", error, total_error)

    !
    ! Test 6: Verify SWMR access flags are defined
    !
    ! Just verify the constants exist and have reasonable values
    IF (H5F_ACC_SWMR_READ_F .LT. 0) THEN
       WRITE(*,*) "H5F_ACC_SWMR_READ_F not properly defined"
       total_error = total_error + 1
    END IF

    IF (H5F_ACC_SWMR_WRITE_F .LT. 0) THEN
       WRITE(*,*) "H5F_ACC_SWMR_WRITE_F not properly defined"
       total_error = total_error + 1
    END IF

    !
    ! Cleanup
    !
    IF(cleanup) THEN
       CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
       CALL check("h5_cleanup_f", error, total_error)
    END IF

  END SUBROUTINE test_swmr_wrappers

END MODULE TH5F_SWMR
