!****h* root/fortran/test/fortranlib_test_1_8.f90
!
! NAME
!  fortranlib_test_1_8.f90
!
! FUNCTION
!  Basic testing of Fortran API's introduced in 1.8 release.
!
! COPYRIGHT
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   Copyright by the Board of Trustees of the University of Illinois.         *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the files COPYING and Copyright.html.  COPYING can be found at the root   *
!   of the source code distribution tree; Copyright.html can be found at the  *
!   root level of an installed copy of the electronic HDF5 document set and   *
!   is linked from the top-level documents page.  It can also be found at     *
!   http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
!   access to either file, you may request a copy from help@hdfgroup.org.     *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
!*****

PROGRAM fortranlibtest

  USE HDF5

  IMPLICIT NONE
  INTEGER :: total_error = 0
  INTEGER :: error
  INTEGER :: ret_total_error
  INTEGER :: majnum, minnum, relnum
  LOGICAL :: cleanup, status

  CALL h5open_f(error)

  cleanup = .TRUE.
  CALL h5_env_nocleanup_f(status)
  IF(status) cleanup=.FALSE.

  WRITE(*,*) '                       ==========================                            '
  WRITE(*,*) '                              FORTRAN 1.8 tests '
  WRITE(*,*) '                       ==========================                            '
  CALL h5get_libversion_f(majnum, minnum, relnum, total_error)
  IF(total_error .EQ. 0) THEN
     WRITE(*, '(" FORTRANLIB_TEST is linked with HDF5 Library version ")', advance="NO")
     WRITE(*, '(I1)', advance="NO") majnum
     WRITE(*, '(".")', advance="NO")
     WRITE(*, '(I1)', advance="NO") minnum
     WRITE(*, '(" release ")', advance="NO")
     WRITE(*, '(I3)') relnum
  ELSE
     total_error = total_error + 1
  ENDIF
  WRITE(*,*)

  ret_total_error = 0
  CALL file_space("file_space_1_8",cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing file free space', &
       total_error)

  ret_total_error = 0
  CALL attribute_test_1_8(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing attributes', &
       total_error)

  ret_total_error = 0
  CALL group_test(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing groups', &
       total_error)

  ret_total_error = 0
  CALL test_h5o(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing object interface', &
       total_error)

  ret_total_error = 0
  CALL dtransform(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing data transform', &
       total_error)

  ret_total_error = 0
  CALL test_h5s_encode(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing dataspace encoding and decoding', &
       total_error)

  ret_total_error = 0
  CALL test_scaleoffset(cleanup, ret_total_error )
  CALL write_test_status(ret_total_error, &
       ' Testing scaleoffset filter', &
       total_error)

  WRITE(*,*)

  WRITE(*,*) '                  ============================================  '
  WRITE(*, fmt = '(19x, 27a)', advance='NO') ' FORTRAN tests completed with '
  WRITE(*, fmt = '(i4)', advance='NO') total_error
  WRITE(*, fmt = '(12a)' ) ' error(s) ! '
  WRITE(*,*) '                  ============================================  '

  CALL h5close_f(error)

  ! if errors detected, exit with non-zero code.
  IF (total_error .NE. 0) CALL h5_exit_f (1)

END PROGRAM fortranlibtest

SUBROUTINE dtransform(cleanup, total_error)
  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(INOUT) :: total_error

  INTEGER(HID_T) :: dxpl_id_c_to_f
  INTEGER(HID_T) :: file_id

  CHARACTER(LEN=15), PARAMETER :: c_to_f = "(9/5.0)*x + 123"
  INTEGER :: error
  CHARACTER(LEN=15) :: ptrgetTest
  CHARACTER(LEN=7) :: ptrgetTest_small
  CHARACTER(LEN=30) :: ptrgetTest_big

  INTEGER(SIZE_T) :: size

  CALL H5Fcreate_f("dtransform.h5", H5F_ACC_TRUNC_F, file_id, error)
  CALL check("dtransform.H5Fcreate_f", error, total_error)

  CALL H5Pcreate_f(H5P_DATASET_XFER_F, dxpl_id_c_to_f, error)
  CALL check("dtransform.H5Pcreate_f", error, total_error)

  CALL H5Pset_data_transform_f(dxpl_id_c_to_f, c_to_f, error)
  CALL check("dtransform.H5Pset_data_transform_f", error, total_error)

  CALL H5Pget_data_transform_f(dxpl_id_c_to_f, ptrgetTest, error, size=size)
  CALL check("dtransform.H5Pget_data_transform_f",  error, total_error)
  CALL VerifyString("dtransform.H5Pget_data_transform_f", c_to_f, ptrgetTest, total_error)
  CALL VERIFY("dtransform.H5Pget_data_transform_f", INT(size),15, total_error)

! check case when receiving buffer to small

  CALL H5Pget_data_transform_f(dxpl_id_c_to_f, ptrgetTest_small, error, size=size)
  CALL check("dtransform.H5Pget_data_transform_f",  error, total_error)
  CALL VerifyString("dtransform.H5Pget_data_transform_f", c_to_f(1:7), ptrgetTest_small, total_error)
  CALL VERIFY("dtransform.H5Pget_data_transform_f", INT(size),15, total_error)

! check case when receiving buffer to big

  CALL H5Pget_data_transform_f(dxpl_id_c_to_f, ptrgetTest_big, error, size=size)
  CALL check("dtransform.H5Pget_data_transform_f",  error, total_error)
  CALL VerifyString("dtransform.H5Pget_data_transform_f", c_to_f(1:15), ptrgetTest_big(1:15), total_error)
  CALL VERIFY("dtransform.H5Pget_data_transform_f", INT(size), 15, total_error)

  CALL H5Fclose_f(file_id, error)
  CALL check("H5Fclose_f", error, total_error)

  IF(cleanup) CALL h5_cleanup_f("dtransform", H5P_DEFAULT_F, error)
  CALL check("h5_cleanup_f", error, total_error)


END SUBROUTINE dtransform


!/****************************************************************
!**
!**  test_genprop_basic_class(): Test basic generic property list code.
!**      Tests creating new generic classes.
!**
!****************************************************************/

SUBROUTINE test_genprop_basic_class(cleanup, total_error)

  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(INOUT) :: total_error

  INTEGER(HID_T) :: cid1		!/* Generic Property class ID */
  INTEGER(HID_T) :: cid2		!/* Generic Property class ID */

  CHARACTER(LEN=7) :: CLASS1_NAME = "Class 1"
  CHARACTER(LEN=7)  :: name              ! /* Name of class */
  CHARACTER(LEN=10) :: name_big          ! /* Name of class bigger buffer */
  CHARACTER(LEN=4)  :: name_small        ! /* Name of class smaller buffer*/
  INTEGER :: error
  INTEGER :: size
  LOGICAL :: flag

  !/* Output message about test being performed */

  !WRITE(*,*) "Testing Basic Generic Property List Class Creation Functionality"

  ! Try some bogus value for class identifier; function should fail gracefully

  cid1 = 456
  CALL H5Pget_class_name_f(cid1, name, size, error)
  CALL VERIFY("H5Pget_class_name", error, -1, error)

  ! /* Create a new generic class, derived from the root of the class hierarchy */
  CALL H5Pcreate_class_f(H5P_ROOT_F, CLASS1_NAME, cid1, error)
  CALL check("H5Pcreate_class", error, total_error)

  ! /* Check class name */
  CALL H5Pget_class_name_f(cid1, name, size, error)
  CALL check("H5Pget_class_name", error, total_error)
  CALL VERIFY("H5Pget_class_name", size,7,error)
  CALL verifystring("H5Pget_class_name", name, CLASS1_NAME, error)
  IF(error.NE.0)THEN
     WRITE(*,*) 'Class names do not match! name=',name, 'CLASS1_NAME=',CLASS1_NAME
     total_error = total_error + 1
  ENDIF

  ! /* Check class name smaller buffer*/
  CALL H5Pget_class_name_f(cid1, name_small, size, error)
  CALL check("H5Pget_class_name", error, total_error)
  CALL VERIFY("H5Pget_class_name", size,7,error)
  CALL verifystring("H5Pget_class_name", name_small(1:4), CLASS1_NAME(1:4), error)
  IF(error.NE.0)THEN
     WRITE(*,*) 'Class names do not match! name=',name_small(1:4), 'CLASS1_NAME=',CLASS1_NAME(1:4)
     total_error = total_error + 1
  ENDIF

  ! /* Check class name bigger buffer*/
  CALL H5Pget_class_name_f(cid1, name_big, size, error)
  CALL check("H5Pget_class_name", error, total_error)
  CALL VERIFY("H5Pget_class_name", size,7,error)
  CALL verifystring("H5Pget_class_name", TRIM(name_big), TRIM(CLASS1_NAME), error)
  IF(error.NE.0)THEN
     WRITE(*,*) 'Class names do not match! name=',TRIM(name_small), 'CLASS1_NAME=',TRIM(CLASS1_NAME)
     total_error = total_error + 1
  ENDIF

  ! /* Check class parent */
  CALL H5Pget_class_parent_f(cid1, cid2, error)
  CALL check("H5Pget_class_parent_f", error, total_error)

  ! /* Verify class parent correct */
  CALL H5Pequal_f(cid2, H5P_ROOT_F, flag, error)
  CALL check("H5Pequal_f", error, total_error)
  CALL verifylogical("H5Pequal_f", flag, .TRUE., total_error)


  ! /* Make certain false postives aren't being returned */
  CALL H5Pequal_f(cid2, H5P_FILE_CREATE_F, flag, error)
  CALL check("H5Pequal_f", error, total_error)
  CALL verifylogical("H5Pequal_f", flag, .FALSE., total_error)

  !/* Close parent class */
  CALL H5Pclose_class_f(cid2, error)
  CALL check("H5Pclose_class_f", error, total_error)


  !/* Close class */
  CALL H5Pclose_class_f(cid1, error)
  CALL check("H5Pclose_class_f", error, total_error)

END SUBROUTINE test_genprop_basic_class

SUBROUTINE test_h5s_encode(cleanup, total_error)

!/****************************************************************
!**
!**  test_h5s_encode(): Test H5S (dataspace) encoding and decoding.
!**
!****************************************************************/

  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(INOUT) :: total_error

  INTEGER(hid_t) :: sid1, sid3!	/* Dataspace ID		*/
  INTEGER(hid_t) :: decoded_sid1, decoded_sid3
  INTEGER :: rank	!/* LOGICAL rank of dataspace	*/
  INTEGER(size_t) :: sbuf_size=0, scalar_size=0

! Make sure the size is large
  CHARACTER(LEN=288) :: sbuf
  CHARACTER(LEN=288) :: scalar_buf

  INTEGER(hsize_t) :: n ! /* Number of dataspace elements */

  INTEGER(hsize_t), DIMENSION(1:3) :: start = (/0, 0, 0/)
  INTEGER(hsize_t), DIMENSION(1:3) :: stride = (/2, 5, 3/)
  INTEGER(hsize_t), DIMENSION(1:3) :: count = (/2, 2, 2/)
  INTEGER(hsize_t), DIMENSION(1:3) :: BLOCK = (/1, 3, 1/)

  INTEGER :: space_type
  !
  ! Dataset dimensions
  !
  INTEGER, PARAMETER :: SPACE1_DIM1= 3,  SPACE1_DIM2=15, SPACE1_DIM3=13

  INTEGER(HSIZE_T), DIMENSION(1:3) :: dims1 = (/SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3/)
  INTEGER :: SPACE1_RANK = 3
  INTEGER :: error

  !/*-------------------------------------------------------------------------
  ! * Test encoding and decoding of simple dataspace and hyperslab selection.
  ! *-------------------------------------------------------------------------
  ! */

  CALL H5Screate_simple_f(SPACE1_RANK, dims1, sid1, error)
  CALL check("H5Screate_simple", error, total_error)

  CALL h5sselect_hyperslab_f(sid1, H5S_SELECT_SET_F, &
       start, count, error, stride=stride, BLOCK=BLOCK)
  CALL check("h5sselect_hyperslab_f", error, total_error)


  !/* Encode simple data space in a buffer */

  !         First find the buffer size
  CALL H5Sencode_f(sid1, sbuf, sbuf_size, error)
  CALL check("H5Sencode", error, total_error)


  ! /* Try decoding bogus buffer */

  CALL H5Sdecode_f(sbuf, decoded_sid1, error)
  CALL VERIFY("H5Sdecode", error, -1, total_error)

  CALL H5Sencode_f(sid1, sbuf, sbuf_size, error)
  CALL check("H5Sencode", error, total_error)

  ! /* Decode from the dataspace buffer and return an object handle */
  CALL H5Sdecode_f(sbuf, decoded_sid1, error)
  CALL check("H5Sdecode", error, total_error)


  ! /* Verify the decoded dataspace */
  CALL h5sget_simple_extent_npoints_f(decoded_sid1, n, error)
  CALL check("h5sget_simple_extent_npoints_f", error, total_error)
  CALL VERIFY("h5sget_simple_extent_npoints_f", INT(n), SPACE1_DIM1 * SPACE1_DIM2 * SPACE1_DIM3, &
       total_error)

  !
  !Close the dataspace for the dataset.
  !
  CALL h5sclose_f(sid1, error)
  CALL check("h5sclose_f", error, total_error)

  CALL h5sclose_f(decoded_sid1, error)
  CALL check("h5sclose_f", error, total_error)

  ! /*-------------------------------------------------------------------------
  !  * Test encoding and decoding of scalar dataspace.
  !  *-------------------------------------------------------------------------
  !  */
  ! /* Create scalar dataspace */

  CALL H5Screate_f(H5S_SCALAR_F, sid3, error)
  CALL check("H5Screate_f",error, total_error)

  ! /* Encode scalar data space in a buffer */

  !        First find the buffer size
  CALL H5Sencode_f(sid3, scalar_buf, scalar_size, error)
  CALL check("H5Sencode_f", error, total_error)

  ! encode

  CALL H5Sencode_f(sid3, scalar_buf, scalar_size, error)
  CALL check("H5Sencode_f", error, total_error)


  ! /* Decode from the dataspace buffer and return an object handle */

  CALL H5Sdecode_f(scalar_buf, decoded_sid3, error)
  CALL check("H5Sdecode_f", error, total_error)


  ! /* Verify extent type */

  CALL H5Sget_simple_extent_type_f(decoded_sid3, space_type, error)
  CALL check("H5Sget_simple_extent_type_f", error, total_error)
  CALL VERIFY("H5Sget_simple_extent_type_f", space_type, H5S_SCALAR_F, total_error)

  ! /* Verify decoded dataspace */
  CALL h5sget_simple_extent_npoints_f(decoded_sid3, n, error)
  CALL check("h5sget_simple_extent_npoints_f", error, total_error)
  CALL VERIFY("h5sget_simple_extent_npoints_f", INT(n), 1, total_error)

  CALL H5Sget_simple_extent_ndims_f(decoded_sid3, rank, error)
  CALL CHECK("H5Sget_simple_extent_ndims_f", error, total_error)
  CALL VERIFY("H5Sget_simple_extent_ndims_f", rank, 0, total_error )

  CALL h5sclose_f(sid3, error)
  CALL check("h5sclose_f", error, total_error)

  CALL h5sclose_f(decoded_sid3, error)
  CALL check("h5sclose_f", error, total_error)

END SUBROUTINE test_h5s_encode

!-------------------------------------------------------------------------
! Function:    test_scaleoffset
!
! Purpose:     Tests the integer datatype for scaleoffset filter
!              with fill value set
!
! Return:      Success:        0
!              Failure:        >0
!
! Programmer:  M. Scot Breitenfeld
!              Decemeber 11, 2010
!
! Modifications:
!
!-------------------------------------------------------------------------
!

SUBROUTINE test_scaleoffset(cleanup, total_error )

  USE HDF5

  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(INOUT) :: total_error
  INTEGER(hid_t) :: file

  INTEGER(hid_t)   :: dataset, datatype, space, mspace, dc
  INTEGER(hsize_t), DIMENSION(1:2) :: dims = (/2, 5/)
  INTEGER(hsize_t), DIMENSION(1:2) :: chunk_dim = (/2, 5/)
  INTEGER, DIMENSION(1:2,1:5) :: orig_data
  INTEGER, DIMENSION(1:2,1:5) :: new_data
  INTEGER(hsize_t), DIMENSION(1:2) :: start  ! Start of hyperslab 
  INTEGER(hsize_t), DIMENSION(1:2) :: stride ! Stride of hyperslab
  INTEGER(hsize_t), DIMENSION(1:2) :: count  ! BLOCK count
  INTEGER(hsize_t), DIMENSION(1:2) :: BLOCK  ! BLOCK sizes
  INTEGER :: fillval
  INTEGER(size_t) :: j
  REAL :: x
  INTEGER :: error
  LOGICAL :: status

  ! check to see if filter is available
  CALL H5Zfilter_avail_f(H5Z_FILTER_SCALEOFFSET_F, status, error)
  IF(.NOT.status)THEN ! We don't have H5Z_FILTER_SCALEOFFSET_F filter
     total_error = -1       ! so return
     RETURN
  ENDIF

  CALL H5Fcreate_f("h5scaleoffset.h5", H5F_ACC_TRUNC_F, file, error)
  CALL check("H5Fcreate_f", error, total_error)

  CALL H5Tcopy_f(H5T_NATIVE_INTEGER, datatype, error)
  CALL CHECK(" H5Tcopy_f", error, total_error)

  ! Set order of dataset datatype
  CALL H5Tset_order_f(datatype, H5T_ORDER_BE_F, error)
  CALL CHECK(" H5Tset_order_f", error, total_error)

  ! Create the data space for the dataset
  CALL H5Screate_simple_f(2, dims, space, error)
  CALL CHECK(" H5Screate_simple_f", error, total_error)

  ! Create the dataset property list  
  CALL H5Pcreate_f(H5P_DATASET_CREATE_F, dc, error)
  CALL CHECK(" H5Pcreate_f", error, total_error)

  ! Set fill value 
  fillval = 10000
  CALL H5Pset_fill_value_f(dc, H5T_NATIVE_INTEGER, fillval, error)
  CALL CHECK(" H5Pset_fill_value_f", error, total_error)

  ! Set up to use scaleoffset filter, let library calculate minbits
  CALL H5Pset_chunk_f(dc, 2, chunk_dim, error)
  CALL CHECK(" H5Pset_chunk_f", error, total_error)
  
  CALL H5Pset_scaleoffset_f(dc, H5Z_SO_INT_F, H5Z_SO_INT_MINBITS_DEFAULT_F, error)
  CALL CHECK(" H5Pset_scaleoffset_f", error, total_error)
  
  ! Create the dataset
  CALL H5Dcreate_f(file, "scaleoffset_int", datatype, &
       space, dataset, error, dc)
  CALL CHECK(" H5Dcreate_f", error, total_error)

  ! Create the memory data space
  CALL H5Screate_simple_f(2, dims, mspace, error)
  CALL CHECK(" H5Screate_simple_f", error, total_error)

  ! Select hyperslab for data to write, using 1x5 blocks,
  ! (1,1) stride and (1,1) count starting at the position (0,0)
     
  start(1:2) = (/0,0/)
  stride(1:2) = (/1,1/)
  COUNT(1:2) = (/1,1/)
  BLOCK(1:2) = (/1,5/)

  CALL H5Sselect_hyperslab_f(mspace, H5S_SELECT_SET_F, start, &
       count, error, stride, BLOCK)
  CALL CHECK(" H5Sselect_hyperslab_f", error, total_error)

  CALL RANDOM_SEED()
  ! Initialize data of hyperslab
  DO j = 1, dims(2)
     CALL RANDOM_NUMBER(x)
     orig_data(1,j) = INT(x*10000.)
     IF(MOD(j,2_size_t).EQ.0)THEN
        orig_data(1,j) = - orig_data(1,j)
     ENDIF
  ENDDO

  !----------------------------------------------------------------------
  ! STEP 1: Test scaleoffset by setting up a chunked dataset and writing
  ! to it.
  !----------------------------------------------------------------------
  
  ! Only data in the hyperslab will be written, other value should be fill value 
  CALL H5Dwrite_f(dataset, H5T_NATIVE_INTEGER, orig_data, dims, error, mspace, mspace, H5P_DEFAULT_F)
  CALL CHECK(" H5Dwrite_f", error, total_error)

  !----------------------------------------------------------------------
  ! STEP 2: Try to read the data we just wrote.
  !----------------------------------------------------------------------
     
  ! Read the dataset back
  
  CALL H5Dread_f(dataset, H5T_NATIVE_INTEGER, new_data, dims, error, mspace, mspace, H5P_DEFAULT_F)
  CALL CHECK(" H5Dread_f", error, total_error)

  ! Check that the values read are the same as the values written 
  DO j = 1, dims(2)
     IF(new_data(1,j) .NE. orig_data(1,j))THEN
        total_error = total_error + 1
        WRITE(*,'("    Read different values than written.")')
        WRITE(*,'("    At index ", 2(1X,I0))') 1, j
        EXIT
     ENDIF
  ENDDO
  !----------------------------------------------------------------------
  ! Cleanup
  !----------------------------------------------------------------------
  CALL H5Tclose_f(datatype, error)
  CALL CHECK(" H5Tclose_f", error, total_error)
  CALL H5Pclose_f(dc, error)
  CALL CHECK(" H5Pclose_f", error, total_error)
  CALL H5Sclose_f(space, error)
  CALL CHECK(" H5Sclose_f", error, total_error)
  CALL H5Dclose_f(dataset, error)
  CALL CHECK(" H5Dclose_f", error, total_error)
  CALL H5Fclose_f(file, error)
  CALL CHECK(" H5Fclose_f", error, total_error)

END SUBROUTINE test_scaleoffset
