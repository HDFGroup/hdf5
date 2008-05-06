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
SUBROUTINE test_h5o(cleanup, total_error)
  USE HDF5 ! This module contains all necessary modules 
  
  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(OUT) :: total_error

  ! /* Output message about test being performed */
  ! WRITE(*,*) "Testing Objects"

!!$  test_h5o_open();		/* Test generic OPEN FUNCTION */
!!$  test_h5o_open_by_addr();	/* Test opening objects by address */
!!$  test_h5o_close();		/* Test generic CLOSE FUNCTION */
!!$  test_h5o_refcount();        /* Test incrementing and decrementing reference count */
!!$  test_h5o_plist();           /* Test object creation properties */
  CALL test_h5o_link(total_error) ! /* Test object link routine */

END SUBROUTINE test_h5o

!/****************************************************************
!**
!**  test_h5o_link: Test creating link to object
!**
!****************************************************************/

SUBROUTINE test_h5o_link(total_error)

  USE HDF5 ! This module contains all necessary modules 
  
  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: total_error

  INTEGER(HID_T) :: file_id
  INTEGER(HID_T) :: group_id
  INTEGER(HID_T) :: space_id
  INTEGER(HID_T) :: dset_id
  INTEGER(HID_T) :: type_id
  INTEGER(HID_T) :: fapl_id
  INTEGER(HID_T) :: lcpl_id
  INTEGER(HID_T) :: mem_space_id, file_space_id, xfer_prp
  CHARACTER(LEN=8), PARAMETER :: TEST_FILENAME = 'TestFile'
  INTEGER, PARAMETER :: TEST6_DIM1 = 2, TEST6_DIM2 = 5
!EP  INTEGER(HSIZE_T), DIMENSION(1:2), PARAMETER :: dims = (/TEST6_DIM1,TEST6_DIM2/)
  INTEGER(HSIZE_T), DIMENSION(1:2)  :: dims = (/TEST6_DIM1,TEST6_DIM2/)
!EP  INTEGER, DIMENSION(1:TEST6_DIM1,1:TEST6_DIM2) :: wdata, rdata
  INTEGER, DIMENSION(TEST6_DIM1,TEST6_DIM2) :: wdata, rdata

  INTEGER, PARAMETER :: TRUE = 1, FALSE = 0

  LOGICAL :: committed ! /* Whether the named datatype is committed */

  INTEGER :: i, n, j
  INTEGER ::  error  ! /* Value returned from API calls */

  ! /* Initialize the raw data */
  DO i = 1, TEST6_DIM1
     DO j = 1, TEST6_DIM2
        wdata(i,j) = i*j
     ENDDO
  ENDDO
  
  ! /* Create the dataspace */
  CALL h5screate_simple_f(2, dims, space_id, error)
  CALL check("h5screate_simple_f",error,total_error)

  ! /* Create LCPL with intermediate group creation flag set */
  CALL H5Pcreate_f(H5P_LINK_CREATE_F, lcpl_id, error)
  CALL check("h5Pcreate_f",error,total_error)

  CALL H5Pset_create_inter_group_f(lcpl_id, TRUE, error)
  CALL check("H5Pset_create_inter_group_f",error,total_error)

  ! /* Loop over using new group format */
  ! for(new_format = FALSE; new_format <= TRUE; new_format++) {

  !/* Make a FAPL that uses the "use the latest version of the format" bounds */
  CALL H5Pcreate_f(H5P_FILE_ACCESS_F,fapl_id,error)
  CALL check("h5Pcreate_f",error,total_error)
  
  ! /* Set the "use the latest version of the format" bounds for creating objects in the file */
  
  CALL H5Pset_libver_bounds_f(fapl_id, H5F_LIBVER_LATEST_F, H5F_LIBVER_LATEST_F, error)
  CALL check("H5Pset_libver_bounds_f",error, total_error)
  
!!$ ret = H5Pset_libver_bounds(fapl_id, (new_format ? H5F_LIBVER_LATEST : H5F_LIBVER_EARLIEST), H5F_LIBVER_LATEST);
  
  ! /* Create a new HDF5 file */
  CALL H5Fcreate_f(TEST_FILENAME, H5F_ACC_TRUNC_F, file_id, error, H5P_DEFAULT_F, fapl_id)
  CALL check("H5Fcreate_f", error, total_error)

  ! /* Close the FAPL */
  CALL h5pclose_f(fapl_id, error)
  CALL check("h5pclose_f",error,total_error)
  
  ! /* Create and commit a datatype with no name */
  CALL H5Tcopy_f( H5T_NATIVE_INTEGER, type_id, error)
  CALL check("H5Tcopy",error,total_error)
  
  CALL H5Tcommit_anon_f(file_id, type_id, error) ! using no optional parameters
  CALL check("H5Tcommit_anon",error,total_error)

  CALL H5Tcommitted_f(type_id, committed, error)
  CALL check("H5Tcommitted_f",error,total_error)
  CALL verifyLogical("H5Tcommitted_f", committed, .TRUE., total_error)

  ! /* Create a dataset with no name using the committed datatype*/
  CALL H5Dcreate_anon_f(file_id, type_id, space_id, dset_id, error ) ! using no optional parameters
  CALL check("H5Dcreate_anon_f",error,total_error)


  ! /* Verify that we can write to and read from the dataset */
  
  ! /* Write the data to the dataset */

!EP  CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, wdata, dims, error, &
!EP         mem_space_id=H5S_ALL_F, file_space_id=H5S_ALL_F, xfer_prp = H5P_DEFAULT_F)
  CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, wdata, dims, error)
  CALL check("h5dwrite_f", error, total_error)

  ! /* Read the data back */
!EP  CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, rdata, dims, error, &
!EP       mem_space_id=H5S_ALL_F, file_space_id=H5S_ALL_F, xfer_prp = H5P_DEFAULT_F)
  CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, rdata, dims, error)
  CALL check("h5dread_f", error, total_error)
        
  ! /* Verify the data */
  DO i = 1, TEST6_DIM1
     DO j = 1, TEST6_DIM2
        CALL VERIFY("H5Dread_f",wdata(i,j),rdata(i,j),total_error)
        wdata(i,j) = i*j
     ENDDO
  ENDDO

  ! /* Create a group with no name*/

  CALL H5Gcreate_anon_f(file_id, group_id, error)
  CALL check("H5Gcreate_anon", error, total_error)

  ! /* Link nameless datatype into nameless group */
  CALL H5Olink_f(type_id, group_id, "datatype", error, H5P_DEFAULT_F)
  CALL check("H5Olink_f", error, total_error)

  ! /* Link nameless dataset into nameless group with intermediate group */
  CALL H5Olink_f(dset_id, group_id, "inter_group/dataset", error, lcpl_id, H5P_DEFAULT_F)
  CALL check("H5Olink_f", error, total_error)

  ! /* Close IDs for dataset and datatype */
  CALL h5dclose_f(dset_id, error)
  CALL check("h5dclose_f", error, total_error)
  CALL h5tclose_f(type_id, error)
  CALL check("h5tclose_f", error, total_error)


  ! /* Re-open datatype using new link */
  CALL H5Topen_f(group_id, "datatype", type_id, error)
  CALL check("h5topen_f", error, total_error)
  
  ! /* Link nameless group to root group and close the group ID*/
  CALL H5Olink_f(group_id, file_id, "/group", error)
  CALL check("H5Olink_f", error, total_error)
  

  CALL h5gclose_f(group_id, error)
  CALL check("h5gclose_f",error,total_error)

  ! /* Open dataset through root group and verify its data */
  
  CALL H5Dopen_f(file_id, "/group/inter_group/dataset", dset_id, error)
  CALL check("test_lcpl.h5dopen_f", error, total_error)

  ! /* Read data from dataset */
!EP  CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, rdata, dims, error, &
!EP       H5S_ALL_F, H5S_ALL_F, xfer_prp = H5P_DEFAULT_F)
  CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, rdata, dims, error)
  CALL check("h5dread_f", error, total_error)

  ! /* Verify the data */
  DO i = 1, TEST6_DIM1
     DO j = 1, TEST6_DIM2
        CALL VERIFY("H5Dread",wdata(i,j),rdata(i,j),total_error)
     ENDDO
  ENDDO
  ! /* Close open IDs */

  CALL h5dclose_f(dset_id, error)
  CALL check("h5dclose_f",error,total_error)
  CALL h5tclose_f(type_id, error)
  CALL check("h5tclose_f",error,total_error)

  CALL h5fclose_f(file_id, error)
  CALL check("h5fclose_f",error,total_error)

  ! /* Close remaining IDs */
  CALL h5sclose_f(space_id, error)
  CALL check("h5sclose_f",error,total_error)
  CALL h5pclose_f(lcpl_id,error)
  CALL check("h5pclose_f", error, total_error)

END SUBROUTINE test_h5o_link
