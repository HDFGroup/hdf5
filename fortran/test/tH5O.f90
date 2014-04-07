!****h* root/fortran/test/tH5O.f90
!
! NAME
!  tH5O.f90
!
! FUNCTION
!  Basic testing of Fortran H5O APIs.
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
! CONTAINS SUBROUTINES
!  test_h5o, test_h5o_link, test_h5o_plist
!
!*****
MODULE TH5O

CONTAINS

SUBROUTINE test_h5o(cleanup, total_error)
  USE HDF5 ! This module contains all necessary modules
  USE TH5_MISC

  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(INOUT) :: total_error
  INTEGER :: error

  CALL test_h5o_plist(total_error) ! Test object creation properties
  CALL test_h5o_link(total_error) ! Test object link routine

  IF(cleanup) CALL h5_cleanup_f("TestFile", H5P_DEFAULT_F, error)
  CALL check("h5_cleanup_f", error, total_error)
  IF(cleanup) CALL h5_cleanup_f("test", H5P_DEFAULT_F, error)
  CALL check("h5_cleanup_f", error, total_error)

END SUBROUTINE test_h5o

!/****************************************************************
!**
!**  test_h5o_link: Test creating link to object
!**
!****************************************************************/

SUBROUTINE test_h5o_link(total_error)

  USE HDF5 ! This module contains all necessary modules
  USE TH5_MISC

  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: total_error

  INTEGER(HID_T) :: file_id
  INTEGER(HID_T) :: group_id
  INTEGER(HID_T) :: space_id
  INTEGER(HID_T) :: dset_id
  INTEGER(HID_T) :: type_id
  INTEGER(HID_T) :: fapl_id
  INTEGER(HID_T) :: lcpl_id
  INTEGER(HID_T) :: ocpypl_id
  CHARACTER(LEN=11), PARAMETER :: TEST_FILENAME = 'TestFile.h5'
  INTEGER, PARAMETER :: TEST6_DIM1 = 2, TEST6_DIM2 = 5
!EP  INTEGER(HSIZE_T), DIMENSION(1:2), PARAMETER :: dims = (/TEST6_DIM1,TEST6_DIM2/)
  INTEGER(HSIZE_T), DIMENSION(1:2)  :: dims = (/TEST6_DIM1,TEST6_DIM2/)
!EP  INTEGER, DIMENSION(1:TEST6_DIM1,1:TEST6_DIM2) :: wdata, rdata
  INTEGER, DIMENSION(TEST6_DIM1,TEST6_DIM2) :: wdata, rdata

  INTEGER, PARAMETER :: TRUE = 1

  LOGICAL :: committed ! /* Whether the named datatype is committed 

  INTEGER :: i, j
  INTEGER ::  error  ! /* Value returned from API calls 

  CHARACTER(LEN=14) :: NAME_DATATYPE_SIMPLE="H5T_NATIVE_INT"
  CHARACTER(LEN=16) :: NAME_DATATYPE_SIMPLE2="H5T_NATIVE_INT-2"
  INTEGER(HID_T) :: tid, tid2
  LOGICAL :: flag
  
  ! Data for tested h5ocopy_f
  CHARACTER(LEN=3) , PARAMETER :: dataset = "DS1"
  INTEGER          , PARAMETER :: dim0     = 4

  INTEGER(HSIZE_T), DIMENSION(1:1)    :: dims2 = (/dim0/) ! size read/write buffer
  INTEGER         , DIMENSION(1:dim0) :: wdata2   ! Write buffer 
  LOGICAL :: link_exists
  CHARACTER(LEN=8)  :: chr_exact
  CHARACTER(LEN=10) :: chr_lg
  INTEGER(size_t) :: nlinks
  INTEGER(HID_T) :: plist = -1

  CHARACTER(LEN=20) :: dset_comment = "dataset comment"
  CHARACTER(LEN=13) :: grp_comment = "group comment"
  CHARACTER(LEN=10) :: comment_sm ! to small comment sized buffer
  CHARACTER(LEN=15) :: comment ! exact comment sized buffer
  CHARACTER(LEN=20) :: comment_lg ! large comment sized buffer
  INTEGER(HSSIZE_T) :: comment_size
  INTEGER(SIZE_T)   :: comment_size2

  !  Initialize the raw data 
  DO i = 1, TEST6_DIM1
     DO j = 1, TEST6_DIM2
        wdata(i,j) = i*j
     ENDDO
  ENDDO

  !  Create the dataspace 
  CALL h5screate_simple_f(2, dims, space_id, error)
  CALL check("h5screate_simple_f",error,total_error)

  !  Create LCPL with intermediate group creation flag set 
  CALL H5Pcreate_f(H5P_LINK_CREATE_F, lcpl_id, error)
  CALL check("h5Pcreate_f",error,total_error)

  CALL H5Pset_create_inter_group_f(lcpl_id, TRUE, error)
  CALL check("H5Pset_create_inter_group_f",error,total_error)

  !  Loop over using new group format 
  ! for(new_format = FALSE; new_format <= TRUE; new_format++) {

  ! Make a FAPL that uses the "use the latest version of the format" bounds 
  CALL H5Pcreate_f(H5P_FILE_ACCESS_F,fapl_id,error)
  CALL check("h5Pcreate_f",error,total_error)

  !  Set the "use the latest version of the format" bounds for creating objects in the file 

  CALL H5Pset_libver_bounds_f(fapl_id, H5F_LIBVER_LATEST_F, H5F_LIBVER_LATEST_F, error)
  CALL check("H5Pset_libver_bounds_f",error, total_error)

  !  Create a new HDF5 file 
  CALL H5Fcreate_f(TEST_FILENAME, H5F_ACC_TRUNC_F, file_id, error, H5P_DEFAULT_F, fapl_id)
  CALL check("H5Fcreate_f", error, total_error)

  !  Close the FAPL 
  CALL h5pclose_f(fapl_id, error)
  CALL check("h5pclose_f",error,total_error)

  !  Create and commit a datatype with no name 
  CALL H5Tcopy_f( H5T_NATIVE_INTEGER, type_id, error)
  CALL check("H5Tcopy_F",error,total_error)

  CALL H5Tcommit_anon_f(file_id, type_id, error) ! using no optional parameters
  CALL check("H5Tcommit_anon_F",error,total_error)

  CALL H5Tcommitted_f(type_id, committed, error)
  CALL check("H5Tcommitted_f",error,total_error)
  CALL verifyLogical("H5Tcommitted_f", committed, .TRUE., total_error)

  !  Create a dataset with no name using the committed datatype
  CALL H5Dcreate_anon_f(file_id, type_id, space_id, dset_id, error ) ! using no optional parameters
  CALL check("H5Dcreate_anon_f",error,total_error)
  !
  !  Verify that we can write to and read from the dataset 
  !
  !  Write the data to the dataset 

!EP  CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, wdata, dims, error, &
!EP         mem_space_id=H5S_ALL_F, file_space_id=H5S_ALL_F, xfer_prp = H5P_DEFAULT_F)
  CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, wdata, dims, error)
  CALL check("h5dwrite_f", error, total_error)

  !  Read the data back 
!EP  CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, rdata, dims, error, &
!EP       mem_space_id=H5S_ALL_F, file_space_id=H5S_ALL_F, xfer_prp = H5P_DEFAULT_F)
  CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, rdata, dims, error)
  CALL check("h5dread_f", error, total_error)

  !  Verify the data 
  DO i = 1, TEST6_DIM1
     DO j = 1, TEST6_DIM2
        CALL VERIFY("H5Dread_f",wdata(i,j),rdata(i,j),total_error)
        wdata(i,j) = i*j
     ENDDO
  ENDDO

  !  Create a group with no name

  CALL H5Gcreate_anon_f(file_id, group_id, error)
  CALL check("H5Gcreate_anon", error, total_error)

  !  Link nameless datatype into nameless group 
  CALL H5Olink_f(type_id, group_id, "datatype", error, H5P_DEFAULT_F)
  CALL check("H5Olink_f", error, total_error)

  !  Link nameless dataset into nameless group with intermediate group 
  CALL H5Olink_f(dset_id, group_id, "inter_group/dataset", error, lcpl_id, H5P_DEFAULT_F)
  CALL check("H5Olink_f", error, total_error)

  !  Close IDs for dataset and datatype 
  CALL h5dclose_f(dset_id, error)
  CALL check("h5dclose_f", error, total_error)
  CALL h5tclose_f(type_id, error)
  CALL check("h5tclose_f", error, total_error)

  !  Re-open datatype using new link 
  CALL H5Topen_f(group_id, "datatype", type_id, error)
  CALL check("h5topen_f", error, total_error)

  !  Link nameless group to root group and close the group ID
  CALL H5Olink_f(group_id, file_id, "/group", error)
  CALL check("H5Olink_f", error, total_error)

  CALL h5gclose_f(group_id, error)
  CALL check("h5gclose_f",error,total_error)

  !  Open dataset through root group and verify its data 
  CALL H5Dopen_f(file_id, "/group/inter_group/dataset", dset_id, error)
  CALL check("test_lcpl.h5dopen_f", error, total_error)

  !  Read data from dataset 
!EP  CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, rdata, dims, error, &
!EP       H5S_ALL_F, H5S_ALL_F, xfer_prp = H5P_DEFAULT_F)
  CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, rdata, dims, error)
  CALL check("h5dread_f", error, total_error)

  !  Verify the data 
  DO i = 1, TEST6_DIM1
     DO j = 1, TEST6_DIM2
        CALL VERIFY("H5Dread",wdata(i,j),rdata(i,j),total_error)
     ENDDO
  ENDDO
  !  Close open IDs 

  CALL h5dclose_f(dset_id, error)
  CALL check("h5dclose_f",error,total_error)
  CALL h5tclose_f(type_id, error)
  CALL check("h5tclose_f",error,total_error)

  !  Close remaining IDs 
  CALL h5sclose_f(space_id, error)
  CALL check("h5sclose_f",error,total_error)
  CALL h5pclose_f(lcpl_id,error)
  CALL check("h5pclose_f", error, total_error)

  ! *********************
  !    CHECK H5OCOPY_F
  ! *********************

  DO i = 1, dim0
     wdata2(i) = i-1
  ENDDO
  !
  ! Create dataspace.  Setting size to be the current size.
  !
  CALL h5screate_simple_f(1, dims2, space_id, error)
  CALL check("h5screate_simple_f", error, total_error)
  !
  ! Create intermediate groups
  !
  CALL h5gcreate_f(file_id,"/G1",group_id,error)
  CALL check("h5gcreate_f", error, total_error)
  CALL h5gcreate_f(file_id,"/G1/G2",group_id,error)
  CALL check("h5gcreate_f", error, total_error)
  CALL h5gcreate_f(file_id,"/G1/G2/G3",group_id,error)
  CALL check("h5gcreate_f", error, total_error)

  ! Try putting a comment on the group /G1/G2/G3 by name
  CALL h5oset_comment_by_name_f(file_id, "/G1/G2/G3", grp_comment, error)
  CALL check("h5oset_comment_by_name_f", error, total_error)

  comment_lg = ' '

  CALL h5oget_comment_by_name_f(file_id, "/G1/G2/G3", comment_lg, error) 
  CALL check("h5oget_comment_by_name_f", error, total_error)

  IF(comment_lg(1:13).NE.grp_comment)THEN
     CALL check("h5oget_comment_by_name_f", -1, total_error)
  ENDIF
  IF(comment_lg(14:20).NE.'       ')THEN ! make sure no NULL terminator
     CALL check("h5oget_comment_by_name_f", -1, total_error)
  ENDIF

  ! Try putting a comment on the group /G1/G2/G3 by name with trailing blanks

  CALL h5oset_comment_by_name_f(file_id, "/G1/G2/G3"//'  ', grp_comment, error)
  CALL check("h5oset_comment_by_name_f", error, total_error)

  comment_lg = ' '

  CALL h5oget_comment_by_name_f(file_id, "/G1/G2/G3"//'  ', comment_lg, error) 
  CALL check("h5oget_comment_by_name_f", error, total_error)

  IF(comment_lg(1:13).NE.grp_comment)THEN
     CALL check("h5oget_comment_by_name_f", -1, total_error)
  ENDIF
  IF(comment_lg(14:20).NE.'       ')THEN ! make sure no NULL terminator
     CALL check("h5oget_comment_by_name_f", -1, total_error)
  ENDIF

  !
  ! Create the dataset
  !
  CALL h5dcreate_f(group_id, dataset, H5T_STD_I32LE, space_id, dset_id, error)
  CALL check("h5dcreate_f", error, total_error)

  ! Putting a comment on the dataset
  CALL h5oset_comment_f(dset_id, dset_comment, error)
  CALL check("h5oset_comment_f", error, total_error)

  ! Try reading into a buffer that is the correct size

  CALL h5oget_comment_f(dset_id, comment, error) 
  CALL check("h5oget_comment_f", error, total_error)

  IF(comment(1:15).NE.dset_comment(1:15))THEN
     CALL check("h5oget_comment_f", -1, total_error)
  ENDIF

  ! Try reading into a buffer that is to small

  CALL h5oget_comment_f(dset_id, comment_sm, error) 
  CALL check("h5oget_comment_f", error, total_error)

  IF(comment_sm(1:10).NE.dset_comment(1:10))THEN
     CALL check("h5oget_comment_f", -1, total_error)
  ENDIF 

 ! Try reading into a buffer that is larger then needed

  comment_lg = ' '

  CALL h5oget_comment_f(dset_id, comment_lg, error) 
  CALL check("h5oget_comment_f", error, total_error)

  IF(comment_lg(1:15).NE.dset_comment)THEN
     CALL check("h5oget_comment_f", -1, total_error)
  ENDIF
  IF(comment_lg(16:20).NE.'     ')THEN ! make sure no NULL terminator
     CALL check("h5oget_comment_f", -1, total_error)
  ENDIF
  !
  ! Check optional parameter
  !
  CALL h5oget_comment_f(dset_id, comment_lg, error, comment_size) 
  CALL check("h5oget_comment_f", error, total_error)

  IF( comment_size.NE.15)THEN
     CALL check("h5oget_comment_f", -1, total_error)
  ENDIF

  ! CHECK h5oget_comment_by_name_f

  ! Try reading into a buffer that is the correct size

  CALL h5oget_comment_by_name_f(dset_id, ".", comment, error) 
  CALL check("h5oget_comment_by_name_f", error, total_error)

  IF(comment(1:15).NE.dset_comment(1:15))THEN
     CALL check("h5oget_comment_by_name_f", -1, total_error)
  ENDIF

  ! Try with trailing blanks in the name

  CALL h5oget_comment_by_name_f(dset_id, ".     ", comment, error) 
  CALL check("h5oget_comment_by_name_f", error, total_error)

  IF(comment(1:15).NE.dset_comment(1:15))THEN
     CALL check("h5oget_comment_by_name_f", -1, total_error)
  ENDIF

  !
  ! Check optional parameter
  !
  CALL h5oget_comment_by_name_f(dset_id, ".     ", comment_lg, error, comment_size2) 
  CALL check("h5oget_comment_by_name_f", error, total_error)

  IF( comment_size2.NE.15)THEN
     CALL check("h5oget_comment_by_name_f", -1, total_error)
  ENDIF

  !
  ! Write the data to the dataset.
  !
  CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, wdata2, dims2, error)
  CALL check("h5dwrite_f", error, total_error)

  ! *************************
  ! CHECK H5OEXISTS_BY_NAME_F
  ! *************************

  ! Create a soft link to /G1
  CALL h5lcreate_soft_f("/G1", file_id, "/G1_LINK", error)
  CALL check("h5lcreate_soft_f", error, total_error)


  ! Create a soft link to /G1000, does not exist
  CALL h5lcreate_soft_f("/G1000", file_id, "/G1_FALSE", error)
  CALL check("h5lcreate_soft_f", error, total_error)

  ! Create a soft link to /G1_LINK
  CALL h5lcreate_soft_f("/G1_FALSE", file_id, "/G2_FALSE", error)
  CALL check("h5lcreate_soft_f", error, total_error)

  ! See if the link exists
  CALL h5oexists_by_name_f(file_id,"/G1_LINK", link_exists, error)
  CALL check("h5oexists_by_name_f", error, total_error)

  ! Object should exist
  IF(.NOT.link_exists)THEN
     CALL check("h5oexists_by_name_f", -1, total_error)
  ENDIF

  chr_exact = "/G1_LINK"
  ! See if the link exists
  CALL h5oexists_by_name_f(file_id,chr_exact, link_exists, error, H5P_DEFAULT_F)
  CALL check("h5oexists_by_name_f", error, total_error)

  ! Object should exist
  IF(.NOT.link_exists)THEN
     CALL check("h5oexists_by_name_f", -1, total_error)
  ENDIF

  chr_lg = "/G1_LINK"
  ! See if the link exists
  CALL h5oexists_by_name_f(file_id,chr_lg, link_exists, error, H5P_DEFAULT_F)
  CALL check("h5oexists_by_name_f", error, total_error)

  ! Object should exist
  IF(.NOT.link_exists)THEN
     CALL check("h5oexists_by_name_f", -1, total_error)
  ENDIF

  chr_lg = "/G1_LINK  "
  ! See if the link exists
  CALL h5oexists_by_name_f(file_id,chr_lg, link_exists, error, H5P_DEFAULT_F)
  CALL check("h5oexists_by_name_f", error, total_error)

  ! Object should exist
  IF(.NOT.link_exists)THEN
     CALL check("h5oexists_by_name_f", -1, total_error)
  ENDIF

  ! See if the link exists
  CALL h5oexists_by_name_f(file_id,"/G1_FALSE", link_exists, error)
  CALL check("h5oexists_by_name_f", error, total_error)

  ! Object should not exist
  IF(link_exists)THEN
     CALL check("h5oexists_by_name_f", -1, total_error)
  ENDIF

  ! Check optional parameter

  CALL h5pcreate_f(H5P_LINK_ACCESS_F,plist,error)
  CALL check("h5pcreate_f",error,total_error)

  nlinks = 2
  CALL h5pset_nlinks_f(plist, nlinks, error)
  CALL check("h5pset_nlinks_f", error, total_error)
  ! Ensure that nlinks was set successfully
  nlinks = 0
  CALL h5pget_nlinks_f(plist, nlinks, error)
  CALL check("h5pget_nlinks_f",error,total_error)
  CALL VERIFY("h5pget_nlinks_f", INT(nlinks), 2, total_error)

  ! See if the link exists
  CALL h5oexists_by_name_f(file_id,"/G1_LINK", link_exists, error, plist)
  CALL check("h5oexists_by_name_f", error, total_error)

  ! Object should exist
  IF(.not.link_exists)THEN
     CALL check("h5oexists_by_name_f", -1, total_error)
  ENDIF
  !
  ! Close and release resources.
  !
  CALL h5dclose_f(dset_id , error)
  CALL check(" h5dclose_f", error, total_error)
  CALL h5sclose_f(space_id, error)
  CALL check("h5sclose_f", error, total_error)
  CALL h5gclose_f(group_id, error)
  CALL check("h5gclose_f", error, total_error)

  ! Test opening an object by index, note 
  CALL h5oopen_by_idx_f(file_id, "/G1/G2/G3", H5_INDEX_NAME_F, H5_ITER_INC_F, 0_hsize_t, group_id, error)
  CALL check("h5oopen_by_idx_f", error, total_error)

  CALL h5oclose_f(group_id, error)
  CALL check("h5gclose_f", error, total_error)

  !
  ! create property to pass copy options
  !
  CALL h5pcreate_f(H5P_LINK_CREATE_F, lcpl_id, error)
  CALL check("h5Pcreate_f", error, total_error)

  CALL h5pset_create_inter_group_f(lcpl_id, TRUE, error)
  CALL check("H5Pset_create_inter_group_f", error, total_error)
  !
  ! Check optional parameter lcpl_id, this would fail if lcpl_id was not specified
  !
  CALL h5ocopy_f(file_id, "/G1/G2/G3/DS1", file_id, "/G1/G_cp1/DS2", error, lcpl_id=lcpl_id)
  CALL check("h5ocopy_f -- W/ OPTION: lcpl_id", error ,total_error)

  CALL h5pclose_f(lcpl_id, error)
  CALL check("h5pclose_f",error,total_error)

  CALL h5pcreate_f(H5P_OBJECT_COPY_F, ocpypl_id, error)
  CALL check("h5Pcreate_f",error,total_error)

  CALL h5pset_copy_object_f(ocpypl_id, H5O_COPY_SHALLOW_HIERARCHY_F, error) 
  CALL check("H5Pset_copy_object_f",error,total_error)

  CALL h5ocopy_f(file_id, "/G1/G2", file_id, "/G1/G_cp2", error, ocpypl_id=ocpypl_id) 
  CALL check("h5ocopy_f",error,total_error)

  ! Makes sure the "DS1" dataset was not copied since we set a 
  ! flag to copy only immediate members of a group.
  ! Therefore, this should fail.
  CALL h5dopen_f(file_id, "/G1/G_cp2/DS1", dset_id, error)
  IF(error.EQ.0)THEN
     CALL check("h5ocopy_f -- W/ OPTION: ocpypl_id", -1, total_error)
  ENDIF

  CALL h5pclose_f(ocpypl_id, error)
  CALL check("h5pclose_f",error,total_error)

  ! create datatype
  CALL h5tcopy_f(H5T_NATIVE_INTEGER, tid, error)
  CALL check("h5tcopy_f", error, total_error)

  ! create named datatype
  CALL h5tcommit_f(file_id, NAME_DATATYPE_SIMPLE, tid, error)
  CALL check("h5tcommit_f", error, total_error)

  ! close the datatype
  CALL h5tclose_f(tid, error)
  CALL check("h5tclose_f",error, total_error)

  CALL h5ocopy_f(file_id, NAME_DATATYPE_SIMPLE, file_id, NAME_DATATYPE_SIMPLE2, error)
  CALL check("h5ocopy_f",error,total_error)

  ! open the datatype for copy
  CALL h5topen_f(file_id, NAME_DATATYPE_SIMPLE, tid, error)
  CALL check("h5topen_f",error,total_error)

  ! open the copied datatype
  CALL h5topen_f(file_id, NAME_DATATYPE_SIMPLE2, tid2, error)
  CALL check("h5topen_f",error,total_error)

  ! Compare the datatypes
  CALL h5tequal_f(tid, tid2, flag, error)
  IF(.NOT.flag)THEN
     CALL check("h5ocopy_f FAILED", -1, total_error)
  ENDIF

  ! close the destination datatype
  CALL h5tclose_f(tid, error)
  CALL check("h5tclose_f",error,total_error)

  ! close the destination datatype
  CALL h5tclose_f(tid2, error)
  CALL check("h5tclose_f",error,total_error)

  CALL h5fclose_f(file_id, error)
  CALL check("h5fclose_f",error,total_error)

END SUBROUTINE test_h5o_link

!***************************************************************
!**
!**  test_h5o_plist(): Test object creation properties
!**
!***************************************************************

SUBROUTINE test_h5o_plist(total_error)

  USE HDF5 ! This module contains all necessary modules
  USE TH5_MISC

  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: total_error

  INTEGER(hid_t) :: fid                        ! HDF5 File ID      
  INTEGER(hid_t) :: grp, dset, dtype, dspace   ! Object identifiers 
  INTEGER(hid_t) :: fapl                       ! File access property list 
  INTEGER(hid_t) :: gcpl, dcpl, tcpl           ! Object creation properties 
  INTEGER :: def_max_compact, def_min_dense    ! Default phase change parameters 
  INTEGER :: max_compact, min_dense            ! Actual phase change parameters 
  INTEGER :: error                             ! Value returned from API calls 
  CHARACTER(LEN=7), PARAMETER :: TEST_FILENAME = 'test.h5'


!  PRINT*,'Testing object creation properties'

  ! Make a FAPL that uses the "use the latest version of the format" flag 
  CALL H5Pcreate_f(H5P_FILE_ACCESS_F, fapl, error)
  CALL check("H5Pcreate_f", error, total_error)

  !  Set the "use the latest version of the format" bounds for creating objects in the file 
  CALL H5Pset_libver_bounds_f(fapl, H5F_LIBVER_LATEST_F, H5F_LIBVER_LATEST_F, error)
  CALL check("H5Pcreate_f", error, total_error)

  !  Create a new HDF5 file 
  CALL H5Fcreate_f(TEST_FILENAME, H5F_ACC_TRUNC_F, fid, error, access_prp=fapl)
  CALL check("H5Fcreate_f", error, total_error)

  !  Create group, dataset & named datatype creation property lists 
  CALL H5Pcreate_f(H5P_GROUP_CREATE_F, gcpl, error)
  CALL check("H5Pcreate_f", error, total_error)
  CALL H5Pcreate_f(H5P_DATASET_CREATE_F, dcpl, error)
  CALL check("H5Pcreate_f", error, total_error)
  CALL H5Pcreate_f(H5P_DATATYPE_CREATE_F, tcpl, error)
  CALL check("H5Pcreate_f", error, total_error)

  !  Retrieve default attribute phase change values 
  CALL H5Pget_attr_phase_change_f(gcpl, def_max_compact, def_min_dense, error)
  CALL check("H5Pget_attr_phase_change_f", error, total_error)

  !  Set non-default attribute phase change values on each creation property list 
  CALL H5Pset_attr_phase_change_f(gcpl, def_max_compact+1, def_min_dense-1, error)
  CALL check("H5Pget_attr_phase_change_f", error, total_error)
  CALL H5Pset_attr_phase_change_f(dcpl, def_max_compact+1, def_min_dense-1, error)
  CALL check("H5Pget_attr_phase_change_f", error, total_error)
  CALL H5Pset_attr_phase_change_f(tcpl, def_max_compact+1, def_min_dense-1, error)
  CALL check("H5Pget_attr_phase_change_f", error, total_error)

  !  Retrieve attribute phase change values on each creation property list and verify 
  CALL H5Pget_attr_phase_change_f(gcpl, max_compact, min_dense, error)
  CALL check("H5Pget_attr_phase_change_f", error, total_error)
  CALL VERIFY("H5Pget_attr_phase_change_f", max_compact, (def_max_compact + 1), total_error)
  CALL VERIFY("H5Pget_attr_phase_change_f", min_dense, (def_min_dense - 1), total_error)

  CALL H5Pget_attr_phase_change_f(dcpl, max_compact, min_dense, error)
  CALL check("H5Pget_attr_phase_change_f", error, total_error)
  CALL VERIFY("H5Pget_attr_phase_change_f", max_compact, (def_max_compact + 1), total_error)
  CALL VERIFY("H5Pget_attr_phase_change_f", min_dense, (def_min_dense - 1), total_error)

  CALL H5Pget_attr_phase_change_f(tcpl, max_compact, min_dense, error)
  CALL check("H5Pget_attr_phase_change_f", error, total_error)
  CALL VERIFY("H5Pget_attr_phase_change_f", max_compact, (def_max_compact + 1), total_error)
  CALL VERIFY("H5Pget_attr_phase_change_f", min_dense, (def_min_dense - 1), total_error)

  ! Create a group, dataset, and committed datatype within the file,
  ! using the respective type of creation property lists.
  ! 

  ! Create the group anonymously and link it in 
  CALL H5Gcreate_anon_f(fid, grp, error, gcpl_id=gcpl)
  CALL check("H5Gcreate_anon_f", error, total_error)

  CALL H5Olink_f(grp, fid, "group", error)
  CALL check("H5Olink_f", error, total_error)

  !  Commit the type inside the group anonymously and link it in 
  CALL h5tcopy_f(H5T_NATIVE_INTEGER, dtype, error)
  CALL check("h5tcopy_f", error, total_error)

  CALL H5Tcommit_anon_f(fid, dtype, error, tcpl_id=tcpl)
  CALL check("H5Tcommit_anon_f",error,total_error)

  CALL H5Olink_f(dtype, fid, "datatype", error)
  CALL check("H5Olink_f", error, total_error)

  !  Create the dataspace for the dataset. 
  CALL h5screate_f(H5S_SCALAR_F, dspace, error)
  CALL check("h5screate_f",error,total_error)

  !  Create the dataset anonymously and link it in 
  CALL H5Dcreate_anon_f(fid, H5T_NATIVE_INTEGER, dspace, dset, error, dcpl )
  CALL check("H5Dcreate_anon_f",error,total_error)

  CALL H5Olink_f(dset, fid, "dataset", error)
  CALL check("H5Olink_f", error, total_error)

  CALL h5sclose_f(dspace, error)
  CALL check("h5sclose_f",error,total_error)

  !  Close current creation property lists 
  CALL h5pclose_f(gcpl,error)
  CALL check("h5pclose_f", error, total_error)
  CALL h5pclose_f(dcpl,error)
  CALL check("h5pclose_f", error, total_error)
  CALL h5pclose_f(tcpl,error)
  CALL check("h5pclose_f", error, total_error)

  !  Retrieve each object's creation property list 
  CALL H5Gget_create_plist_f(grp, gcpl, error)
  CALL check("H5Gget_create_plist", error, total_error)

  CALL H5Tget_create_plist_f(dtype, tcpl, error)
  CALL check("H5Tget_create_plist_f", error, total_error)

  CALL H5Dget_create_plist_f(dset, dcpl, error)
  CALL check("H5Dget_create_plist_f", error, total_error)

  !  Retrieve attribute phase change values on each creation property list and verify 
  CALL H5Pget_attr_phase_change_f(gcpl, max_compact, min_dense, error)
  CALL check("H5Pget_attr_phase_change_f", error, total_error)
  CALL VERIFY("H5Pget_attr_phase_change_f", max_compact, (def_max_compact + 1), total_error)
  CALL VERIFY("H5Pget_attr_phase_change_f", min_dense, (def_min_dense - 1), total_error)

  CALL H5Pget_attr_phase_change_f(dcpl, max_compact, min_dense, error)
  CALL check("H5Pget_attr_phase_change_f", error, total_error)
  CALL VERIFY("H5Pget_attr_phase_change_f", max_compact, (def_max_compact + 1), total_error)
  CALL VERIFY("H5Pget_attr_phase_change_f", min_dense, (def_min_dense - 1), total_error)

  CALL H5Pget_attr_phase_change_f(tcpl, max_compact, min_dense, error)
  CALL check("H5Pget_attr_phase_change_f", error, total_error)
  CALL VERIFY("H5Pget_attr_phase_change_f", max_compact, (def_max_compact + 1), total_error)
  CALL VERIFY("H5Pget_attr_phase_change_f", min_dense, (def_min_dense - 1), total_error)

  ! Close current objects 
  CALL h5pclose_f(gcpl,error)
  CALL check("h5pclose_f", error, total_error)
  CALL h5pclose_f(dcpl,error)
  CALL check("h5pclose_f", error, total_error)
  CALL h5pclose_f(tcpl,error)
  CALL check("h5pclose_f", error, total_error)

  CALL h5gclose_f(grp, error)
  CALL check("h5gclose_f",error,total_error)

  CALL h5tclose_f(dtype, error)
  CALL check("h5tclose_f",error,total_error)
  CALL h5dclose_f(dset, error)
  CALL check("h5dclose_f",error,total_error)
  CALL h5fclose_f(fid, error)
  CALL check("h5fclose_f",error,total_error)
  !  Re-open the file and check that the object creation properties persist 
  CALL h5fopen_f(TEST_FILENAME, H5F_ACC_RDONLY_F, fid, error, access_prp=fapl)
  CALL check("H5fopen_f",error,total_error)

  !  Re-open objects 
  CALL H5Gopen_f(fid, "group", grp, error)
  CALL check("h5gopen_f", error, total_error)

  CALL H5Topen_f(fid, "datatype", dtype,error)
  CALL check("h5topen_f", error, total_error)

  CALL H5Dopen_f(fid, "dataset", dset, error)
  CALL check("h5dopen_f", error, total_error)

  !  Retrieve each object's creation property list 
  CALL H5Gget_create_plist_f(grp, gcpl, error)
  CALL check("H5Gget_create_plist", error, total_error)

  CALL H5Tget_create_plist_f(dtype, tcpl, error)
  CALL check("H5Tget_create_plist_f", error, total_error)

  CALL H5Dget_create_plist_f(dset, dcpl, error)
  CALL check("H5Dget_create_plist_f", error, total_error)

  !  Retrieve attribute phase change values on each creation property list and verify 
  CALL H5Pget_attr_phase_change_f(gcpl, max_compact, min_dense, error)
  CALL check("H5Pget_attr_phase_change_f", error, total_error)
  CALL VERIFY("H5Pget_attr_phase_change_f", max_compact, (def_max_compact + 1), total_error)
  CALL VERIFY("H5Pget_attr_phase_change_f", min_dense, (def_min_dense - 1), total_error)

  CALL H5Pget_attr_phase_change_f(dcpl, max_compact, min_dense, error)
  CALL check("H5Pget_attr_phase_change_f", error, total_error)
  CALL VERIFY("H5Pget_attr_phase_change_f", max_compact, (def_max_compact + 1), total_error)
  CALL VERIFY("H5Pget_attr_phase_change_f", min_dense, (def_min_dense - 1), total_error)

  CALL H5Pget_attr_phase_change_f(tcpl, max_compact, min_dense, error)
  CALL check("H5Pget_attr_phase_change_f", error, total_error)
  CALL VERIFY("H5Pget_attr_phase_change_f", max_compact, (def_max_compact + 1), total_error)
  CALL VERIFY("H5Pget_attr_phase_change_f", min_dense, (def_min_dense - 1), total_error)

  !  Close current objects 
  CALL h5pclose_f(gcpl,error)
  CALL check("h5pclose_f", error, total_error)
  CALL h5pclose_f(dcpl,error)
  CALL check("h5pclose_f", error, total_error)
  CALL h5pclose_f(tcpl,error)
  CALL check("h5pclose_f", error, total_error)

  CALL h5gclose_f(grp, error)
  CALL check("h5gclose_f",error,total_error)

  CALL h5tclose_f(dtype, error)
  CALL check("h5tclose_f",error,total_error)
  CALL h5dclose_f(dset, error)
  CALL check("h5dclose_f",error,total_error)
  CALL h5fclose_f(fid, error)
  CALL check("h5fclose_f",error,total_error)

  !  Close the FAPL 
  CALL H5Pclose_f(fapl, error)
  CALL check("H5Pclose_f", error, total_error)

END SUBROUTINE test_h5o_plist

END MODULE TH5O
