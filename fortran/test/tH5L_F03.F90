!****h* root/fortran/test/tH5L_F03.f90
!
! NAME
!  tH5L_F03.f90
!
! FUNCTION
!  Test FORTRAN HDF5 H5L APIs which are dependent on FORTRAN 2003
!  features.
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
! USES
!  liter_cb_mod
!
! CONTAINS SUBROUTINES
!  test_iter_group
!
!*****

MODULE EXTENTS

  IMPLICIT NONE

  INTEGER, PARAMETER :: MAX_CHAR_LEN = 30

END MODULE EXTENTS

MODULE liter_cb_mod

  USE HDF5
  USE TH5_MISC
  USE TH5_MISC_GEN
  USE EXTENTS
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

  TYPE iter_enum
     INTEGER RET_ZERO
     INTEGER RET_TWO
     INTEGER RET_CHANGE
     INTEGER RET_CHANGE2
  END TYPE iter_enum

  ! Custom group iteration callback data
  TYPE, bind(c) ::  iter_info
     CHARACTER(KIND=C_CHAR), DIMENSION(1:MAX_CHAR_LEN) :: name !  The name of the object
     INTEGER(c_int) :: TYPE    !  The TYPE of the object
     INTEGER(c_int) :: command ! The TYPE of RETURN value
  END TYPE iter_info

CONTAINS

!***************************************************************
!**
!**  liter_cb(): Custom link iteration callback routine.
!**
!***************************************************************

  INTEGER(KIND=C_INT) FUNCTION liter_cb(group, name, link_info, op_data) bind(C)

    IMPLICIT NONE

    INTEGER(HID_T), VALUE :: group
    CHARACTER(LEN=1), DIMENSION(1:MAX_CHAR_LEN) :: name


    TYPE (H5L_info_t) :: link_info

    TYPE(iter_info) :: op_data

    INTEGER, SAVE :: count
    INTEGER, SAVE :: count2

    INTEGER :: nlen, i

    liter_cb = 0

!!$    iter_info *info = (iter_info *)op_data;
!!$    static int count = 0;
!!$    static int count2 = 0;
    nlen = 0
    DO i = 1, MAX_CHAR_LEN
       IF( name(i) .EQ. CHAR(0) )THEN
          nlen = i - 1
          EXIT
       ENDIF
    ENDDO
    IF(nlen.NE.0)THEN
       op_data%name(1:nlen) = name(1:nlen)
    ENDIF

    SELECT CASE (op_data%command)

    CASE(0)
       liter_cb = 0
    CASE(2)
       liter_cb = 2
    CASE(3)
       count = count + 1
       IF(count.GT.10) THEN
          liter_cb = 1
       ELSE
          liter_cb = 0
       ENDIF
    CASE(4)
       count2 = count2 + 1
       IF(count2.GT.10) THEN
          liter_cb = 1
       ELSE
          liter_cb = 0
       ENDIF
    END SELECT

  END FUNCTION liter_cb
END MODULE liter_cb_mod

MODULE lvisit_cb_mod

  USE HDF5
  USE TH5_MISC
  USE TH5_MISC_GEN
  USE EXTENTS
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

  ! Custom group iteration callback data
  TYPE, bind(c) ::  visit_info
     CHARACTER(KIND=C_CHAR), DIMENSION(1:11*MAX_CHAR_LEN) :: name !  The name of the object
     INTEGER(c_int) :: TYPE    !  The TYPE of the object
     INTEGER(c_int) :: command ! The TYPE of RETURN value
     INTEGER(c_int) :: n_obj ! The TYPE of RETURN value
  END TYPE visit_info

CONTAINS

!***************************************************************
!**
!**  lvisit_cb(): Custom link visit callback routine.
!**
!***************************************************************

  INTEGER(KIND=C_INT) FUNCTION lvisit_cb(group, name, link_info, op_data) bind(C)

    IMPLICIT NONE

    INTEGER(HID_T), VALUE :: group
    CHARACTER(LEN=1), DIMENSION(1:MAX_CHAR_LEN) :: name

    TYPE(H5L_info_t) :: link_info
    TYPE(visit_info) :: op_data

    INTEGER :: nlen, i, istart, iend

    op_data%n_obj = op_data%n_obj + 1_C_INT

    nlen = 1
    DO i = 1, MAX_CHAR_LEN
       IF( name(i) .EQ. CHAR(0) )THEN
          nlen = i - 1
          EXIT
       ENDIF
    ENDDO
    IF(nlen.NE.0)THEN
       istart = (op_data%n_obj-1)*MAX_CHAR_LEN + 1
       iend = istart + MAX_CHAR_LEN - 1
       op_data%name(istart:istart+nlen-1) = name(1:nlen)
    ENDIF

    lvisit_cb = 0

  END FUNCTION lvisit_cb
END MODULE lvisit_cb_mod

MODULE TH5L_F03

CONTAINS

! *****************************************
! ***        H 5 L   T E S T S
! *****************************************


!***************************************************************
!**
!**  test_iter_group(): Test group iteration functionality
!**
!***************************************************************
SUBROUTINE test_iter_group(cleanup, total_error)

  USE liter_cb_mod
  IMPLICIT NONE

  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(INOUT) :: total_error

  INTEGER(HID_T) :: fapl
  INTEGER(HID_T) :: file             !  File ID
  INTEGER(HID_T) :: dataset          ! Dataset ID
  INTEGER(HID_T) :: datatype         ! Common datatype ID
  INTEGER(HID_T) :: filespace        ! Common dataspace ID
  INTEGER(HID_T) :: grp              ! Group ID
  INTEGER i,j                        ! counting variable
  INTEGER(hsize_t) idx               ! Index in the group
  CHARACTER(LEN=11) :: DATAFILE = "titerate.h5"
  INTEGER, PARAMETER :: ndatasets = 50
  CHARACTER(LEN=10) :: name !  temporary name buffer
  CHARACTER(LEN=10), DIMENSION(1:ndatasets+2) :: lnames !  Names of the links created

  TYPE(iter_info), TARGET :: info

  INTEGER :: error
  INTEGER :: ret_value
  TYPE(C_FUNPTR) :: f1
  TYPE(C_PTR) :: f2
  CHARACTER(LEN=2) :: ichr2
  CHARACTER(LEN=10) :: ichr10

  ! Get the default FAPL
  CALL H5Pcreate_f(H5P_FILE_ACCESS_F, fapl, error)
  CALL check("h5pcreate_f", error, total_error)

  ! Set the "use the latest version of the format" bounds for creating objects in the file
  CALL H5Pset_libver_bounds_f(fapl, H5F_LIBVER_LATEST_F, H5F_LIBVER_LATEST_F, error)
  CALL check("H5Pset_libver_bounds_f",error, total_error)

  ! Create the test file with the datasets
  CALL h5fcreate_f(DATAFILE, H5F_ACC_TRUNC_F, file, error, H5P_DEFAULT_F, fapl)
  CALL check("h5fcreate_f", error, total_error)

  ! Test iterating over empty group
  idx = 0
  info%command = 0
  f1 = C_FUNLOC(liter_cb)
  f2 = C_LOC(info)

  CALL H5Literate_f(file, H5_INDEX_NAME_F, H5_ITER_INC_F, idx, f1, f2, ret_value, error)
  CALL check("H5Literate_f", error, total_error)

  CALL H5Tcopy_f(H5T_NATIVE_INTEGER, datatype, error)
  CALL check("H5Tcopy_f", error, total_error)

  CALL H5Screate_f(H5S_SCALAR_F, filespace, error)
  CALL check("H5Screate_f", error, total_error)

  DO i = 1, ndatasets
     WRITE(ichr2, '(I2.2)') i

     name = 'Dataset '//ichr2

     CALL h5dcreate_f(file, name, datatype, filespace, dataset, error)
     CALL check("H5dcreate_f", error, total_error)

     lnames(i) = name

     CALL h5dclose_f(dataset,error)
     CALL check("H5dclose_f", error, total_error)

  ENDDO

  !  Create a group and named datatype under root group for testing

  CALL H5Gcreate_f(file, "grp0000000", grp, error)
  CALL check("H5Gcreate_f", error, total_error)

  lnames(ndatasets+2) = "grp0000000"

!!$
!!$    lnames[NDATASETS] = strdup("grp");
!!$    CHECK(lnames[NDATASETS], NULL, "strdup");
!!$

  CALL H5Tcommit_f(file, "dtype00000", datatype, error)
  CALL check("H5Tcommit_f", error, total_error)

  lnames(ndatasets+1) = "dtype00000"

  !  Close everything up

  CALL H5Tclose_f(datatype, error)
  CALL check("H5Tclose_f", error, total_error)

  CALL H5Gclose_f(grp, error)
  CALL check("H5Gclose_f", error, total_error)

  CALL H5Sclose_f(filespace, error)
  CALL check("H5Sclose_f", error, total_error)

  CALL H5Fclose_f(file, error)
  CALL check("H5Fclose_f", error, total_error)

  !  Iterate through the datasets in the root group in various ways
  CALL H5Fopen_f(DATAFILE, H5F_ACC_RDONLY_F, file, error, access_prp=fapl)
  CALL check("h5fopen_f", error, total_error)

  ! Test all objects in group, when callback always returns 0
  info%command = 0
  idx = 0
  CALL H5Literate_f(file, H5_INDEX_NAME_F, H5_ITER_INC_F, idx, f1, f2, ret_value, error)
  IF(ret_value.GT.0)THEN
     PRINT*,"ERROR: Group iteration function didn't return zero correctly!"
     CALL verify("H5Literate_f", error, -1, total_error)
  ENDIF

  !   Test all objects in group, when callback always returns 1
  !   This also tests the "restarting" ability, because the index changes

  info%command = 2
  idx = 0
  i = 0
  f1 = C_FUNLOC(liter_cb)
  f2 = C_LOC(info)
  DO
     CALL H5Literate_f(file, H5_INDEX_NAME_F, H5_ITER_INC_F, idx, f1, f2, ret_value, error)
     IF(error.LT.0) EXIT
     !  Verify return value from iterator gets propagated correctly
     CALL verify("H5Literate", ret_value, 2, total_error)
     !  Increment the number of times "2" is returned
     i = i + 1
     ! Verify that the index is the correct value
     CALL verify("H5Literate", INT(idx), INT(i), total_error)
     IF(idx .GT.ndatasets+2)THEN
        PRINT*,"ERROR: Group iteration function walked too far!"
     ENDIF

     ! Verify the correct name is retrieved
     DO j = 1, 10
        ichr10(j:j) = info%name(j)(1:1)
     ENDDO
     CALL verify("H5Literate_f", ichr10, lnames(INT(idx)), total_error)
     IF(i.EQ.52)EXIT ! prints out error message otherwise (for gcc/gfortran/g95) not intel (why) -FIXME- scot
  END DO

  ! put check if did not walk far enough -scot FIXME

  IF(i .NE. (NDATASETS + 2)) THEN
     CALL verify("H5Literate_f", i, INT(NDATASETS + 2), total_error)
     PRINT*,"ERROR: Group iteration function didn't perform multiple iterations correctly"
  ENDIF

  ! Test all objects in group, when callback changes return value
  ! This also tests the "restarting" ability, because the index changes

  info%command = 3
  idx = 0
  i = 0

  f1 = C_FUNLOC(liter_cb)
  f2 = C_LOC(info)
  DO

     CALL H5Literate_f(file, H5_INDEX_NAME_F, H5_ITER_INC_F, idx, f1, f2, ret_value, error)
     IF(error.LT.0) EXIT
     CALL verify("H5Literate_f", ret_value, 1, total_error)

     ! Increment the number of times "1" is returned
     i = i + 1

     ! Verify that the index is the correct value
     CALL verify("H5Literate_f", INT(idx), INT(i+10), total_error)

     IF(idx .GT.ndatasets+2)THEN
        PRINT*,"Group iteration function walked too far!"
     ENDIF

     DO j = 1, 10
        ichr10(j:j) = info%name(j)(1:1)
     ENDDO
     ! Verify that the correct name is retrieved
     CALL verify("H5Literate_f", ichr10, lnames(INT(idx)), total_error)
     IF(i.EQ.42)EXIT ! prints out error message otherwise (for gcc/gfortran/g95) not intel (why) -FIX- scot
  ENDDO

  IF(i .NE. 42 .OR. idx .NE. 52)THEN
     PRINT*,"ERROR: Group iteration function didn't perform multiple iterations correctly!"
     CALL check("H5Literate_f",-1,total_error)
  ENDIF

  CALL H5Fclose_f(file, error)
  CALL check("H5Fclose_f", error, total_error)

  IF(cleanup) CALL h5_cleanup_f("titerate", H5P_DEFAULT_F, error)
  CALL check("h5_cleanup_f", error, total_error)

END SUBROUTINE test_iter_group

!***************************************************************
!**
!**  Test HL visit functionality
!**
!***************************************************************
SUBROUTINE test_visit(cleanup, total_error)

  USE lvisit_cb_mod
  IMPLICIT NONE

  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(INOUT) :: total_error
  INTEGER(HID_T) :: fapl
  INTEGER(HID_T) :: fid
  INTEGER(HID_T) :: gid, gid2 ! Group IDs
  INTEGER(HID_T) :: sid  ! Dataspace ID
  INTEGER(HID_T) :: did ! Dataset ID
  CHARACTER(LEN=11) :: DATAFILE = "tvisit.h5"

  TYPE(C_FUNPTR) :: f1
  TYPE(C_PTR) :: f2
  TYPE(visit_info), TARGET :: udata

  CHARACTER(LEN=MAX_CHAR_LEN), DIMENSION(1:11) :: obj_list
  CHARACTER(LEN=MAX_CHAR_LEN) :: tmp
  INTEGER :: error
  INTEGER :: istart, iend, i, j
  INTEGER :: ret_val

  obj_list(1) = "Dataset_zero"
  obj_list(2) = "Group1"
  obj_list(3) = "Group1/Dataset_one"
  obj_list(4) = "Group1/Group2"
  obj_list(5) = "Group1/Group2/Dataset_two"
  obj_list(6) = "hard_one"
  obj_list(7) = "hard_two"
  obj_list(8) = "hard_zero"
  obj_list(9) = "soft_dangle"
  obj_list(10) = "soft_one"
  obj_list(11) = "soft_two"

  fid = H5I_INVALID_HID_F
  gid = H5I_INVALID_HID_F
  gid2 = H5I_INVALID_HID_F
  sid = H5I_INVALID_HID_F
  did = H5I_INVALID_HID_F

  ! Get the default FAPL
  CALL H5Pcreate_f(H5P_FILE_ACCESS_F, fapl, error)
  CALL check("h5pcreate_f", error, total_error)

  ! Set the "use the latest version of the format" bounds for creating objects in the file
  CALL H5Pset_libver_bounds_f(fapl, H5F_LIBVER_LATEST_F, H5F_LIBVER_LATEST_F, error)
  CALL check("H5Pset_libver_bounds_f",error, total_error)

  ! Create the test file with the datasets
  CALL h5fcreate_f(DATAFILE, H5F_ACC_TRUNC_F, fid, error, H5P_DEFAULT_F, fapl)
  CALL check("h5fcreate_f", error, total_error)

  ! Create group
  CALL h5gcreate_f(fid, "/Group1", gid, error)
  CALL check("h5gcreate_f", error, total_error)

  ! Create nested group
  CALL h5gcreate_f(gid, "Group2", gid2, error)
  CALL check("h5gcreate_f", error, total_error)

  ! Close groups
  CALL h5gclose_f(gid2, error)
  CALL check("h5gclose_f", error, total_error)
  CALL h5gclose_f(gid, error)
  CALL check("h5gclose_f", error, total_error)

  ! Create soft links to groups created
  CALL h5lcreate_soft_f("/Group1", fid, "/soft_one", error)
  CALL check("h5lcreate_soft_f", error, total_error)

  CALL h5lcreate_soft_f("/Group1/Group2", fid, "/soft_two", error)
  CALL check("h5lcreate_soft_f", error, total_error)

  ! Create dangling soft link
  CALL h5lcreate_soft_f("nowhere", fid, "/soft_dangle", error)
  CALL check("h5lcreate_soft_f", error, total_error)

  ! Create hard links to all groups
  CALL h5lcreate_hard_f(fid, "/", fid, "hard_zero", error)
  CALL check("h5lcreate_hard_f1", error, total_error)

  CALL h5lcreate_hard_f(fid, "/Group1", fid, "hard_one", error)
  CALL check("h5lcreate_hard_f2", error, total_error)
  CALL h5lcreate_hard_f(fid, "/Group1/Group2", fid, "hard_two", error)
  CALL check("h5lcreate_hard_f3", error, total_error)

  ! Create dataset in each group
  CALL h5screate_f(H5S_SCALAR_F, sid, error)
  CALL check("h5screate_f", error, total_error)

  CALL h5dcreate_f(fid, "/Dataset_zero", H5T_NATIVE_INTEGER, sid, did, error)
  CALL check("h5dcreate_f", error, total_error)
  CALL h5dclose_f(did, error)
  CALL check("h5dclose_f", error, total_error)

  CALL h5dcreate_f(fid, "/Group1/Dataset_one", H5T_NATIVE_INTEGER, sid, did, error)
  CALL check("h5dcreate_f", error, total_error)
  CALL h5dclose_f(did, error)
  CALL check("h5dclose_f", error, total_error)

  CALL h5dcreate_f(fid, "/Group1/Group2/Dataset_two", H5T_NATIVE_INTEGER, sid, did, error)
  CALL check("h5dcreate_f3", error, total_error)
  CALL h5dclose_f(did, error)
  CALL check("h5dclose_f", error, total_error)

  CALL h5sclose_f(sid, error)
  CALL check("h5sclose_f", error, total_error)

  ! Test visit functions

  f1 = C_FUNLOC(lvisit_cb)
  f2 = C_LOC(udata)

  udata%n_obj = 0
  udata%name(:) = " "
  CALL h5lvisit_f(fid, H5_INDEX_NAME_F, H5_ITER_INC_F, f1, f2, ret_val, error)
  CALL check("h5lvisit_f", error, total_error)
  IF(ret_val.LT.0)THEN
     CALL check("h5lvisit_f", -1, total_error)
  ENDIF

  IF(udata%n_obj.NE.11)THEN
     CALL check("h5lvisit_f: Wrong number of objects visited", -1, total_error)
  ENDIF

  DO i = 1, udata%n_obj
     istart = (i-1)*MAX_CHAR_LEN + 1
     iend = istart + MAX_CHAR_LEN - 1
     tmp = " "
     DO j = 1, MAX_CHAR_LEN
        IF(udata%name(istart+j-1) .NE. " ")THEN
           tmp(j:j) = udata%name(istart+j-1)
        ELSE
           EXIT
        ENDIF
     ENDDO
     IF( TRIM(tmp) .NE. TRIM(obj_list(i)) )THEN
        CALL check("h5lvisit_f: Wrong object list from visit", -1, total_error)
        EXIT
     ENDIF
  ENDDO

  udata%n_obj = 0
  udata%name(:) = " "
  CALL h5lvisit_by_name_f(fid, "/", H5_INDEX_NAME_F, H5_ITER_INC_F, f1, f2, ret_val, error)
  CALL check("h5lvisit_by_name_f", error, total_error)
  IF(ret_val.LT.0)THEN
     CALL check("h5ovisit_f", -1, total_error)
  ENDIF

  IF(udata%n_obj.NE.11)THEN
     CALL check("h5lvisit_by_name_f: Wrong number of objects visited", -1, total_error)
  ENDIF

  DO i = 1, udata%n_obj
     istart = (i-1)*MAX_CHAR_LEN + 1
     iend = istart + MAX_CHAR_LEN - 1
     tmp = " "
     DO j = 1, MAX_CHAR_LEN
        IF(udata%name(istart+j-1) .NE. " ")THEN
           tmp(j:j) = udata%name(istart+j-1)
        ELSE
           EXIT
        ENDIF
     ENDDO
     IF( TRIM(tmp) .NE. TRIM(obj_list(i)) )THEN
        CALL check("h5lvisit_by_name_f: Wrong object list from visit", -1, total_error)
        EXIT
     ENDIF
  ENDDO

  CALL h5fclose_f(fid, error)
  CALL check("h5fclose_f", error, total_error)

  IF(cleanup) CALL h5_cleanup_f("tvisit", H5P_DEFAULT_F, error)
  CALL check("h5_cleanup_f", error, total_error)

END SUBROUTINE test_visit

END MODULE TH5L_F03
