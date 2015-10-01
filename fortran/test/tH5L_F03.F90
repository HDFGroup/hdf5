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
! USES
!  liter_cb_mod
!
! CONTAINS SUBROUTINES
!  test_iter_group
!
!*****
MODULE liter_cb_mod

  USE HDF5 
  USE TH5_MISC 
  USE TH5_MISC_GEN
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
     CHARACTER(KIND=C_CHAR), DIMENSION(1:10) :: name !  The name of the object 
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
    CHARACTER(LEN=1), DIMENSION(1:10) :: name


    TYPE (H5L_info_t) :: link_info

    TYPE(iter_info) :: op_data

    INTEGER, SAVE :: count
    INTEGER, SAVE :: count2

!!$    
!!$    iter_info *info = (iter_info *)op_data;
!!$    static int count = 0;
!!$    static int count2 = 0;

    op_data%name(1:10) = name(1:10)

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
SUBROUTINE test_iter_group(total_error)

  USE liter_cb_mod
  IMPLICIT NONE

  INTEGER, INTENT(INOUT) :: total_error
  INTEGER(HID_T) :: fapl
  INTEGER(HID_T) :: file             !  File ID 
  INTEGER(hid_t) :: dataset          ! Dataset ID 
  INTEGER(hid_t) :: datatype         ! Common datatype ID 
  INTEGER(hid_t) :: filespace        ! Common dataspace ID 
  INTEGER(hid_t) :: grp              ! Group ID 
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
!!$    lnames[NDATASETS] = HDstrdup("grp");
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

END SUBROUTINE test_iter_group

END MODULE TH5L_F03
