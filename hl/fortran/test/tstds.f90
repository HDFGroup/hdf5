! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! * Copyright by The HDF Group.                                               *
! * Copyright by the Board of Trustees of the University of Illinois.         *
! * All rights reserved.                                                      *
! *                                                                           *
! * This file is part of HDF5.  The full HDF5 copyright notice, including     *
! * terms governing use, modification, and redistribution, is contained in    *
! * the files COPYING and Copyright.html.  COPYING can be found at the root   *
! * of the source code distribution tree; Copyright.html can be found at the  *
! * root level of an installed copy of the electronic HDF5 document set and   *
! * is linked from the top-level documents page.  It can also be found at     *
! * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
! * access to either file, you may request a copy from help@hdfgroup.org.     *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
PROGRAM test_ds

  IMPLICIT NONE

  INTEGER :: err

  CALL test_testds(err)

  IF(err.LT.0)THEN
     WRITE(*,'(5X,A)') "DIMENSION SCALES TEST *FAILED*"
  ENDIF

END PROGRAM test_ds

SUBROUTINE test_testds(err)

  USE HDF5
  USE H5LT
  USE H5DS

  IMPLICIT NONE

  INTEGER, PARAMETER :: RANK      = 2 ! rank of DATA dataset 
  INTEGER, PARAMETER :: DIM_DATA  = 12
  INTEGER, PARAMETER :: DIM1_SIZE = 3
  INTEGER, PARAMETER :: DIM2_SIZE = 4
  INTEGER, PARAMETER :: DIM1      = 1
  INTEGER, PARAMETER :: DIM2      = 2
  INTEGER, PARAMETER :: FAILED    = -1

  CHARACTER(LEN=6), PARAMETER :: DSET_NAME = "Mydata"
  CHARACTER(LEN=5), PARAMETER :: DS_1_NAME = "Yaxis"
  CHARACTER(LEN=5), PARAMETER :: DS_1_NAME_A = "Yaxiz"
  CHARACTER(LEN=5), PARAMETER :: DS_2_NAME = "Xaxis"


  INTEGER(hid_t) :: fid    ! file ID
  INTEGER(hid_t) :: did    ! dataset ID
  INTEGER(hid_t) :: dsid   ! DS dataset ID
  INTEGER :: rankds = 1    ! rank of DS dataset 
  INTEGER(hsize_t), DIMENSION(1:rank) ::  dims  = (/DIM2_SIZE,DIM1_SIZE/) ! size of DATA dataset 
  INTEGER, DIMENSION(1:DIM_DATA) :: buf = (/1,2,3,4,5,6,7,8,9,10,11,12/)  ! DATA of DATA dataset 
  INTEGER(hsize_t), DIMENSION(1:1) ::  s1_dim  = (/DIM1_SIZE/)  ! size of DS 1 dataset 
  INTEGER(hsize_t), DIMENSION(1:1) ::  s2_dim  = (/DIM2_SIZE/)  ! size of DS 2 dataset 
  REAL, DIMENSION(1:DIM1_SIZE) ::   s1_wbuf = (/10,20,30/)     ! DATA of DS 1 dataset 
  INTEGER, DIMENSION(1:DIM2_SIZE) :: s2_wbuf = (/10,20,50,100/) ! DATA of DS 2 dataset 
  INTEGER :: err
  INTEGER :: num_scales
  INTEGER(size_t) :: name_len
  CHARACTER(LEN=80) :: name
  INTEGER(size_t) :: label_len
  CHARACTER(LEN=80) :: label
  LOGICAL :: is_attached, is_scale

  !
  ! Initialize FORTRAN predefined datatypes.
  !
  CALL h5open_f(err)
  IF(err.LT.0) RETURN

  ! create a file using default properties
  CALL H5Fcreate_f("tstds.h5",H5F_ACC_TRUNC_F, fid, err)
  IF(err.LT.0) RETURN

  ! make a dataset 
  CALL H5LTmake_dataset_int_f(fid,DSET_NAME,rank,dims,buf, err)
  IF(err.LT.0) RETURN

  ! make a DS dataset for the first dimension
  CALL H5LTmake_dataset_float_f(fid,DS_1_NAME,rankds,s1_dim,s1_wbuf,err)
  IF(err.LT.0) RETURN

  ! make a DS dataset for the second dimension
  CALL H5LTmake_dataset_int_f(fid,DS_2_NAME,rankds,s2_dim,s2_wbuf,err)
  IF(err.LT.0) RETURN

  !-------------------------------------------------------------------------
  ! attach the DS_1_NAME dimension scale to DSET_NAME at dimension 1
  !-------------------------------------------------------------------------

  CALL test_begin(' Test Attaching Dimension Scale         ')

  ! get the dataset id for DSET_NAME
  CALL H5Dopen_f(fid, DSET_NAME, did, err)
  IF(err.LT.0) RETURN

  ! get the DS dataset id
  CALL H5Dopen_f(fid, DS_1_NAME, dsid, err)
  IF(err.LT.0) RETURN

  ! check attaching to a non-existent dimension; should fail
  CALL H5DSattach_scale_f(did, dsid, 20, err)
  IF(err.NE.-1) THEN
     err = FAILED ! should fail, mark as an error
     CALL write_test_status(err)
     RETURN
  ENDIF

  ! attach the DS_1_NAME dimension scale to DSET_NAME at dimension index 1
  CALL H5DSattach_scale_f(did, dsid, DIM1, err)
  IF(err.EQ.-1) THEN
     CALL write_test_status(err)
     RETURN
  ENDIF
  CALL write_test_status(err)

  CALL test_begin(' Test If Dimension Scale Attached       ')

  CALL H5DSis_attached_f(did, dsid, DIM1, is_attached, err)
  IF(err.EQ.-1.OR..NOT.is_attached) THEN
     err = FAILED
     CALL write_test_status(err)
     RETURN
  ENDIF
  CALL write_test_status(err)

  ! Check to see how many Dimension Scales are attached

  CALL test_begin(' Test Getting Number Dimension Scales   ')

  CALL H5DSget_num_scales_f(did, DIM1, num_scales, err)
  IF(err.LT.0.OR.num_scales.NE.1)THEN
     err = FAILED
     CALL write_test_status(err)
     RETURN
  ENDIF
  CALL write_test_status(err)

  CALL test_begin(' Test Detaching Dimension Scale         ')

  ! Detach scale
  CALL H5DSdetach_scale_f(did, dsid, DIM1, err)
  IF(err.LT.0) RETURN

  ! Check to see if a dimension scale is attached, should be .false.
  CALL H5DSis_attached_f(did, dsid, DIM1, is_attached, err)
  IF(err.LT.0.OR.is_attached)THEN
     err = FAILED
     CALL write_test_status(err)
     RETURN
  ENDIF
  CALL write_test_status(err)
  
  !-------------------------------------------------------------------------
  ! set the DS_1_NAME dimension scale to DSET_NAME at dimension 0
  !-------------------------------------------------------------------------
 
  CALL test_begin(' Test Setting Dimension Scale           ')

  CALL H5DSset_scale_f(dsid, err, "Dimension Scale Set 1")
  IF(err.LT.0.OR.is_attached)THEN
     err = FAILED
     CALL write_test_status(err)
     RETURN
  ENDIF
  CALL write_test_status(err)

  CALL test_begin(' Test If Dimension Scale                ')

  CALL H5DSis_scale_f(dsid, is_scale, err)
  IF(err.LT.0.OR..NOT.is_scale)THEN
     err = FAILED
     CALL write_test_status(err)
     RETURN
  ENDIF
  CALL write_test_status(err)

  ! Get scale name; test to large character buffer

  CALL test_begin(' Test Getting Dimension Scale By Name   ')

  name_len = 25
  name = ''
  CALL H5DSget_scale_name_f(dsid, name, name_len, err)
  IF(err.LT.0 .OR. &
       name_len.NE.21 .OR. &
       TRIM(name).NE."Dimension Scale Set 1" .OR. &
       name(22:25).NE.'   ')THEN
     err = FAILED
     CALL write_test_status(err)
     RETURN
  ENDIF

  ! Get scale name; test exact size character buffer
  name_len = 21
  name = ''
  CALL H5DSget_scale_name_f(dsid, name(1:name_len), name_len, err)
  IF(err.LT.0.OR.name_len.NE.21.OR.TRIM(name).NE."Dimension Scale Set 1")THEN
     err = FAILED
     CALL write_test_status(err)
     RETURN
  ENDIF

  ! Get scale name; test to small character buffer
  name_len = 5
  name = ''
  CALL H5DSget_scale_name_f(dsid, name(1:name_len), name_len, err)
  IF(err.LT.0.OR.name_len.NE.21.OR.TRIM(name).NE."Dimen")THEN
     err = FAILED
     CALL write_test_status(err)
     RETURN
  ENDIF
  
  ! close DS id
  CALL H5Dclose_f(dsid, err)
  IF(err.LT.0) RETURN
  
  !-------------------------------------------------------------------------
  ! attach the DS_2_NAME dimension scale to DSET_NAME
  !-------------------------------------------------------------------------
 
  ! get the DS dataset id
  CALL H5Dopen_f(fid, DS_2_NAME, dsid, err)
  IF(err.LT.0) RETURN

  ! attach the DS_2_NAME dimension scale to DSET_NAME as the 2nd dimension (index 2)
  CALL H5DSattach_scale_f(did, dsid, DIM2, err)
  IF(err.LT.0) RETURN

  CALL H5DSis_attached_f(did, dsid, DIM2, is_attached, err)
  IF(err.LT.0) RETURN

  ! test sending no Dimension Scale name

  CALL H5DSset_scale_f(dsid, err)
  IF(err.LT.0)THEN
     CALL write_test_status(err)
     RETURN
  ENDIF

  CALL H5DSis_scale_f(dsid, is_scale, err)
  IF(err.LT.0.OR..NOT.is_scale)THEN
     err = FAILED
     CALL write_test_status(err)
     RETURN
  ENDIF

  ! Get scale name when there is no scale name
  name_len = 5
  name = ''
  CALL H5DSget_scale_name_f(dsid, name(1:name_len), name_len, err)
  IF(err.LT.0.OR.name_len.NE.0)THEN ! name_len is 0 if no name is found
     err = FAILED
     CALL write_test_status(err)
     RETURN
  ENDIF

  CALL write_test_status(err)

  CALL test_begin(' Test Setting Dimension Scale Label     ')

  CALL H5DSset_label_f(did, DIM2, "Label12", err)
  IF(err.LT.0)THEN
     CALL write_test_status(err)
     RETURN
  ENDIF

  ! Test label where character length is to small
  
  label_len = 5
  label = ''
  CALL H5DSget_label_f(did, DIM2, label(1:label_len), label_len, err)
  IF(err.LT.0.OR.label(1:5).NE."Label".OR.label_len.NE.7)THEN
     err = FAILED
     CALL write_test_status(err)
     RETURN
  ENDIF

  ! Test label where character length is exact

  label_len = 7
  label = ''
  CALL H5DSget_label_f(did, DIM2, label(1:label_len), label_len, err)
  IF(err.LT.0.OR.label(1:label_len).NE."Label12".OR.label_len.NE.7)THEN
     err = FAILED
     CALL write_test_status(err)
     RETURN
  ENDIF

  ! Test label where character length is to big

  label_len = 25
  label = ''
  CALL H5DSget_label_f(did, DIM2, label, label_len, err)
  IF(err.LT.0.OR. &
       label(1:label_len).NE."Label12" .OR. &
       label_len.NE.7 .OR. &
       label(8:25).NE.'                  ')THEN
     err = FAILED
     CALL write_test_status(err)
     RETURN
  ENDIF
  CALL write_test_status(err)

 ! close DS id
  CALL H5Dclose_f(dsid, err)
  IF(err.LT.0) RETURN

 ! close file 
  CALL H5Fclose_f(fid, err)
  IF(err.LT.0) RETURN

END SUBROUTINE test_testds

!-------------------------------------------------------------------------
! test_begin
!-------------------------------------------------------------------------

SUBROUTINE test_begin(string)
  CHARACTER(LEN=*), INTENT(IN) :: string
  WRITE(*, fmt = '(A)', advance = 'no') ADJUSTL(string)
END SUBROUTINE test_begin

!-------------------------------------------------------------------------
! passed/failed
!-------------------------------------------------------------------------
SUBROUTINE write_test_status( test_result)

! Writes the results of the tests

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: test_result  ! negative,   failed
                                      ! 0       ,   passed

! Controls the output style for reporting test results

  CHARACTER(LEN=8) :: error_string
  CHARACTER(LEN=8), PARAMETER :: success = ' PASSED '
  CHARACTER(LEN=8), PARAMETER :: failure = '*FAILED*'

  error_string = failure
  IF (test_result .EQ.  0) THEN
     error_string = success
  ENDIF
  
  WRITE(*, fmt = '(T34, A)') error_string

END SUBROUTINE write_test_status
