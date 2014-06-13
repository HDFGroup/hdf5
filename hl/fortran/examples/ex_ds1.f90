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
PROGRAM example_ds

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

  CHARACTER(LEN=6), PARAMETER :: DSET_NAME = "MYDATA"
  CHARACTER(LEN=5), PARAMETER :: DS_1_NAME = "Xaxis"
  CHARACTER(LEN=5), PARAMETER :: DS_2_NAME = "Yaxis"


  INTEGER(hid_t) :: fid    ! file ID
  INTEGER(hid_t) :: did    ! dataset ID
  INTEGER(hid_t) :: dsid   ! DS dataset ID
  INTEGER :: rankds = 1    ! rank of DS dataset 
  INTEGER(hsize_t), DIMENSION(1:rank) ::  dims  = (/DIM2_SIZE,DIM1_SIZE/) ! size of data dataset 
  INTEGER, DIMENSION(1:DIM_DATA) :: buf = (/1,2,3,4,5,6,7,8,9,10,11,12/)  ! data of data dataset 
  INTEGER(hsize_t), DIMENSION(1:1) ::  s1_dim  = (/DIM1_SIZE/)  ! size of DS 1 dataset 
  INTEGER(hsize_t), DIMENSION(1:1) ::  s2_dim  = (/DIM2_SIZE/)  ! size of DS 2 dataset 
  REAL, DIMENSION(1:DIM1_SIZE) :: s1_wbuf = (/10,20,30/)     ! data of DS 1 dataset 
  REAL, DIMENSION(1:DIM2_SIZE) :: s2_wbuf = (/10,20,50,100/) ! data of DS 2 dataset 
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

  ! create a file using default properties
  CALL H5Fcreate_f("ex_ds1.h5",H5F_ACC_TRUNC_F, fid, err)

  ! make a dataset 
  CALL H5LTmake_dataset_int_f(fid, DSET_NAME, rank,dims,buf, err)

  ! make a DS dataset for the first dimension
  CALL H5LTmake_dataset_float_f(fid,DS_1_NAME,rankds,s1_dim,s1_wbuf,err)

  ! make a DS dataset for the second dimension
  CALL H5LTmake_dataset_float_f(fid,DS_2_NAME,rankds,s2_dim,s2_wbuf,err)

  !-------------------------------------------------------------------------
  ! attach the DS_1_NAME dimension scale to DSET_NAME at dimension 1
  ! and then detach it.
  !-------------------------------------------------------------------------
 
  ! get the dataset id for DSET_NAME
  CALL H5Dopen_f(fid, DSET_NAME, did, err)

  ! get the DS dataset id
  CALL H5Dopen_f(fid, DS_1_NAME, dsid, err)

  WRITE(*,'(/,5A,I0)') &
       "Attach Dimension Scale """,TRIM(DS_1_NAME),'" to "', TRIM(DSET_NAME), '" at dimension ', DIM1

  ! attach the DS_1_NAME dimension scale to DSET_NAME at dimension index 1
  CALL H5DSattach_scale_f(did, dsid, DIM1, err)

  ! Test if dimension Scale Attached 
  CALL H5DSis_attached_f(did, dsid, DIM1, is_attached, err)
  WRITE(*,'(/,5X,3(A,1X),I0,A,L1)') 'Is',TRIM(DS_1_NAME),&
       'attached to dimension',DIM1,' ... ',is_attached
  

  ! Check to see how many Dimension Scales are attached

  CALL H5DSget_num_scales_f(did, DIM1, num_scales, err)

  WRITE(*,'(5X,A,I0)') 'Total number of Dimension Scales Attached ... ', num_scales

  ! Detach scale
  CALL H5DSdetach_scale_f(did, dsid, DIM1, err)
  WRITE(*,'(/,5A,I0)') &
       "Detach Dimension Scale """,TRIM(DS_1_NAME),'" from "', TRIM(DSET_NAME), '" at dimension ', DIM1

  ! Check to see if a dimension scale is attached, should be .false.
  CALL H5DSis_attached_f(did, dsid, DIM1, is_attached, err)
  WRITE(*,'(/,5X,3(A,1X),I0,A,L1)') 'Is',TRIM(DS_1_NAME),&
       'attached to dimension',DIM1,' ... ',is_attached
  
  !-------------------------------------------------------------------------
  ! set the DS_1_NAME dimension scale to DSET_NAME at dimension 1
  !-------------------------------------------------------------------------
 
  WRITE(*,'(/,5A,I0)') &
       'Set Dimension Scale "', TRIM(DS_1_NAME), '" to "', TRIM(DSET_NAME), '" at dimension ', DIM1

  CALL H5DSset_scale_f(dsid, err, "Set X")

  ! Test if Dimension Scale

  CALL H5DSis_scale_f(dsid, is_scale, err)

  ! Get scale name

  name_len = 25
  name = ''
  CALL H5DSget_scale_name_f(dsid, name, name_len, err)

  WRITE(*,'(/,5X,A,A)') 'The Dimension Scale name is ... ', name(1:name_len) 

  ! Setting Dimension Scale Label

  WRITE(*,'(/,A,I0)') "Setting Dimension Scale label ""X"" for dimension ", DIM1 

  CALL H5DSset_label_f(did, DIM1, "X", err)

  label_len = 25
  label = ''
  CALL H5DSget_label_f(did, DIM1, label, label_len, err)

  WRITE(*,'(/,5X,A,I0,2A)') 'Dimension Scale Label for dimension ', DIM1, ' is ... ', label(1:label_len)

  ! close DS id
  CALL H5Dclose_f(dsid, err)
  
  !-------------------------------------------------------------------------
  ! attach the DS_2_NAME dimension scale to DSET_NAME
  !-------------------------------------------------------------------------
 
  ! get the DS dataset id
  CALL H5Dopen_f(fid, DS_2_NAME, dsid, err)

  ! attach the DS_2_NAME dimension scale to DSET_NAME as the 2nd dimension (index 2)

  WRITE(*,'(/,5A,I0)') &
       'Set Dimension Scale "', TRIM(DS_2_NAME), '" to "', TRIM(DSET_NAME), '" at dimension ', DIM2

  CALL H5DSattach_scale_f(did, dsid, DIM2, err)

  CALL H5DSis_attached_f(did, dsid, DIM2, is_attached, err)

  CALL H5DSset_scale_f(dsid, err, "Set Y")

  ! Get scale name
  name_len = 25
  name = ''
  CALL H5DSget_scale_name_f(dsid, name(1:name_len), name_len, err)

  WRITE(*,'(/,5X,A,A)') 'The Dimension Scale name is ... ', name(1:name_len) 


  ! Setting Dimension Scale Label

  WRITE(*,'(/,A,I0)') "Setting Dimension Scale label ""Y"" for dimension ", DIM2

  CALL H5DSset_label_f(did, DIM2, "Y", err)

  ! Get Label

  label_len = 25
  label = ''
  CALL H5DSget_label_f(did, DIM2, label, label_len, err)
  
  WRITE(*,'(/,5X,A,I0,2A,/)') 'Dimension Scale Label for dimension ', DIM2, ' is ... ', label(1:label_len)

 ! close DS id
  CALL H5Dclose_f(dsid, err)

 ! close file 
  CALL H5Fclose_f(fid, err)

END PROGRAM example_ds

