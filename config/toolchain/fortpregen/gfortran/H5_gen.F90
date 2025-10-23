!****h* ROBODoc/H5_gen.F90
!
! NAME
!  H5_gen
! 
! PURPOSE
!  This module is generated at build by H5_buildiface.F90 to handle all the
!  detected KINDs for APIs being passed INTEGERs, REALs and CHARACTERs. Currently 
!  these are H5A, H5D and H5P APIs
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
! AUTHOR
!  H5_buildiface.F90
!
!*****

MODULE H5_GEN
  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR, C_LOC
  USE H5GLOBAL
  USE H5A
  USE H5D
  USE H5P
  IMPLICIT NONE
  PRIVATE h5awrite_rkind_4_rank_0
  PRIVATE h5awrite_rkind_4_rank_1
  PRIVATE h5awrite_rkind_4_rank_2
  PRIVATE h5awrite_rkind_4_rank_3
  PRIVATE h5awrite_rkind_4_rank_4
  PRIVATE h5awrite_rkind_4_rank_5
  PRIVATE h5awrite_rkind_4_rank_6
  PRIVATE h5awrite_rkind_4_rank_7
  PRIVATE h5awrite_rkind_8_rank_0
  PRIVATE h5awrite_rkind_8_rank_1
  PRIVATE h5awrite_rkind_8_rank_2
  PRIVATE h5awrite_rkind_8_rank_3
  PRIVATE h5awrite_rkind_8_rank_4
  PRIVATE h5awrite_rkind_8_rank_5
  PRIVATE h5awrite_rkind_8_rank_6
  PRIVATE h5awrite_rkind_8_rank_7
  PRIVATE h5awrite_rkind_10_rank_0
  PRIVATE h5awrite_rkind_10_rank_1
  PRIVATE h5awrite_rkind_10_rank_2
  PRIVATE h5awrite_rkind_10_rank_3
  PRIVATE h5awrite_rkind_10_rank_4
  PRIVATE h5awrite_rkind_10_rank_5
  PRIVATE h5awrite_rkind_10_rank_6
  PRIVATE h5awrite_rkind_10_rank_7
  PRIVATE h5awrite_rkind_16_rank_0
  PRIVATE h5awrite_rkind_16_rank_1
  PRIVATE h5awrite_rkind_16_rank_2
  PRIVATE h5awrite_rkind_16_rank_3
  PRIVATE h5awrite_rkind_16_rank_4
  PRIVATE h5awrite_rkind_16_rank_5
  PRIVATE h5awrite_rkind_16_rank_6
  PRIVATE h5awrite_rkind_16_rank_7
  PRIVATE h5awrite_ikind_1_rank_0
  PRIVATE h5awrite_ikind_1_rank_1
  PRIVATE h5awrite_ikind_1_rank_2
  PRIVATE h5awrite_ikind_1_rank_3
  PRIVATE h5awrite_ikind_1_rank_4
  PRIVATE h5awrite_ikind_1_rank_5
  PRIVATE h5awrite_ikind_1_rank_6
  PRIVATE h5awrite_ikind_1_rank_7
  PRIVATE h5awrite_ikind_2_rank_0
  PRIVATE h5awrite_ikind_2_rank_1
  PRIVATE h5awrite_ikind_2_rank_2
  PRIVATE h5awrite_ikind_2_rank_3
  PRIVATE h5awrite_ikind_2_rank_4
  PRIVATE h5awrite_ikind_2_rank_5
  PRIVATE h5awrite_ikind_2_rank_6
  PRIVATE h5awrite_ikind_2_rank_7
  PRIVATE h5awrite_ikind_4_rank_0
  PRIVATE h5awrite_ikind_4_rank_1
  PRIVATE h5awrite_ikind_4_rank_2
  PRIVATE h5awrite_ikind_4_rank_3
  PRIVATE h5awrite_ikind_4_rank_4
  PRIVATE h5awrite_ikind_4_rank_5
  PRIVATE h5awrite_ikind_4_rank_6
  PRIVATE h5awrite_ikind_4_rank_7
  PRIVATE h5awrite_ikind_8_rank_0
  PRIVATE h5awrite_ikind_8_rank_1
  PRIVATE h5awrite_ikind_8_rank_2
  PRIVATE h5awrite_ikind_8_rank_3
  PRIVATE h5awrite_ikind_8_rank_4
  PRIVATE h5awrite_ikind_8_rank_5
  PRIVATE h5awrite_ikind_8_rank_6
  PRIVATE h5awrite_ikind_8_rank_7
  PRIVATE h5awrite_ikind_16_rank_0
  PRIVATE h5awrite_ikind_16_rank_1
  PRIVATE h5awrite_ikind_16_rank_2
  PRIVATE h5awrite_ikind_16_rank_3
  PRIVATE h5awrite_ikind_16_rank_4
  PRIVATE h5awrite_ikind_16_rank_5
  PRIVATE h5awrite_ikind_16_rank_6
  PRIVATE h5awrite_ikind_16_rank_7
  PRIVATE h5awrite_ckind_rank_1
  PRIVATE h5awrite_ckind_rank_2
  PRIVATE h5awrite_ckind_rank_3
  PRIVATE h5awrite_ckind_rank_4
  PRIVATE h5awrite_ckind_rank_5
  PRIVATE h5awrite_ckind_rank_6
  PRIVATE h5awrite_ckind_rank_7
  PRIVATE h5aread_rkind_4_rank_0
  PRIVATE h5aread_rkind_4_rank_1
  PRIVATE h5aread_rkind_4_rank_2
  PRIVATE h5aread_rkind_4_rank_3
  PRIVATE h5aread_rkind_4_rank_4
  PRIVATE h5aread_rkind_4_rank_5
  PRIVATE h5aread_rkind_4_rank_6
  PRIVATE h5aread_rkind_4_rank_7
  PRIVATE h5aread_rkind_8_rank_0
  PRIVATE h5aread_rkind_8_rank_1
  PRIVATE h5aread_rkind_8_rank_2
  PRIVATE h5aread_rkind_8_rank_3
  PRIVATE h5aread_rkind_8_rank_4
  PRIVATE h5aread_rkind_8_rank_5
  PRIVATE h5aread_rkind_8_rank_6
  PRIVATE h5aread_rkind_8_rank_7
  PRIVATE h5aread_rkind_10_rank_0
  PRIVATE h5aread_rkind_10_rank_1
  PRIVATE h5aread_rkind_10_rank_2
  PRIVATE h5aread_rkind_10_rank_3
  PRIVATE h5aread_rkind_10_rank_4
  PRIVATE h5aread_rkind_10_rank_5
  PRIVATE h5aread_rkind_10_rank_6
  PRIVATE h5aread_rkind_10_rank_7
  PRIVATE h5aread_rkind_16_rank_0
  PRIVATE h5aread_rkind_16_rank_1
  PRIVATE h5aread_rkind_16_rank_2
  PRIVATE h5aread_rkind_16_rank_3
  PRIVATE h5aread_rkind_16_rank_4
  PRIVATE h5aread_rkind_16_rank_5
  PRIVATE h5aread_rkind_16_rank_6
  PRIVATE h5aread_rkind_16_rank_7
  PRIVATE h5aread_ikind_1_rank_0
  PRIVATE h5aread_ikind_1_rank_1
  PRIVATE h5aread_ikind_1_rank_2
  PRIVATE h5aread_ikind_1_rank_3
  PRIVATE h5aread_ikind_1_rank_4
  PRIVATE h5aread_ikind_1_rank_5
  PRIVATE h5aread_ikind_1_rank_6
  PRIVATE h5aread_ikind_1_rank_7
  PRIVATE h5aread_ikind_2_rank_0
  PRIVATE h5aread_ikind_2_rank_1
  PRIVATE h5aread_ikind_2_rank_2
  PRIVATE h5aread_ikind_2_rank_3
  PRIVATE h5aread_ikind_2_rank_4
  PRIVATE h5aread_ikind_2_rank_5
  PRIVATE h5aread_ikind_2_rank_6
  PRIVATE h5aread_ikind_2_rank_7
  PRIVATE h5aread_ikind_4_rank_0
  PRIVATE h5aread_ikind_4_rank_1
  PRIVATE h5aread_ikind_4_rank_2
  PRIVATE h5aread_ikind_4_rank_3
  PRIVATE h5aread_ikind_4_rank_4
  PRIVATE h5aread_ikind_4_rank_5
  PRIVATE h5aread_ikind_4_rank_6
  PRIVATE h5aread_ikind_4_rank_7
  PRIVATE h5aread_ikind_8_rank_0
  PRIVATE h5aread_ikind_8_rank_1
  PRIVATE h5aread_ikind_8_rank_2
  PRIVATE h5aread_ikind_8_rank_3
  PRIVATE h5aread_ikind_8_rank_4
  PRIVATE h5aread_ikind_8_rank_5
  PRIVATE h5aread_ikind_8_rank_6
  PRIVATE h5aread_ikind_8_rank_7
  PRIVATE h5aread_ikind_16_rank_0
  PRIVATE h5aread_ikind_16_rank_1
  PRIVATE h5aread_ikind_16_rank_2
  PRIVATE h5aread_ikind_16_rank_3
  PRIVATE h5aread_ikind_16_rank_4
  PRIVATE h5aread_ikind_16_rank_5
  PRIVATE h5aread_ikind_16_rank_6
  PRIVATE h5aread_ikind_16_rank_7
  PRIVATE h5aread_ckind_rank_1
  PRIVATE h5aread_ckind_rank_2
  PRIVATE h5aread_ckind_rank_3
  PRIVATE h5aread_ckind_rank_4
  PRIVATE h5aread_ckind_rank_5
  PRIVATE h5aread_ckind_rank_6
  PRIVATE h5aread_ckind_rank_7
  PRIVATE h5dwrite_rkind_4_rank_0
  PRIVATE h5dwrite_rkind_4_rank_1
  PRIVATE h5dwrite_rkind_4_rank_2
  PRIVATE h5dwrite_rkind_4_rank_3
  PRIVATE h5dwrite_rkind_4_rank_4
  PRIVATE h5dwrite_rkind_4_rank_5
  PRIVATE h5dwrite_rkind_4_rank_6
  PRIVATE h5dwrite_rkind_4_rank_7
  PRIVATE h5dwrite_rkind_8_rank_0
  PRIVATE h5dwrite_rkind_8_rank_1
  PRIVATE h5dwrite_rkind_8_rank_2
  PRIVATE h5dwrite_rkind_8_rank_3
  PRIVATE h5dwrite_rkind_8_rank_4
  PRIVATE h5dwrite_rkind_8_rank_5
  PRIVATE h5dwrite_rkind_8_rank_6
  PRIVATE h5dwrite_rkind_8_rank_7
  PRIVATE h5dwrite_rkind_10_rank_0
  PRIVATE h5dwrite_rkind_10_rank_1
  PRIVATE h5dwrite_rkind_10_rank_2
  PRIVATE h5dwrite_rkind_10_rank_3
  PRIVATE h5dwrite_rkind_10_rank_4
  PRIVATE h5dwrite_rkind_10_rank_5
  PRIVATE h5dwrite_rkind_10_rank_6
  PRIVATE h5dwrite_rkind_10_rank_7
  PRIVATE h5dwrite_rkind_16_rank_0
  PRIVATE h5dwrite_rkind_16_rank_1
  PRIVATE h5dwrite_rkind_16_rank_2
  PRIVATE h5dwrite_rkind_16_rank_3
  PRIVATE h5dwrite_rkind_16_rank_4
  PRIVATE h5dwrite_rkind_16_rank_5
  PRIVATE h5dwrite_rkind_16_rank_6
  PRIVATE h5dwrite_rkind_16_rank_7
  PRIVATE h5dwrite_ikind_1_rank_0
  PRIVATE h5dwrite_ikind_1_rank_1
  PRIVATE h5dwrite_ikind_1_rank_2
  PRIVATE h5dwrite_ikind_1_rank_3
  PRIVATE h5dwrite_ikind_1_rank_4
  PRIVATE h5dwrite_ikind_1_rank_5
  PRIVATE h5dwrite_ikind_1_rank_6
  PRIVATE h5dwrite_ikind_1_rank_7
  PRIVATE h5dwrite_ikind_2_rank_0
  PRIVATE h5dwrite_ikind_2_rank_1
  PRIVATE h5dwrite_ikind_2_rank_2
  PRIVATE h5dwrite_ikind_2_rank_3
  PRIVATE h5dwrite_ikind_2_rank_4
  PRIVATE h5dwrite_ikind_2_rank_5
  PRIVATE h5dwrite_ikind_2_rank_6
  PRIVATE h5dwrite_ikind_2_rank_7
  PRIVATE h5dwrite_ikind_4_rank_0
  PRIVATE h5dwrite_ikind_4_rank_1
  PRIVATE h5dwrite_ikind_4_rank_2
  PRIVATE h5dwrite_ikind_4_rank_3
  PRIVATE h5dwrite_ikind_4_rank_4
  PRIVATE h5dwrite_ikind_4_rank_5
  PRIVATE h5dwrite_ikind_4_rank_6
  PRIVATE h5dwrite_ikind_4_rank_7
  PRIVATE h5dwrite_ikind_8_rank_0
  PRIVATE h5dwrite_ikind_8_rank_1
  PRIVATE h5dwrite_ikind_8_rank_2
  PRIVATE h5dwrite_ikind_8_rank_3
  PRIVATE h5dwrite_ikind_8_rank_4
  PRIVATE h5dwrite_ikind_8_rank_5
  PRIVATE h5dwrite_ikind_8_rank_6
  PRIVATE h5dwrite_ikind_8_rank_7
  PRIVATE h5dwrite_ikind_16_rank_0
  PRIVATE h5dwrite_ikind_16_rank_1
  PRIVATE h5dwrite_ikind_16_rank_2
  PRIVATE h5dwrite_ikind_16_rank_3
  PRIVATE h5dwrite_ikind_16_rank_4
  PRIVATE h5dwrite_ikind_16_rank_5
  PRIVATE h5dwrite_ikind_16_rank_6
  PRIVATE h5dwrite_ikind_16_rank_7
  PRIVATE h5dwrite_ckind_rank_1
  PRIVATE h5dwrite_ckind_rank_2
  PRIVATE h5dwrite_ckind_rank_3
  PRIVATE h5dwrite_ckind_rank_4
  PRIVATE h5dwrite_ckind_rank_5
  PRIVATE h5dwrite_ckind_rank_6
  PRIVATE h5dwrite_ckind_rank_7
  PRIVATE h5dread_rkind_4_rank_0
  PRIVATE h5dread_rkind_4_rank_1
  PRIVATE h5dread_rkind_4_rank_2
  PRIVATE h5dread_rkind_4_rank_3
  PRIVATE h5dread_rkind_4_rank_4
  PRIVATE h5dread_rkind_4_rank_5
  PRIVATE h5dread_rkind_4_rank_6
  PRIVATE h5dread_rkind_4_rank_7
  PRIVATE h5dread_rkind_8_rank_0
  PRIVATE h5dread_rkind_8_rank_1
  PRIVATE h5dread_rkind_8_rank_2
  PRIVATE h5dread_rkind_8_rank_3
  PRIVATE h5dread_rkind_8_rank_4
  PRIVATE h5dread_rkind_8_rank_5
  PRIVATE h5dread_rkind_8_rank_6
  PRIVATE h5dread_rkind_8_rank_7
  PRIVATE h5dread_rkind_10_rank_0
  PRIVATE h5dread_rkind_10_rank_1
  PRIVATE h5dread_rkind_10_rank_2
  PRIVATE h5dread_rkind_10_rank_3
  PRIVATE h5dread_rkind_10_rank_4
  PRIVATE h5dread_rkind_10_rank_5
  PRIVATE h5dread_rkind_10_rank_6
  PRIVATE h5dread_rkind_10_rank_7
  PRIVATE h5dread_rkind_16_rank_0
  PRIVATE h5dread_rkind_16_rank_1
  PRIVATE h5dread_rkind_16_rank_2
  PRIVATE h5dread_rkind_16_rank_3
  PRIVATE h5dread_rkind_16_rank_4
  PRIVATE h5dread_rkind_16_rank_5
  PRIVATE h5dread_rkind_16_rank_6
  PRIVATE h5dread_rkind_16_rank_7
  PRIVATE h5dread_ikind_1_rank_0
  PRIVATE h5dread_ikind_1_rank_1
  PRIVATE h5dread_ikind_1_rank_2
  PRIVATE h5dread_ikind_1_rank_3
  PRIVATE h5dread_ikind_1_rank_4
  PRIVATE h5dread_ikind_1_rank_5
  PRIVATE h5dread_ikind_1_rank_6
  PRIVATE h5dread_ikind_1_rank_7
  PRIVATE h5dread_ikind_2_rank_0
  PRIVATE h5dread_ikind_2_rank_1
  PRIVATE h5dread_ikind_2_rank_2
  PRIVATE h5dread_ikind_2_rank_3
  PRIVATE h5dread_ikind_2_rank_4
  PRIVATE h5dread_ikind_2_rank_5
  PRIVATE h5dread_ikind_2_rank_6
  PRIVATE h5dread_ikind_2_rank_7
  PRIVATE h5dread_ikind_4_rank_0
  PRIVATE h5dread_ikind_4_rank_1
  PRIVATE h5dread_ikind_4_rank_2
  PRIVATE h5dread_ikind_4_rank_3
  PRIVATE h5dread_ikind_4_rank_4
  PRIVATE h5dread_ikind_4_rank_5
  PRIVATE h5dread_ikind_4_rank_6
  PRIVATE h5dread_ikind_4_rank_7
  PRIVATE h5dread_ikind_8_rank_0
  PRIVATE h5dread_ikind_8_rank_1
  PRIVATE h5dread_ikind_8_rank_2
  PRIVATE h5dread_ikind_8_rank_3
  PRIVATE h5dread_ikind_8_rank_4
  PRIVATE h5dread_ikind_8_rank_5
  PRIVATE h5dread_ikind_8_rank_6
  PRIVATE h5dread_ikind_8_rank_7
  PRIVATE h5dread_ikind_16_rank_0
  PRIVATE h5dread_ikind_16_rank_1
  PRIVATE h5dread_ikind_16_rank_2
  PRIVATE h5dread_ikind_16_rank_3
  PRIVATE h5dread_ikind_16_rank_4
  PRIVATE h5dread_ikind_16_rank_5
  PRIVATE h5dread_ikind_16_rank_6
  PRIVATE h5dread_ikind_16_rank_7
  PRIVATE h5dread_ckind_rank_1
  PRIVATE h5dread_ckind_rank_2
  PRIVATE h5dread_ckind_rank_3
  PRIVATE h5dread_ckind_rank_4
  PRIVATE h5dread_ckind_rank_5
  PRIVATE h5dread_ckind_rank_6
  PRIVATE h5dread_ckind_rank_7
  PRIVATE h5pset_fill_value_kind_4
  PRIVATE h5pset_fill_value_kind_8
  PRIVATE h5pset_fill_value_kind_10
  PRIVATE h5pset_fill_value_kind_16
  PRIVATE h5pget_fill_value_kind_4
  PRIVATE h5pget_fill_value_kind_8
  PRIVATE h5pget_fill_value_kind_10
  PRIVATE h5pget_fill_value_kind_16
  PRIVATE h5pset_kind_4
  PRIVATE h5pset_kind_8
  PRIVATE h5pset_kind_10
  PRIVATE h5pset_kind_16
  PRIVATE h5pget_kind_4
  PRIVATE h5pget_kind_8
  PRIVATE h5pget_kind_10
  PRIVATE h5pget_kind_16
  PRIVATE h5pregister_kind_4
  PRIVATE h5pregister_kind_8
  PRIVATE h5pregister_kind_10
  PRIVATE h5pregister_kind_16
  PRIVATE h5pinsert_kind_4
  PRIVATE h5pinsert_kind_8
  PRIVATE h5pinsert_kind_10
  PRIVATE h5pinsert_kind_16
  INTERFACE h5awrite_f
     MODULE PROCEDURE h5awrite_rkind_4_rank_0
     MODULE PROCEDURE h5awrite_rkind_4_rank_1
     MODULE PROCEDURE h5awrite_rkind_4_rank_2
     MODULE PROCEDURE h5awrite_rkind_4_rank_3
     MODULE PROCEDURE h5awrite_rkind_4_rank_4
     MODULE PROCEDURE h5awrite_rkind_4_rank_5
     MODULE PROCEDURE h5awrite_rkind_4_rank_6
     MODULE PROCEDURE h5awrite_rkind_4_rank_7
     MODULE PROCEDURE h5awrite_rkind_8_rank_0
     MODULE PROCEDURE h5awrite_rkind_8_rank_1
     MODULE PROCEDURE h5awrite_rkind_8_rank_2
     MODULE PROCEDURE h5awrite_rkind_8_rank_3
     MODULE PROCEDURE h5awrite_rkind_8_rank_4
     MODULE PROCEDURE h5awrite_rkind_8_rank_5
     MODULE PROCEDURE h5awrite_rkind_8_rank_6
     MODULE PROCEDURE h5awrite_rkind_8_rank_7
     MODULE PROCEDURE h5awrite_rkind_10_rank_0
     MODULE PROCEDURE h5awrite_rkind_10_rank_1
     MODULE PROCEDURE h5awrite_rkind_10_rank_2
     MODULE PROCEDURE h5awrite_rkind_10_rank_3
     MODULE PROCEDURE h5awrite_rkind_10_rank_4
     MODULE PROCEDURE h5awrite_rkind_10_rank_5
     MODULE PROCEDURE h5awrite_rkind_10_rank_6
     MODULE PROCEDURE h5awrite_rkind_10_rank_7
     MODULE PROCEDURE h5awrite_rkind_16_rank_0
     MODULE PROCEDURE h5awrite_rkind_16_rank_1
     MODULE PROCEDURE h5awrite_rkind_16_rank_2
     MODULE PROCEDURE h5awrite_rkind_16_rank_3
     MODULE PROCEDURE h5awrite_rkind_16_rank_4
     MODULE PROCEDURE h5awrite_rkind_16_rank_5
     MODULE PROCEDURE h5awrite_rkind_16_rank_6
     MODULE PROCEDURE h5awrite_rkind_16_rank_7
     MODULE PROCEDURE h5awrite_ikind_1_rank_0
     MODULE PROCEDURE h5awrite_ikind_1_rank_1
     MODULE PROCEDURE h5awrite_ikind_1_rank_2
     MODULE PROCEDURE h5awrite_ikind_1_rank_3
     MODULE PROCEDURE h5awrite_ikind_1_rank_4
     MODULE PROCEDURE h5awrite_ikind_1_rank_5
     MODULE PROCEDURE h5awrite_ikind_1_rank_6
     MODULE PROCEDURE h5awrite_ikind_1_rank_7
     MODULE PROCEDURE h5awrite_ikind_2_rank_0
     MODULE PROCEDURE h5awrite_ikind_2_rank_1
     MODULE PROCEDURE h5awrite_ikind_2_rank_2
     MODULE PROCEDURE h5awrite_ikind_2_rank_3
     MODULE PROCEDURE h5awrite_ikind_2_rank_4
     MODULE PROCEDURE h5awrite_ikind_2_rank_5
     MODULE PROCEDURE h5awrite_ikind_2_rank_6
     MODULE PROCEDURE h5awrite_ikind_2_rank_7
     MODULE PROCEDURE h5awrite_ikind_4_rank_0
     MODULE PROCEDURE h5awrite_ikind_4_rank_1
     MODULE PROCEDURE h5awrite_ikind_4_rank_2
     MODULE PROCEDURE h5awrite_ikind_4_rank_3
     MODULE PROCEDURE h5awrite_ikind_4_rank_4
     MODULE PROCEDURE h5awrite_ikind_4_rank_5
     MODULE PROCEDURE h5awrite_ikind_4_rank_6
     MODULE PROCEDURE h5awrite_ikind_4_rank_7
     MODULE PROCEDURE h5awrite_ikind_8_rank_0
     MODULE PROCEDURE h5awrite_ikind_8_rank_1
     MODULE PROCEDURE h5awrite_ikind_8_rank_2
     MODULE PROCEDURE h5awrite_ikind_8_rank_3
     MODULE PROCEDURE h5awrite_ikind_8_rank_4
     MODULE PROCEDURE h5awrite_ikind_8_rank_5
     MODULE PROCEDURE h5awrite_ikind_8_rank_6
     MODULE PROCEDURE h5awrite_ikind_8_rank_7
     MODULE PROCEDURE h5awrite_ikind_16_rank_0
     MODULE PROCEDURE h5awrite_ikind_16_rank_1
     MODULE PROCEDURE h5awrite_ikind_16_rank_2
     MODULE PROCEDURE h5awrite_ikind_16_rank_3
     MODULE PROCEDURE h5awrite_ikind_16_rank_4
     MODULE PROCEDURE h5awrite_ikind_16_rank_5
     MODULE PROCEDURE h5awrite_ikind_16_rank_6
     MODULE PROCEDURE h5awrite_ikind_16_rank_7
     MODULE PROCEDURE h5awrite_ckind_rank_1
     MODULE PROCEDURE h5awrite_ckind_rank_2
     MODULE PROCEDURE h5awrite_ckind_rank_3
     MODULE PROCEDURE h5awrite_ckind_rank_4
     MODULE PROCEDURE h5awrite_ckind_rank_5
     MODULE PROCEDURE h5awrite_ckind_rank_6
     MODULE PROCEDURE h5awrite_ckind_rank_7
  END INTERFACE
  INTERFACE h5aread_f
     MODULE PROCEDURE h5aread_rkind_4_rank_0
     MODULE PROCEDURE h5aread_rkind_4_rank_1
     MODULE PROCEDURE h5aread_rkind_4_rank_2
     MODULE PROCEDURE h5aread_rkind_4_rank_3
     MODULE PROCEDURE h5aread_rkind_4_rank_4
     MODULE PROCEDURE h5aread_rkind_4_rank_5
     MODULE PROCEDURE h5aread_rkind_4_rank_6
     MODULE PROCEDURE h5aread_rkind_4_rank_7
     MODULE PROCEDURE h5aread_rkind_8_rank_0
     MODULE PROCEDURE h5aread_rkind_8_rank_1
     MODULE PROCEDURE h5aread_rkind_8_rank_2
     MODULE PROCEDURE h5aread_rkind_8_rank_3
     MODULE PROCEDURE h5aread_rkind_8_rank_4
     MODULE PROCEDURE h5aread_rkind_8_rank_5
     MODULE PROCEDURE h5aread_rkind_8_rank_6
     MODULE PROCEDURE h5aread_rkind_8_rank_7
     MODULE PROCEDURE h5aread_rkind_10_rank_0
     MODULE PROCEDURE h5aread_rkind_10_rank_1
     MODULE PROCEDURE h5aread_rkind_10_rank_2
     MODULE PROCEDURE h5aread_rkind_10_rank_3
     MODULE PROCEDURE h5aread_rkind_10_rank_4
     MODULE PROCEDURE h5aread_rkind_10_rank_5
     MODULE PROCEDURE h5aread_rkind_10_rank_6
     MODULE PROCEDURE h5aread_rkind_10_rank_7
     MODULE PROCEDURE h5aread_rkind_16_rank_0
     MODULE PROCEDURE h5aread_rkind_16_rank_1
     MODULE PROCEDURE h5aread_rkind_16_rank_2
     MODULE PROCEDURE h5aread_rkind_16_rank_3
     MODULE PROCEDURE h5aread_rkind_16_rank_4
     MODULE PROCEDURE h5aread_rkind_16_rank_5
     MODULE PROCEDURE h5aread_rkind_16_rank_6
     MODULE PROCEDURE h5aread_rkind_16_rank_7
     MODULE PROCEDURE h5aread_ikind_1_rank_0
     MODULE PROCEDURE h5aread_ikind_1_rank_1
     MODULE PROCEDURE h5aread_ikind_1_rank_2
     MODULE PROCEDURE h5aread_ikind_1_rank_3
     MODULE PROCEDURE h5aread_ikind_1_rank_4
     MODULE PROCEDURE h5aread_ikind_1_rank_5
     MODULE PROCEDURE h5aread_ikind_1_rank_6
     MODULE PROCEDURE h5aread_ikind_1_rank_7
     MODULE PROCEDURE h5aread_ikind_2_rank_0
     MODULE PROCEDURE h5aread_ikind_2_rank_1
     MODULE PROCEDURE h5aread_ikind_2_rank_2
     MODULE PROCEDURE h5aread_ikind_2_rank_3
     MODULE PROCEDURE h5aread_ikind_2_rank_4
     MODULE PROCEDURE h5aread_ikind_2_rank_5
     MODULE PROCEDURE h5aread_ikind_2_rank_6
     MODULE PROCEDURE h5aread_ikind_2_rank_7
     MODULE PROCEDURE h5aread_ikind_4_rank_0
     MODULE PROCEDURE h5aread_ikind_4_rank_1
     MODULE PROCEDURE h5aread_ikind_4_rank_2
     MODULE PROCEDURE h5aread_ikind_4_rank_3
     MODULE PROCEDURE h5aread_ikind_4_rank_4
     MODULE PROCEDURE h5aread_ikind_4_rank_5
     MODULE PROCEDURE h5aread_ikind_4_rank_6
     MODULE PROCEDURE h5aread_ikind_4_rank_7
     MODULE PROCEDURE h5aread_ikind_8_rank_0
     MODULE PROCEDURE h5aread_ikind_8_rank_1
     MODULE PROCEDURE h5aread_ikind_8_rank_2
     MODULE PROCEDURE h5aread_ikind_8_rank_3
     MODULE PROCEDURE h5aread_ikind_8_rank_4
     MODULE PROCEDURE h5aread_ikind_8_rank_5
     MODULE PROCEDURE h5aread_ikind_8_rank_6
     MODULE PROCEDURE h5aread_ikind_8_rank_7
     MODULE PROCEDURE h5aread_ikind_16_rank_0
     MODULE PROCEDURE h5aread_ikind_16_rank_1
     MODULE PROCEDURE h5aread_ikind_16_rank_2
     MODULE PROCEDURE h5aread_ikind_16_rank_3
     MODULE PROCEDURE h5aread_ikind_16_rank_4
     MODULE PROCEDURE h5aread_ikind_16_rank_5
     MODULE PROCEDURE h5aread_ikind_16_rank_6
     MODULE PROCEDURE h5aread_ikind_16_rank_7
     MODULE PROCEDURE h5aread_ckind_rank_1
     MODULE PROCEDURE h5aread_ckind_rank_2
     MODULE PROCEDURE h5aread_ckind_rank_3
     MODULE PROCEDURE h5aread_ckind_rank_4
     MODULE PROCEDURE h5aread_ckind_rank_5
     MODULE PROCEDURE h5aread_ckind_rank_6
     MODULE PROCEDURE h5aread_ckind_rank_7
  END INTERFACE
  INTERFACE h5dwrite_f
     MODULE PROCEDURE h5dwrite_rkind_4_rank_0
     MODULE PROCEDURE h5dwrite_rkind_4_rank_1
     MODULE PROCEDURE h5dwrite_rkind_4_rank_2
     MODULE PROCEDURE h5dwrite_rkind_4_rank_3
     MODULE PROCEDURE h5dwrite_rkind_4_rank_4
     MODULE PROCEDURE h5dwrite_rkind_4_rank_5
     MODULE PROCEDURE h5dwrite_rkind_4_rank_6
     MODULE PROCEDURE h5dwrite_rkind_4_rank_7
     MODULE PROCEDURE h5dwrite_rkind_8_rank_0
     MODULE PROCEDURE h5dwrite_rkind_8_rank_1
     MODULE PROCEDURE h5dwrite_rkind_8_rank_2
     MODULE PROCEDURE h5dwrite_rkind_8_rank_3
     MODULE PROCEDURE h5dwrite_rkind_8_rank_4
     MODULE PROCEDURE h5dwrite_rkind_8_rank_5
     MODULE PROCEDURE h5dwrite_rkind_8_rank_6
     MODULE PROCEDURE h5dwrite_rkind_8_rank_7
     MODULE PROCEDURE h5dwrite_rkind_10_rank_0
     MODULE PROCEDURE h5dwrite_rkind_10_rank_1
     MODULE PROCEDURE h5dwrite_rkind_10_rank_2
     MODULE PROCEDURE h5dwrite_rkind_10_rank_3
     MODULE PROCEDURE h5dwrite_rkind_10_rank_4
     MODULE PROCEDURE h5dwrite_rkind_10_rank_5
     MODULE PROCEDURE h5dwrite_rkind_10_rank_6
     MODULE PROCEDURE h5dwrite_rkind_10_rank_7
     MODULE PROCEDURE h5dwrite_rkind_16_rank_0
     MODULE PROCEDURE h5dwrite_rkind_16_rank_1
     MODULE PROCEDURE h5dwrite_rkind_16_rank_2
     MODULE PROCEDURE h5dwrite_rkind_16_rank_3
     MODULE PROCEDURE h5dwrite_rkind_16_rank_4
     MODULE PROCEDURE h5dwrite_rkind_16_rank_5
     MODULE PROCEDURE h5dwrite_rkind_16_rank_6
     MODULE PROCEDURE h5dwrite_rkind_16_rank_7
     MODULE PROCEDURE h5dwrite_ikind_1_rank_0
     MODULE PROCEDURE h5dwrite_ikind_1_rank_1
     MODULE PROCEDURE h5dwrite_ikind_1_rank_2
     MODULE PROCEDURE h5dwrite_ikind_1_rank_3
     MODULE PROCEDURE h5dwrite_ikind_1_rank_4
     MODULE PROCEDURE h5dwrite_ikind_1_rank_5
     MODULE PROCEDURE h5dwrite_ikind_1_rank_6
     MODULE PROCEDURE h5dwrite_ikind_1_rank_7
     MODULE PROCEDURE h5dwrite_ikind_2_rank_0
     MODULE PROCEDURE h5dwrite_ikind_2_rank_1
     MODULE PROCEDURE h5dwrite_ikind_2_rank_2
     MODULE PROCEDURE h5dwrite_ikind_2_rank_3
     MODULE PROCEDURE h5dwrite_ikind_2_rank_4
     MODULE PROCEDURE h5dwrite_ikind_2_rank_5
     MODULE PROCEDURE h5dwrite_ikind_2_rank_6
     MODULE PROCEDURE h5dwrite_ikind_2_rank_7
     MODULE PROCEDURE h5dwrite_ikind_4_rank_0
     MODULE PROCEDURE h5dwrite_ikind_4_rank_1
     MODULE PROCEDURE h5dwrite_ikind_4_rank_2
     MODULE PROCEDURE h5dwrite_ikind_4_rank_3
     MODULE PROCEDURE h5dwrite_ikind_4_rank_4
     MODULE PROCEDURE h5dwrite_ikind_4_rank_5
     MODULE PROCEDURE h5dwrite_ikind_4_rank_6
     MODULE PROCEDURE h5dwrite_ikind_4_rank_7
     MODULE PROCEDURE h5dwrite_ikind_8_rank_0
     MODULE PROCEDURE h5dwrite_ikind_8_rank_1
     MODULE PROCEDURE h5dwrite_ikind_8_rank_2
     MODULE PROCEDURE h5dwrite_ikind_8_rank_3
     MODULE PROCEDURE h5dwrite_ikind_8_rank_4
     MODULE PROCEDURE h5dwrite_ikind_8_rank_5
     MODULE PROCEDURE h5dwrite_ikind_8_rank_6
     MODULE PROCEDURE h5dwrite_ikind_8_rank_7
     MODULE PROCEDURE h5dwrite_ikind_16_rank_0
     MODULE PROCEDURE h5dwrite_ikind_16_rank_1
     MODULE PROCEDURE h5dwrite_ikind_16_rank_2
     MODULE PROCEDURE h5dwrite_ikind_16_rank_3
     MODULE PROCEDURE h5dwrite_ikind_16_rank_4
     MODULE PROCEDURE h5dwrite_ikind_16_rank_5
     MODULE PROCEDURE h5dwrite_ikind_16_rank_6
     MODULE PROCEDURE h5dwrite_ikind_16_rank_7
     MODULE PROCEDURE h5dwrite_ckind_rank_1
     MODULE PROCEDURE h5dwrite_ckind_rank_2
     MODULE PROCEDURE h5dwrite_ckind_rank_3
     MODULE PROCEDURE h5dwrite_ckind_rank_4
     MODULE PROCEDURE h5dwrite_ckind_rank_5
     MODULE PROCEDURE h5dwrite_ckind_rank_6
     MODULE PROCEDURE h5dwrite_ckind_rank_7
  END INTERFACE
  INTERFACE h5dread_f
     MODULE PROCEDURE h5dread_rkind_4_rank_0
     MODULE PROCEDURE h5dread_rkind_4_rank_1
     MODULE PROCEDURE h5dread_rkind_4_rank_2
     MODULE PROCEDURE h5dread_rkind_4_rank_3
     MODULE PROCEDURE h5dread_rkind_4_rank_4
     MODULE PROCEDURE h5dread_rkind_4_rank_5
     MODULE PROCEDURE h5dread_rkind_4_rank_6
     MODULE PROCEDURE h5dread_rkind_4_rank_7
     MODULE PROCEDURE h5dread_rkind_8_rank_0
     MODULE PROCEDURE h5dread_rkind_8_rank_1
     MODULE PROCEDURE h5dread_rkind_8_rank_2
     MODULE PROCEDURE h5dread_rkind_8_rank_3
     MODULE PROCEDURE h5dread_rkind_8_rank_4
     MODULE PROCEDURE h5dread_rkind_8_rank_5
     MODULE PROCEDURE h5dread_rkind_8_rank_6
     MODULE PROCEDURE h5dread_rkind_8_rank_7
     MODULE PROCEDURE h5dread_rkind_10_rank_0
     MODULE PROCEDURE h5dread_rkind_10_rank_1
     MODULE PROCEDURE h5dread_rkind_10_rank_2
     MODULE PROCEDURE h5dread_rkind_10_rank_3
     MODULE PROCEDURE h5dread_rkind_10_rank_4
     MODULE PROCEDURE h5dread_rkind_10_rank_5
     MODULE PROCEDURE h5dread_rkind_10_rank_6
     MODULE PROCEDURE h5dread_rkind_10_rank_7
     MODULE PROCEDURE h5dread_rkind_16_rank_0
     MODULE PROCEDURE h5dread_rkind_16_rank_1
     MODULE PROCEDURE h5dread_rkind_16_rank_2
     MODULE PROCEDURE h5dread_rkind_16_rank_3
     MODULE PROCEDURE h5dread_rkind_16_rank_4
     MODULE PROCEDURE h5dread_rkind_16_rank_5
     MODULE PROCEDURE h5dread_rkind_16_rank_6
     MODULE PROCEDURE h5dread_rkind_16_rank_7
     MODULE PROCEDURE h5dread_ikind_1_rank_0
     MODULE PROCEDURE h5dread_ikind_1_rank_1
     MODULE PROCEDURE h5dread_ikind_1_rank_2
     MODULE PROCEDURE h5dread_ikind_1_rank_3
     MODULE PROCEDURE h5dread_ikind_1_rank_4
     MODULE PROCEDURE h5dread_ikind_1_rank_5
     MODULE PROCEDURE h5dread_ikind_1_rank_6
     MODULE PROCEDURE h5dread_ikind_1_rank_7
     MODULE PROCEDURE h5dread_ikind_2_rank_0
     MODULE PROCEDURE h5dread_ikind_2_rank_1
     MODULE PROCEDURE h5dread_ikind_2_rank_2
     MODULE PROCEDURE h5dread_ikind_2_rank_3
     MODULE PROCEDURE h5dread_ikind_2_rank_4
     MODULE PROCEDURE h5dread_ikind_2_rank_5
     MODULE PROCEDURE h5dread_ikind_2_rank_6
     MODULE PROCEDURE h5dread_ikind_2_rank_7
     MODULE PROCEDURE h5dread_ikind_4_rank_0
     MODULE PROCEDURE h5dread_ikind_4_rank_1
     MODULE PROCEDURE h5dread_ikind_4_rank_2
     MODULE PROCEDURE h5dread_ikind_4_rank_3
     MODULE PROCEDURE h5dread_ikind_4_rank_4
     MODULE PROCEDURE h5dread_ikind_4_rank_5
     MODULE PROCEDURE h5dread_ikind_4_rank_6
     MODULE PROCEDURE h5dread_ikind_4_rank_7
     MODULE PROCEDURE h5dread_ikind_8_rank_0
     MODULE PROCEDURE h5dread_ikind_8_rank_1
     MODULE PROCEDURE h5dread_ikind_8_rank_2
     MODULE PROCEDURE h5dread_ikind_8_rank_3
     MODULE PROCEDURE h5dread_ikind_8_rank_4
     MODULE PROCEDURE h5dread_ikind_8_rank_5
     MODULE PROCEDURE h5dread_ikind_8_rank_6
     MODULE PROCEDURE h5dread_ikind_8_rank_7
     MODULE PROCEDURE h5dread_ikind_16_rank_0
     MODULE PROCEDURE h5dread_ikind_16_rank_1
     MODULE PROCEDURE h5dread_ikind_16_rank_2
     MODULE PROCEDURE h5dread_ikind_16_rank_3
     MODULE PROCEDURE h5dread_ikind_16_rank_4
     MODULE PROCEDURE h5dread_ikind_16_rank_5
     MODULE PROCEDURE h5dread_ikind_16_rank_6
     MODULE PROCEDURE h5dread_ikind_16_rank_7
     MODULE PROCEDURE h5dread_ckind_rank_1
     MODULE PROCEDURE h5dread_ckind_rank_2
     MODULE PROCEDURE h5dread_ckind_rank_3
     MODULE PROCEDURE h5dread_ckind_rank_4
     MODULE PROCEDURE h5dread_ckind_rank_5
     MODULE PROCEDURE h5dread_ckind_rank_6
     MODULE PROCEDURE h5dread_ckind_rank_7
  END INTERFACE
  INTERFACE h5pset_fill_value_f
     MODULE PROCEDURE h5pset_fill_value_kind_4
     MODULE PROCEDURE h5pset_fill_value_kind_8
     MODULE PROCEDURE h5pset_fill_value_kind_10
     MODULE PROCEDURE h5pset_fill_value_kind_16
  END INTERFACE
  INTERFACE h5pget_fill_value_f
     MODULE PROCEDURE h5pget_fill_value_kind_4
     MODULE PROCEDURE h5pget_fill_value_kind_8
     MODULE PROCEDURE h5pget_fill_value_kind_10
     MODULE PROCEDURE h5pget_fill_value_kind_16
  END INTERFACE
  INTERFACE h5pset_f
     MODULE PROCEDURE h5pset_kind_4
     MODULE PROCEDURE h5pset_kind_8
     MODULE PROCEDURE h5pset_kind_10
     MODULE PROCEDURE h5pset_kind_16
  END INTERFACE
  INTERFACE h5pget_f
     MODULE PROCEDURE h5pget_kind_4
     MODULE PROCEDURE h5pget_kind_8
     MODULE PROCEDURE h5pget_kind_10
     MODULE PROCEDURE h5pget_kind_16
  END INTERFACE
  INTERFACE h5pregister_f
     MODULE PROCEDURE h5pregister_kind_4
     MODULE PROCEDURE h5pregister_kind_8
     MODULE PROCEDURE h5pregister_kind_10
     MODULE PROCEDURE h5pregister_kind_16
  END INTERFACE
  INTERFACE h5pinsert_f
     MODULE PROCEDURE h5pinsert_kind_4
     MODULE PROCEDURE h5pinsert_kind_8
     MODULE PROCEDURE h5pinsert_kind_10
     MODULE PROCEDURE h5pinsert_kind_16
  END INTERFACE
CONTAINS
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_4_rank_0
!DEC$endif
  SUBROUTINE h5awrite_rkind_4_rank_0(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(IN), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf)               
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_4_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_4_rank_1
!DEC$endif
  SUBROUTINE h5awrite_rkind_4_rank_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1))            
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_4_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_4_rank_2
!DEC$endif
  SUBROUTINE h5awrite_rkind_4_rank_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1))          
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_4_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_4_rank_3
!DEC$endif
  SUBROUTINE h5awrite_rkind_4_rank_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_4_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_4_rank_4
!DEC$endif
  SUBROUTINE h5awrite_rkind_4_rank_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_4_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_4_rank_5
!DEC$endif
  SUBROUTINE h5awrite_rkind_4_rank_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_4_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_4_rank_6
!DEC$endif
  SUBROUTINE h5awrite_rkind_4_rank_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_4_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_4_rank_7
!DEC$endif
  SUBROUTINE h5awrite_rkind_4_rank_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_4_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_8_rank_0
!DEC$endif
  SUBROUTINE h5awrite_rkind_8_rank_0(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(IN), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf)               
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_8_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_8_rank_1
!DEC$endif
  SUBROUTINE h5awrite_rkind_8_rank_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1))            
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_8_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_8_rank_2
!DEC$endif
  SUBROUTINE h5awrite_rkind_8_rank_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1))          
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_8_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_8_rank_3
!DEC$endif
  SUBROUTINE h5awrite_rkind_8_rank_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_8_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_8_rank_4
!DEC$endif
  SUBROUTINE h5awrite_rkind_8_rank_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_8_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_8_rank_5
!DEC$endif
  SUBROUTINE h5awrite_rkind_8_rank_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_8_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_8_rank_6
!DEC$endif
  SUBROUTINE h5awrite_rkind_8_rank_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_8_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_8_rank_7
!DEC$endif
  SUBROUTINE h5awrite_rkind_8_rank_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_8_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_10_rank_0
!DEC$endif
  SUBROUTINE h5awrite_rkind_10_rank_0(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(IN), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf)               
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_10_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_10_rank_1
!DEC$endif
  SUBROUTINE h5awrite_rkind_10_rank_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1))            
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_10_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_10_rank_2
!DEC$endif
  SUBROUTINE h5awrite_rkind_10_rank_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1))          
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_10_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_10_rank_3
!DEC$endif
  SUBROUTINE h5awrite_rkind_10_rank_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_10_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_10_rank_4
!DEC$endif
  SUBROUTINE h5awrite_rkind_10_rank_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_10_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_10_rank_5
!DEC$endif
  SUBROUTINE h5awrite_rkind_10_rank_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_10_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_10_rank_6
!DEC$endif
  SUBROUTINE h5awrite_rkind_10_rank_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_10_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_10_rank_7
!DEC$endif
  SUBROUTINE h5awrite_rkind_10_rank_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_10_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_16_rank_0
!DEC$endif
  SUBROUTINE h5awrite_rkind_16_rank_0(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(IN), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf)               
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_16_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_16_rank_1
!DEC$endif
  SUBROUTINE h5awrite_rkind_16_rank_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1))            
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_16_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_16_rank_2
!DEC$endif
  SUBROUTINE h5awrite_rkind_16_rank_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1))          
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_16_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_16_rank_3
!DEC$endif
  SUBROUTINE h5awrite_rkind_16_rank_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_16_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_16_rank_4
!DEC$endif
  SUBROUTINE h5awrite_rkind_16_rank_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_16_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_16_rank_5
!DEC$endif
  SUBROUTINE h5awrite_rkind_16_rank_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_16_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_16_rank_6
!DEC$endif
  SUBROUTINE h5awrite_rkind_16_rank_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_16_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_rkind_16_rank_7
!DEC$endif
  SUBROUTINE h5awrite_rkind_16_rank_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_rkind_16_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_1_rank_0
!DEC$endif
  SUBROUTINE h5awrite_ikind_1_rank_0(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(IN), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf)               
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_1_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_1_rank_1
!DEC$endif
  SUBROUTINE h5awrite_ikind_1_rank_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1))            
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_1_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_1_rank_2
!DEC$endif
  SUBROUTINE h5awrite_ikind_1_rank_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1))          
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_1_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_1_rank_3
!DEC$endif
  SUBROUTINE h5awrite_ikind_1_rank_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_1_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_1_rank_4
!DEC$endif
  SUBROUTINE h5awrite_ikind_1_rank_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_1_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_1_rank_5
!DEC$endif
  SUBROUTINE h5awrite_ikind_1_rank_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_1_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_1_rank_6
!DEC$endif
  SUBROUTINE h5awrite_ikind_1_rank_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_1_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_1_rank_7
!DEC$endif
  SUBROUTINE h5awrite_ikind_1_rank_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_1_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_2_rank_0
!DEC$endif
  SUBROUTINE h5awrite_ikind_2_rank_0(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(IN), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf)               
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_2_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_2_rank_1
!DEC$endif
  SUBROUTINE h5awrite_ikind_2_rank_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1))            
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_2_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_2_rank_2
!DEC$endif
  SUBROUTINE h5awrite_ikind_2_rank_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1))          
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_2_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_2_rank_3
!DEC$endif
  SUBROUTINE h5awrite_ikind_2_rank_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_2_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_2_rank_4
!DEC$endif
  SUBROUTINE h5awrite_ikind_2_rank_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_2_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_2_rank_5
!DEC$endif
  SUBROUTINE h5awrite_ikind_2_rank_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_2_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_2_rank_6
!DEC$endif
  SUBROUTINE h5awrite_ikind_2_rank_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_2_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_2_rank_7
!DEC$endif
  SUBROUTINE h5awrite_ikind_2_rank_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_2_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_4_rank_0
!DEC$endif
  SUBROUTINE h5awrite_ikind_4_rank_0(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(IN), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf)               
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_4_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_4_rank_1
!DEC$endif
  SUBROUTINE h5awrite_ikind_4_rank_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1))            
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_4_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_4_rank_2
!DEC$endif
  SUBROUTINE h5awrite_ikind_4_rank_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1))          
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_4_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_4_rank_3
!DEC$endif
  SUBROUTINE h5awrite_ikind_4_rank_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_4_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_4_rank_4
!DEC$endif
  SUBROUTINE h5awrite_ikind_4_rank_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_4_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_4_rank_5
!DEC$endif
  SUBROUTINE h5awrite_ikind_4_rank_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_4_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_4_rank_6
!DEC$endif
  SUBROUTINE h5awrite_ikind_4_rank_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_4_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_4_rank_7
!DEC$endif
  SUBROUTINE h5awrite_ikind_4_rank_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_4_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_8_rank_0
!DEC$endif
  SUBROUTINE h5awrite_ikind_8_rank_0(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(IN), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf)               
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_8_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_8_rank_1
!DEC$endif
  SUBROUTINE h5awrite_ikind_8_rank_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1))            
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_8_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_8_rank_2
!DEC$endif
  SUBROUTINE h5awrite_ikind_8_rank_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1))          
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_8_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_8_rank_3
!DEC$endif
  SUBROUTINE h5awrite_ikind_8_rank_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_8_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_8_rank_4
!DEC$endif
  SUBROUTINE h5awrite_ikind_8_rank_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_8_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_8_rank_5
!DEC$endif
  SUBROUTINE h5awrite_ikind_8_rank_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_8_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_8_rank_6
!DEC$endif
  SUBROUTINE h5awrite_ikind_8_rank_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_8_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_8_rank_7
!DEC$endif
  SUBROUTINE h5awrite_ikind_8_rank_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_8_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_16_rank_0
!DEC$endif
  SUBROUTINE h5awrite_ikind_16_rank_0(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(IN), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf)               
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_16_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_16_rank_1
!DEC$endif
  SUBROUTINE h5awrite_ikind_16_rank_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1))            
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_16_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_16_rank_2
!DEC$endif
  SUBROUTINE h5awrite_ikind_16_rank_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1))          
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_16_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_16_rank_3
!DEC$endif
  SUBROUTINE h5awrite_ikind_16_rank_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_16_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_16_rank_4
!DEC$endif
  SUBROUTINE h5awrite_ikind_16_rank_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_16_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_16_rank_5
!DEC$endif
  SUBROUTINE h5awrite_ikind_16_rank_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_16_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_16_rank_6
!DEC$endif
  SUBROUTINE h5awrite_ikind_16_rank_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_16_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ikind_16_rank_7
!DEC$endif
  SUBROUTINE h5awrite_ikind_16_rank_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ikind_16_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ckind_rank_1
!DEC$endif
  SUBROUTINE h5awrite_ckind_rank_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*)  , INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)(1:1))            
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ckind_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ckind_rank_2
!DEC$endif
  SUBROUTINE h5awrite_ckind_rank_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*)  , INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1)(1:1))          
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ckind_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ckind_rank_3
!DEC$endif
  SUBROUTINE h5awrite_ckind_rank_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*)  , INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1)(1:1))        
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ckind_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ckind_rank_4
!DEC$endif
  SUBROUTINE h5awrite_ckind_rank_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*)  , INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1)(1:1))      
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ckind_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ckind_rank_5
!DEC$endif
  SUBROUTINE h5awrite_ckind_rank_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*)  , INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1)(1:1))    
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ckind_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ckind_rank_6
!DEC$endif
  SUBROUTINE h5awrite_ckind_rank_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*)  , INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1)(1:1))  
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ckind_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5awrite_ckind_rank_7
!DEC$endif
  SUBROUTINE h5awrite_ckind_rank_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*)  , INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1)(1:1))
    hdferr = H5Awrite_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5awrite_ckind_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_4_rank_0
!DEC$endif
  SUBROUTINE h5aread_rkind_4_rank_0(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(INOUT), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf)               
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_4_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_4_rank_1
!DEC$endif
  SUBROUTINE h5aread_rkind_4_rank_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1))            
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_4_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_4_rank_2
!DEC$endif
  SUBROUTINE h5aread_rkind_4_rank_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1))          
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_4_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_4_rank_3
!DEC$endif
  SUBROUTINE h5aread_rkind_4_rank_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_4_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_4_rank_4
!DEC$endif
  SUBROUTINE h5aread_rkind_4_rank_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_4_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_4_rank_5
!DEC$endif
  SUBROUTINE h5aread_rkind_4_rank_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_4_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_4_rank_6
!DEC$endif
  SUBROUTINE h5aread_rkind_4_rank_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_4_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_4_rank_7
!DEC$endif
  SUBROUTINE h5aread_rkind_4_rank_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_4_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_8_rank_0
!DEC$endif
  SUBROUTINE h5aread_rkind_8_rank_0(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(INOUT), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf)               
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_8_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_8_rank_1
!DEC$endif
  SUBROUTINE h5aread_rkind_8_rank_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1))            
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_8_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_8_rank_2
!DEC$endif
  SUBROUTINE h5aread_rkind_8_rank_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1))          
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_8_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_8_rank_3
!DEC$endif
  SUBROUTINE h5aread_rkind_8_rank_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_8_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_8_rank_4
!DEC$endif
  SUBROUTINE h5aread_rkind_8_rank_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_8_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_8_rank_5
!DEC$endif
  SUBROUTINE h5aread_rkind_8_rank_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_8_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_8_rank_6
!DEC$endif
  SUBROUTINE h5aread_rkind_8_rank_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_8_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_8_rank_7
!DEC$endif
  SUBROUTINE h5aread_rkind_8_rank_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_8_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_10_rank_0
!DEC$endif
  SUBROUTINE h5aread_rkind_10_rank_0(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(INOUT), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf)               
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_10_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_10_rank_1
!DEC$endif
  SUBROUTINE h5aread_rkind_10_rank_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1))            
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_10_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_10_rank_2
!DEC$endif
  SUBROUTINE h5aread_rkind_10_rank_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1))          
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_10_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_10_rank_3
!DEC$endif
  SUBROUTINE h5aread_rkind_10_rank_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_10_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_10_rank_4
!DEC$endif
  SUBROUTINE h5aread_rkind_10_rank_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_10_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_10_rank_5
!DEC$endif
  SUBROUTINE h5aread_rkind_10_rank_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_10_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_10_rank_6
!DEC$endif
  SUBROUTINE h5aread_rkind_10_rank_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_10_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_10_rank_7
!DEC$endif
  SUBROUTINE h5aread_rkind_10_rank_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_10_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_16_rank_0
!DEC$endif
  SUBROUTINE h5aread_rkind_16_rank_0(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(INOUT), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf)               
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_16_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_16_rank_1
!DEC$endif
  SUBROUTINE h5aread_rkind_16_rank_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1))            
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_16_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_16_rank_2
!DEC$endif
  SUBROUTINE h5aread_rkind_16_rank_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1))          
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_16_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_16_rank_3
!DEC$endif
  SUBROUTINE h5aread_rkind_16_rank_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_16_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_16_rank_4
!DEC$endif
  SUBROUTINE h5aread_rkind_16_rank_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_16_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_16_rank_5
!DEC$endif
  SUBROUTINE h5aread_rkind_16_rank_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_16_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_16_rank_6
!DEC$endif
  SUBROUTINE h5aread_rkind_16_rank_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_16_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_rkind_16_rank_7
!DEC$endif
  SUBROUTINE h5aread_rkind_16_rank_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_rkind_16_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_1_rank_0
!DEC$endif
  SUBROUTINE h5aread_ikind_1_rank_0(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(INOUT), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf)               
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_1_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_1_rank_1
!DEC$endif
  SUBROUTINE h5aread_ikind_1_rank_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1))            
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_1_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_1_rank_2
!DEC$endif
  SUBROUTINE h5aread_ikind_1_rank_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1))          
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_1_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_1_rank_3
!DEC$endif
  SUBROUTINE h5aread_ikind_1_rank_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_1_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_1_rank_4
!DEC$endif
  SUBROUTINE h5aread_ikind_1_rank_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_1_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_1_rank_5
!DEC$endif
  SUBROUTINE h5aread_ikind_1_rank_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_1_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_1_rank_6
!DEC$endif
  SUBROUTINE h5aread_ikind_1_rank_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_1_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_1_rank_7
!DEC$endif
  SUBROUTINE h5aread_ikind_1_rank_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_1_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_2_rank_0
!DEC$endif
  SUBROUTINE h5aread_ikind_2_rank_0(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(INOUT), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf)               
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_2_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_2_rank_1
!DEC$endif
  SUBROUTINE h5aread_ikind_2_rank_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1))            
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_2_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_2_rank_2
!DEC$endif
  SUBROUTINE h5aread_ikind_2_rank_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1))          
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_2_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_2_rank_3
!DEC$endif
  SUBROUTINE h5aread_ikind_2_rank_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_2_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_2_rank_4
!DEC$endif
  SUBROUTINE h5aread_ikind_2_rank_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_2_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_2_rank_5
!DEC$endif
  SUBROUTINE h5aread_ikind_2_rank_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_2_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_2_rank_6
!DEC$endif
  SUBROUTINE h5aread_ikind_2_rank_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_2_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_2_rank_7
!DEC$endif
  SUBROUTINE h5aread_ikind_2_rank_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_2_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_4_rank_0
!DEC$endif
  SUBROUTINE h5aread_ikind_4_rank_0(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(INOUT), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf)               
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_4_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_4_rank_1
!DEC$endif
  SUBROUTINE h5aread_ikind_4_rank_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1))            
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_4_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_4_rank_2
!DEC$endif
  SUBROUTINE h5aread_ikind_4_rank_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1))          
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_4_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_4_rank_3
!DEC$endif
  SUBROUTINE h5aread_ikind_4_rank_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_4_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_4_rank_4
!DEC$endif
  SUBROUTINE h5aread_ikind_4_rank_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_4_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_4_rank_5
!DEC$endif
  SUBROUTINE h5aread_ikind_4_rank_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_4_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_4_rank_6
!DEC$endif
  SUBROUTINE h5aread_ikind_4_rank_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_4_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_4_rank_7
!DEC$endif
  SUBROUTINE h5aread_ikind_4_rank_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_4_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_8_rank_0
!DEC$endif
  SUBROUTINE h5aread_ikind_8_rank_0(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(INOUT), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf)               
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_8_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_8_rank_1
!DEC$endif
  SUBROUTINE h5aread_ikind_8_rank_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1))            
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_8_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_8_rank_2
!DEC$endif
  SUBROUTINE h5aread_ikind_8_rank_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1))          
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_8_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_8_rank_3
!DEC$endif
  SUBROUTINE h5aread_ikind_8_rank_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_8_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_8_rank_4
!DEC$endif
  SUBROUTINE h5aread_ikind_8_rank_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_8_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_8_rank_5
!DEC$endif
  SUBROUTINE h5aread_ikind_8_rank_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_8_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_8_rank_6
!DEC$endif
  SUBROUTINE h5aread_ikind_8_rank_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_8_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_8_rank_7
!DEC$endif
  SUBROUTINE h5aread_ikind_8_rank_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_8_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_16_rank_0
!DEC$endif
  SUBROUTINE h5aread_ikind_16_rank_0(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(INOUT), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf)               
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_16_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_16_rank_1
!DEC$endif
  SUBROUTINE h5aread_ikind_16_rank_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1))            
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_16_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_16_rank_2
!DEC$endif
  SUBROUTINE h5aread_ikind_16_rank_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1))          
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_16_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_16_rank_3
!DEC$endif
  SUBROUTINE h5aread_ikind_16_rank_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_16_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_16_rank_4
!DEC$endif
  SUBROUTINE h5aread_ikind_16_rank_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_16_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_16_rank_5
!DEC$endif
  SUBROUTINE h5aread_ikind_16_rank_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_16_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_16_rank_6
!DEC$endif
  SUBROUTINE h5aread_ikind_16_rank_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_16_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ikind_16_rank_7
!DEC$endif
  SUBROUTINE h5aread_ikind_16_rank_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ikind_16_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ckind_rank_1
!DEC$endif
  SUBROUTINE h5aread_ckind_rank_1(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1)(1:1))            
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ckind_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ckind_rank_2
!DEC$endif
  SUBROUTINE h5aread_ckind_rank_2(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1)(1:1))          
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ckind_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ckind_rank_3
!DEC$endif
  SUBROUTINE h5aread_ckind_rank_3(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1)(1:1))        
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ckind_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ckind_rank_4
!DEC$endif
  SUBROUTINE h5aread_ckind_rank_4(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1)(1:1))      
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ckind_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ckind_rank_5
!DEC$endif
  SUBROUTINE h5aread_ckind_rank_5(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1)(1:1))    
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ckind_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ckind_rank_6
!DEC$endif
  SUBROUTINE h5aread_ckind_rank_6(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1)(1:1))  
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ckind_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5aread_ckind_rank_7
!DEC$endif
  SUBROUTINE h5aread_ckind_rank_7(attr_id, memtype_id, buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T)    , INTENT(IN) :: attr_id
    INTEGER(HID_T)    , INTENT(IN) :: memtype_id
    INTEGER(HSIZE_T)  , INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER           , INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1)(1:1))
    hdferr = H5Aread_f_c(attr_id, memtype_id, f_ptr)
  END SUBROUTINE h5aread_ckind_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_4_rank_0
!DEC$endif
  SUBROUTINE h5dread_rkind_4_rank_0(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(INOUT), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)               
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_4_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_4_rank_1
!DEC$endif
  SUBROUTINE h5dread_rkind_4_rank_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))            
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_4_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_4_rank_2
!DEC$endif
  SUBROUTINE h5dread_rkind_4_rank_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))          
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_4_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_4_rank_3
!DEC$endif
  SUBROUTINE h5dread_rkind_4_rank_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_4_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_4_rank_4
!DEC$endif
  SUBROUTINE h5dread_rkind_4_rank_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_4_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_4_rank_5
!DEC$endif
  SUBROUTINE h5dread_rkind_4_rank_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_4_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_4_rank_6
!DEC$endif
  SUBROUTINE h5dread_rkind_4_rank_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_4_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_4_rank_7
!DEC$endif
  SUBROUTINE h5dread_rkind_4_rank_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_4_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_8_rank_0
!DEC$endif
  SUBROUTINE h5dread_rkind_8_rank_0(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(INOUT), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)               
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_8_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_8_rank_1
!DEC$endif
  SUBROUTINE h5dread_rkind_8_rank_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))            
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_8_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_8_rank_2
!DEC$endif
  SUBROUTINE h5dread_rkind_8_rank_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))          
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_8_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_8_rank_3
!DEC$endif
  SUBROUTINE h5dread_rkind_8_rank_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_8_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_8_rank_4
!DEC$endif
  SUBROUTINE h5dread_rkind_8_rank_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_8_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_8_rank_5
!DEC$endif
  SUBROUTINE h5dread_rkind_8_rank_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_8_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_8_rank_6
!DEC$endif
  SUBROUTINE h5dread_rkind_8_rank_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_8_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_8_rank_7
!DEC$endif
  SUBROUTINE h5dread_rkind_8_rank_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_8_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_10_rank_0
!DEC$endif
  SUBROUTINE h5dread_rkind_10_rank_0(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(INOUT), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)               
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_10_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_10_rank_1
!DEC$endif
  SUBROUTINE h5dread_rkind_10_rank_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))            
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_10_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_10_rank_2
!DEC$endif
  SUBROUTINE h5dread_rkind_10_rank_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))          
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_10_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_10_rank_3
!DEC$endif
  SUBROUTINE h5dread_rkind_10_rank_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_10_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_10_rank_4
!DEC$endif
  SUBROUTINE h5dread_rkind_10_rank_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_10_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_10_rank_5
!DEC$endif
  SUBROUTINE h5dread_rkind_10_rank_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_10_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_10_rank_6
!DEC$endif
  SUBROUTINE h5dread_rkind_10_rank_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_10_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_10_rank_7
!DEC$endif
  SUBROUTINE h5dread_rkind_10_rank_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_10_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_16_rank_0
!DEC$endif
  SUBROUTINE h5dread_rkind_16_rank_0(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(INOUT), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)               
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_16_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_16_rank_1
!DEC$endif
  SUBROUTINE h5dread_rkind_16_rank_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))            
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_16_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_16_rank_2
!DEC$endif
  SUBROUTINE h5dread_rkind_16_rank_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))          
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_16_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_16_rank_3
!DEC$endif
  SUBROUTINE h5dread_rkind_16_rank_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_16_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_16_rank_4
!DEC$endif
  SUBROUTINE h5dread_rkind_16_rank_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_16_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_16_rank_5
!DEC$endif
  SUBROUTINE h5dread_rkind_16_rank_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_16_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_16_rank_6
!DEC$endif
  SUBROUTINE h5dread_rkind_16_rank_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_16_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_rkind_16_rank_7
!DEC$endif
  SUBROUTINE h5dread_rkind_16_rank_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_rkind_16_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_1_rank_0
!DEC$endif
  SUBROUTINE h5dread_ikind_1_rank_0(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(INOUT), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)               
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_1_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_1_rank_1
!DEC$endif
  SUBROUTINE h5dread_ikind_1_rank_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))            
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_1_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_1_rank_2
!DEC$endif
  SUBROUTINE h5dread_ikind_1_rank_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))          
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_1_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_1_rank_3
!DEC$endif
  SUBROUTINE h5dread_ikind_1_rank_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_1_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_1_rank_4
!DEC$endif
  SUBROUTINE h5dread_ikind_1_rank_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_1_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_1_rank_5
!DEC$endif
  SUBROUTINE h5dread_ikind_1_rank_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_1_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_1_rank_6
!DEC$endif
  SUBROUTINE h5dread_ikind_1_rank_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_1_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_1_rank_7
!DEC$endif
  SUBROUTINE h5dread_ikind_1_rank_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_1_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_2_rank_0
!DEC$endif
  SUBROUTINE h5dread_ikind_2_rank_0(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(INOUT), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)               
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_2_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_2_rank_1
!DEC$endif
  SUBROUTINE h5dread_ikind_2_rank_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))            
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_2_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_2_rank_2
!DEC$endif
  SUBROUTINE h5dread_ikind_2_rank_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))          
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_2_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_2_rank_3
!DEC$endif
  SUBROUTINE h5dread_ikind_2_rank_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_2_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_2_rank_4
!DEC$endif
  SUBROUTINE h5dread_ikind_2_rank_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_2_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_2_rank_5
!DEC$endif
  SUBROUTINE h5dread_ikind_2_rank_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_2_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_2_rank_6
!DEC$endif
  SUBROUTINE h5dread_ikind_2_rank_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_2_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_2_rank_7
!DEC$endif
  SUBROUTINE h5dread_ikind_2_rank_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_2_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_4_rank_0
!DEC$endif
  SUBROUTINE h5dread_ikind_4_rank_0(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(INOUT), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)               
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_4_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_4_rank_1
!DEC$endif
  SUBROUTINE h5dread_ikind_4_rank_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))            
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_4_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_4_rank_2
!DEC$endif
  SUBROUTINE h5dread_ikind_4_rank_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))          
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_4_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_4_rank_3
!DEC$endif
  SUBROUTINE h5dread_ikind_4_rank_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_4_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_4_rank_4
!DEC$endif
  SUBROUTINE h5dread_ikind_4_rank_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_4_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_4_rank_5
!DEC$endif
  SUBROUTINE h5dread_ikind_4_rank_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_4_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_4_rank_6
!DEC$endif
  SUBROUTINE h5dread_ikind_4_rank_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_4_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_4_rank_7
!DEC$endif
  SUBROUTINE h5dread_ikind_4_rank_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_4_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_8_rank_0
!DEC$endif
  SUBROUTINE h5dread_ikind_8_rank_0(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(INOUT), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)               
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_8_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_8_rank_1
!DEC$endif
  SUBROUTINE h5dread_ikind_8_rank_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))            
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_8_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_8_rank_2
!DEC$endif
  SUBROUTINE h5dread_ikind_8_rank_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))          
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_8_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_8_rank_3
!DEC$endif
  SUBROUTINE h5dread_ikind_8_rank_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_8_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_8_rank_4
!DEC$endif
  SUBROUTINE h5dread_ikind_8_rank_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_8_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_8_rank_5
!DEC$endif
  SUBROUTINE h5dread_ikind_8_rank_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_8_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_8_rank_6
!DEC$endif
  SUBROUTINE h5dread_ikind_8_rank_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_8_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_8_rank_7
!DEC$endif
  SUBROUTINE h5dread_ikind_8_rank_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_8_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_16_rank_0
!DEC$endif
  SUBROUTINE h5dread_ikind_16_rank_0(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(INOUT), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)               
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_16_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_16_rank_1
!DEC$endif
  SUBROUTINE h5dread_ikind_16_rank_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))            
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_16_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_16_rank_2
!DEC$endif
  SUBROUTINE h5dread_ikind_16_rank_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))          
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_16_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_16_rank_3
!DEC$endif
  SUBROUTINE h5dread_ikind_16_rank_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_16_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_16_rank_4
!DEC$endif
  SUBROUTINE h5dread_ikind_16_rank_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_16_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_16_rank_5
!DEC$endif
  SUBROUTINE h5dread_ikind_16_rank_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_16_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_16_rank_6
!DEC$endif
  SUBROUTINE h5dread_ikind_16_rank_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_16_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ikind_16_rank_7
!DEC$endif
  SUBROUTINE h5dread_ikind_16_rank_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ikind_16_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ckind_rank_1
!DEC$endif
  SUBROUTINE h5dread_ckind_rank_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1)(1:1))            
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ckind_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ckind_rank_2
!DEC$endif
  SUBROUTINE h5dread_ckind_rank_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1)(1:1))          
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ckind_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ckind_rank_3
!DEC$endif
  SUBROUTINE h5dread_ckind_rank_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1)(1:1))        
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ckind_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ckind_rank_4
!DEC$endif
  SUBROUTINE h5dread_ckind_rank_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1)(1:1))      
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ckind_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ckind_rank_5
!DEC$endif
  SUBROUTINE h5dread_ckind_rank_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1)(1:1))    
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ckind_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ckind_rank_6
!DEC$endif
  SUBROUTINE h5dread_ckind_rank_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1)(1:1))  
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ckind_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dread_ckind_rank_7
!DEC$endif
  SUBROUTINE h5dread_ckind_rank_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*), INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1)(1:1))
    hdferr = h5dread_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dread_ckind_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_4_rank_0
!DEC$endif
  SUBROUTINE h5dwrite_rkind_4_rank_0(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(IN), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)               
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_4_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_4_rank_1
!DEC$endif
  SUBROUTINE h5dwrite_rkind_4_rank_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))            
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_4_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_4_rank_2
!DEC$endif
  SUBROUTINE h5dwrite_rkind_4_rank_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))          
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_4_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_4_rank_3
!DEC$endif
  SUBROUTINE h5dwrite_rkind_4_rank_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_4_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_4_rank_4
!DEC$endif
  SUBROUTINE h5dwrite_rkind_4_rank_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_4_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_4_rank_5
!DEC$endif
  SUBROUTINE h5dwrite_rkind_4_rank_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_4_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_4_rank_6
!DEC$endif
  SUBROUTINE h5dwrite_rkind_4_rank_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_4_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_4_rank_7
!DEC$endif
  SUBROUTINE h5dwrite_rkind_4_rank_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_4_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_8_rank_0
!DEC$endif
  SUBROUTINE h5dwrite_rkind_8_rank_0(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(IN), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)               
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_8_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_8_rank_1
!DEC$endif
  SUBROUTINE h5dwrite_rkind_8_rank_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))            
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_8_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_8_rank_2
!DEC$endif
  SUBROUTINE h5dwrite_rkind_8_rank_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))          
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_8_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_8_rank_3
!DEC$endif
  SUBROUTINE h5dwrite_rkind_8_rank_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_8_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_8_rank_4
!DEC$endif
  SUBROUTINE h5dwrite_rkind_8_rank_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_8_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_8_rank_5
!DEC$endif
  SUBROUTINE h5dwrite_rkind_8_rank_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_8_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_8_rank_6
!DEC$endif
  SUBROUTINE h5dwrite_rkind_8_rank_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_8_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_8_rank_7
!DEC$endif
  SUBROUTINE h5dwrite_rkind_8_rank_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_8_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_10_rank_0
!DEC$endif
  SUBROUTINE h5dwrite_rkind_10_rank_0(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(IN), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)               
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_10_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_10_rank_1
!DEC$endif
  SUBROUTINE h5dwrite_rkind_10_rank_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))            
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_10_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_10_rank_2
!DEC$endif
  SUBROUTINE h5dwrite_rkind_10_rank_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))          
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_10_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_10_rank_3
!DEC$endif
  SUBROUTINE h5dwrite_rkind_10_rank_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_10_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_10_rank_4
!DEC$endif
  SUBROUTINE h5dwrite_rkind_10_rank_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_10_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_10_rank_5
!DEC$endif
  SUBROUTINE h5dwrite_rkind_10_rank_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_10_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_10_rank_6
!DEC$endif
  SUBROUTINE h5dwrite_rkind_10_rank_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_10_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_10_rank_7
!DEC$endif
  SUBROUTINE h5dwrite_rkind_10_rank_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_10_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_16_rank_0
!DEC$endif
  SUBROUTINE h5dwrite_rkind_16_rank_0(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(IN), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)               
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_16_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_16_rank_1
!DEC$endif
  SUBROUTINE h5dwrite_rkind_16_rank_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))            
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_16_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_16_rank_2
!DEC$endif
  SUBROUTINE h5dwrite_rkind_16_rank_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))          
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_16_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_16_rank_3
!DEC$endif
  SUBROUTINE h5dwrite_rkind_16_rank_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_16_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_16_rank_4
!DEC$endif
  SUBROUTINE h5dwrite_rkind_16_rank_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_16_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_16_rank_5
!DEC$endif
  SUBROUTINE h5dwrite_rkind_16_rank_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_16_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_16_rank_6
!DEC$endif
  SUBROUTINE h5dwrite_rkind_16_rank_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_16_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_rkind_16_rank_7
!DEC$endif
  SUBROUTINE h5dwrite_rkind_16_rank_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_rkind_16_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_1_rank_0
!DEC$endif
  SUBROUTINE h5dwrite_ikind_1_rank_0(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(IN), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)               
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_1_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_1_rank_1
!DEC$endif
  SUBROUTINE h5dwrite_ikind_1_rank_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))            
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_1_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_1_rank_2
!DEC$endif
  SUBROUTINE h5dwrite_ikind_1_rank_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))          
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_1_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_1_rank_3
!DEC$endif
  SUBROUTINE h5dwrite_ikind_1_rank_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_1_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_1_rank_4
!DEC$endif
  SUBROUTINE h5dwrite_ikind_1_rank_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_1_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_1_rank_5
!DEC$endif
  SUBROUTINE h5dwrite_ikind_1_rank_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_1_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_1_rank_6
!DEC$endif
  SUBROUTINE h5dwrite_ikind_1_rank_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_1_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_1_rank_7
!DEC$endif
  SUBROUTINE h5dwrite_ikind_1_rank_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_1_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_2_rank_0
!DEC$endif
  SUBROUTINE h5dwrite_ikind_2_rank_0(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(IN), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)               
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_2_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_2_rank_1
!DEC$endif
  SUBROUTINE h5dwrite_ikind_2_rank_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))            
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_2_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_2_rank_2
!DEC$endif
  SUBROUTINE h5dwrite_ikind_2_rank_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))          
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_2_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_2_rank_3
!DEC$endif
  SUBROUTINE h5dwrite_ikind_2_rank_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_2_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_2_rank_4
!DEC$endif
  SUBROUTINE h5dwrite_ikind_2_rank_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_2_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_2_rank_5
!DEC$endif
  SUBROUTINE h5dwrite_ikind_2_rank_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_2_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_2_rank_6
!DEC$endif
  SUBROUTINE h5dwrite_ikind_2_rank_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_2_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_2_rank_7
!DEC$endif
  SUBROUTINE h5dwrite_ikind_2_rank_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_2_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_4_rank_0
!DEC$endif
  SUBROUTINE h5dwrite_ikind_4_rank_0(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(IN), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)               
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_4_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_4_rank_1
!DEC$endif
  SUBROUTINE h5dwrite_ikind_4_rank_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))            
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_4_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_4_rank_2
!DEC$endif
  SUBROUTINE h5dwrite_ikind_4_rank_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))          
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_4_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_4_rank_3
!DEC$endif
  SUBROUTINE h5dwrite_ikind_4_rank_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_4_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_4_rank_4
!DEC$endif
  SUBROUTINE h5dwrite_ikind_4_rank_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_4_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_4_rank_5
!DEC$endif
  SUBROUTINE h5dwrite_ikind_4_rank_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_4_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_4_rank_6
!DEC$endif
  SUBROUTINE h5dwrite_ikind_4_rank_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_4_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_4_rank_7
!DEC$endif
  SUBROUTINE h5dwrite_ikind_4_rank_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_4_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_8_rank_0
!DEC$endif
  SUBROUTINE h5dwrite_ikind_8_rank_0(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(IN), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)               
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_8_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_8_rank_1
!DEC$endif
  SUBROUTINE h5dwrite_ikind_8_rank_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))            
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_8_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_8_rank_2
!DEC$endif
  SUBROUTINE h5dwrite_ikind_8_rank_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))          
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_8_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_8_rank_3
!DEC$endif
  SUBROUTINE h5dwrite_ikind_8_rank_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_8_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_8_rank_4
!DEC$endif
  SUBROUTINE h5dwrite_ikind_8_rank_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_8_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_8_rank_5
!DEC$endif
  SUBROUTINE h5dwrite_ikind_8_rank_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_8_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_8_rank_6
!DEC$endif
  SUBROUTINE h5dwrite_ikind_8_rank_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_8_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_8_rank_7
!DEC$endif
  SUBROUTINE h5dwrite_ikind_8_rank_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_8_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_16_rank_0
!DEC$endif
  SUBROUTINE h5dwrite_ikind_16_rank_0(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(IN), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf)               
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_16_rank_0
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_16_rank_1
!DEC$endif
  SUBROUTINE h5dwrite_ikind_16_rank_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1))            
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_16_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_16_rank_2
!DEC$endif
  SUBROUTINE h5dwrite_ikind_16_rank_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1))          
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_16_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_16_rank_3
!DEC$endif
  SUBROUTINE h5dwrite_ikind_16_rank_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1))        
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_16_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_16_rank_4
!DEC$endif
  SUBROUTINE h5dwrite_ikind_16_rank_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1))      
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_16_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_16_rank_5
!DEC$endif
  SUBROUTINE h5dwrite_ikind_16_rank_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1))    
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_16_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_16_rank_6
!DEC$endif
  SUBROUTINE h5dwrite_ikind_16_rank_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1))  
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_16_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ikind_16_rank_7
!DEC$endif
  SUBROUTINE h5dwrite_ikind_16_rank_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ikind_16_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ckind_rank_1
!DEC$endif
  SUBROUTINE h5dwrite_ckind_rank_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1)(1:1))            
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ckind_rank_1
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ckind_rank_2
!DEC$endif
  SUBROUTINE h5dwrite_ckind_rank_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1)(1:1))          
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ckind_rank_2
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ckind_rank_3
!DEC$endif
  SUBROUTINE h5dwrite_ckind_rank_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1)(1:1))        
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ckind_rank_3
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ckind_rank_4
!DEC$endif
  SUBROUTINE h5dwrite_ckind_rank_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1)(1:1))      
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ckind_rank_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ckind_rank_5
!DEC$endif
  SUBROUTINE h5dwrite_ckind_rank_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1)(1:1))    
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ckind_rank_5
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ckind_rank_6
!DEC$endif
  SUBROUTINE h5dwrite_ckind_rank_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1)(1:1))  
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ckind_rank_6
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5dwrite_ckind_rank_7
!DEC$endif
  SUBROUTINE h5dwrite_ckind_rank_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(IN) :: mem_type_id
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    CHARACTER(LEN=*),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: mem_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: file_space_id
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: xfer_prp
    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default
    TYPE(C_PTR) :: f_ptr
    xfer_prp_default  = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F
    IF(PRESENT(xfer_prp)) xfer_prp_default = xfer_prp
    IF(PRESENT(mem_space_id))  mem_space_id_default = mem_space_id
    IF(PRESENT(file_space_id)) file_space_id_default = file_space_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1)(1:1))
    hdferr = h5dwrite_f_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, f_ptr)
  END SUBROUTINE h5dwrite_ckind_rank_7
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_fill_value_kind_4
!DEC$endif
  SUBROUTINE h5pset_fill_value_kind_4(prp_id, type_id, fillvalue, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HID_T), INTENT(IN) :: type_id
    REAL(KIND=4), INTENT(IN), TARGET :: fillvalue
    INTEGER, INTENT(OUT) :: hdferr 
    TYPE(C_PTR) :: f_ptr 
    f_ptr = C_LOC(fillvalue)
    hdferr = INT(h5pset_fill_value(prp_id, type_id, f_ptr))
  END SUBROUTINE h5pset_fill_value_kind_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_fill_value_kind_8
!DEC$endif
  SUBROUTINE h5pset_fill_value_kind_8(prp_id, type_id, fillvalue, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HID_T), INTENT(IN) :: type_id
    REAL(KIND=8), INTENT(IN), TARGET :: fillvalue
    INTEGER, INTENT(OUT) :: hdferr 
    TYPE(C_PTR) :: f_ptr 
    f_ptr = C_LOC(fillvalue)
    hdferr = INT(h5pset_fill_value(prp_id, type_id, f_ptr))
  END SUBROUTINE h5pset_fill_value_kind_8
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_fill_value_kind_10
!DEC$endif
  SUBROUTINE h5pset_fill_value_kind_10(prp_id, type_id, fillvalue, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HID_T), INTENT(IN) :: type_id
    REAL(KIND=10), INTENT(IN), TARGET :: fillvalue
    INTEGER, INTENT(OUT) :: hdferr 
    TYPE(C_PTR) :: f_ptr 
    f_ptr = C_LOC(fillvalue)
    hdferr = INT(h5pset_fill_value(prp_id, type_id, f_ptr))
  END SUBROUTINE h5pset_fill_value_kind_10
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_fill_value_kind_16
!DEC$endif
  SUBROUTINE h5pset_fill_value_kind_16(prp_id, type_id, fillvalue, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HID_T), INTENT(IN) :: type_id
    REAL(KIND=16), INTENT(IN), TARGET :: fillvalue
    INTEGER, INTENT(OUT) :: hdferr 
    TYPE(C_PTR) :: f_ptr 
    f_ptr = C_LOC(fillvalue)
    hdferr = INT(h5pset_fill_value(prp_id, type_id, f_ptr))
  END SUBROUTINE h5pset_fill_value_kind_16
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_fill_value_kind_4
!DEC$endif
  SUBROUTINE h5pget_fill_value_kind_4(prp_id, type_id, fillvalue, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HID_T), INTENT(IN) :: type_id
    REAL(KIND=4), INTENT(OUT), TARGET :: fillvalue
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(fillvalue)
    hdferr = INT(h5pget_fill_value(prp_id, type_id, f_ptr))
  END SUBROUTINE h5pget_fill_value_kind_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_fill_value_kind_8
!DEC$endif
  SUBROUTINE h5pget_fill_value_kind_8(prp_id, type_id, fillvalue, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HID_T), INTENT(IN) :: type_id
    REAL(KIND=8), INTENT(OUT), TARGET :: fillvalue
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(fillvalue)
    hdferr = INT(h5pget_fill_value(prp_id, type_id, f_ptr))
  END SUBROUTINE h5pget_fill_value_kind_8
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_fill_value_kind_10
!DEC$endif
  SUBROUTINE h5pget_fill_value_kind_10(prp_id, type_id, fillvalue, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HID_T), INTENT(IN) :: type_id
    REAL(KIND=10), INTENT(OUT), TARGET :: fillvalue
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(fillvalue)
    hdferr = INT(h5pget_fill_value(prp_id, type_id, f_ptr))
  END SUBROUTINE h5pget_fill_value_kind_10
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_fill_value_kind_16
!DEC$endif
  SUBROUTINE h5pget_fill_value_kind_16(prp_id, type_id, fillvalue, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    INTEGER(HID_T), INTENT(IN) :: type_id
    REAL(KIND=16), INTENT(OUT), TARGET :: fillvalue
    INTEGER, INTENT(OUT) :: hdferr
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(fillvalue)
    hdferr = INT(h5pget_fill_value(prp_id, type_id, f_ptr))
  END SUBROUTINE h5pget_fill_value_kind_16
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_kind_4
!DEC$endif
  SUBROUTINE h5pset_kind_4(prp_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
    REAL(KIND=4), INTENT(IN), TARGET :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(value)
    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)
  END SUBROUTINE h5pset_kind_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_kind_8
!DEC$endif
  SUBROUTINE h5pset_kind_8(prp_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
    REAL(KIND=8), INTENT(IN), TARGET :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(value)
    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)
  END SUBROUTINE h5pset_kind_8
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_kind_10
!DEC$endif
  SUBROUTINE h5pset_kind_10(prp_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
    REAL(KIND=10), INTENT(IN), TARGET :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(value)
    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)
  END SUBROUTINE h5pset_kind_10
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pset_kind_16
!DEC$endif
  SUBROUTINE h5pset_kind_16(prp_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: name
    REAL(KIND=16), INTENT(IN), TARGET :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(value)
    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)
  END SUBROUTINE h5pset_kind_16
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_kind_4
!DEC$endif
  SUBROUTINE h5pget_kind_4(prp_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    REAL(KIND=4),   INTENT(OUT), TARGET :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(value)
    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)
  END SUBROUTINE h5pget_kind_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_kind_8
!DEC$endif
  SUBROUTINE h5pget_kind_8(prp_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    REAL(KIND=8),   INTENT(OUT), TARGET :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(value)
    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)
  END SUBROUTINE h5pget_kind_8
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_kind_10
!DEC$endif
  SUBROUTINE h5pget_kind_10(prp_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    REAL(KIND=10),   INTENT(OUT), TARGET :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(value)
    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)
  END SUBROUTINE h5pget_kind_10
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pget_kind_16
!DEC$endif
  SUBROUTINE h5pget_kind_16(prp_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id
    CHARACTER(LEN=*), INTENT(IN) :: name
    REAL(KIND=16),   INTENT(OUT), TARGET :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len
    TYPE(C_PTR) :: f_ptr
    f_ptr = C_LOC(value)
    name_len = LEN(name)
    hdferr = h5pget_c(prp_id, name, name_len, f_ptr)
  END SUBROUTINE h5pget_kind_16
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pregister_kind_4
!DEC$endif
SUBROUTINE h5pregister_kind_4(class, name, size, value, hdferr)
  IMPLICIT NONE
  INTEGER(HID_T), INTENT(IN) :: class
  CHARACTER(LEN=*), INTENT(IN) :: name
  INTEGER(SIZE_T), INTENT(IN) :: size
  REAL(KIND=4), INTENT(IN), TARGET :: value
  INTEGER, INTENT(OUT) :: hdferr
  INTEGER :: name_len
  TYPE(C_PTR) :: f_ptr
  f_ptr = C_LOC(value)
  name_len = LEN(name)
  hdferr = h5pregister_c(class, name, name_len, size, f_ptr)
END SUBROUTINE h5pregister_kind_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pregister_kind_8
!DEC$endif
SUBROUTINE h5pregister_kind_8(class, name, size, value, hdferr)
  IMPLICIT NONE
  INTEGER(HID_T), INTENT(IN) :: class
  CHARACTER(LEN=*), INTENT(IN) :: name
  INTEGER(SIZE_T), INTENT(IN) :: size
  REAL(KIND=8), INTENT(IN), TARGET :: value
  INTEGER, INTENT(OUT) :: hdferr
  INTEGER :: name_len
  TYPE(C_PTR) :: f_ptr
  f_ptr = C_LOC(value)
  name_len = LEN(name)
  hdferr = h5pregister_c(class, name, name_len, size, f_ptr)
END SUBROUTINE h5pregister_kind_8
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pregister_kind_10
!DEC$endif
SUBROUTINE h5pregister_kind_10(class, name, size, value, hdferr)
  IMPLICIT NONE
  INTEGER(HID_T), INTENT(IN) :: class
  CHARACTER(LEN=*), INTENT(IN) :: name
  INTEGER(SIZE_T), INTENT(IN) :: size
  REAL(KIND=10), INTENT(IN), TARGET :: value
  INTEGER, INTENT(OUT) :: hdferr
  INTEGER :: name_len
  TYPE(C_PTR) :: f_ptr
  f_ptr = C_LOC(value)
  name_len = LEN(name)
  hdferr = h5pregister_c(class, name, name_len, size, f_ptr)
END SUBROUTINE h5pregister_kind_10
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pregister_kind_16
!DEC$endif
SUBROUTINE h5pregister_kind_16(class, name, size, value, hdferr)
  IMPLICIT NONE
  INTEGER(HID_T), INTENT(IN) :: class
  CHARACTER(LEN=*), INTENT(IN) :: name
  INTEGER(SIZE_T), INTENT(IN) :: size
  REAL(KIND=16), INTENT(IN), TARGET :: value
  INTEGER, INTENT(OUT) :: hdferr
  INTEGER :: name_len
  TYPE(C_PTR) :: f_ptr
  f_ptr = C_LOC(value)
  name_len = LEN(name)
  hdferr = h5pregister_c(class, name, name_len, size, f_ptr)
END SUBROUTINE h5pregister_kind_16
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pinsert_kind_4
!DEC$endif
  SUBROUTINE h5pinsert_kind_4(plist, name, size, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(SIZE_T), INTENT(IN) :: size
    REAL(KIND=4),   INTENT(IN), TARGET :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len
    TYPE(c_ptr) :: f_ptr
    f_ptr = c_loc(value)
    name_len = LEN(name)
    hdferr = h5pinsert_c(plist, name , name_len, size, f_ptr)
  END SUBROUTINE h5pinsert_kind_4
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pinsert_kind_8
!DEC$endif
  SUBROUTINE h5pinsert_kind_8(plist, name, size, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(SIZE_T), INTENT(IN) :: size
    REAL(KIND=8),   INTENT(IN), TARGET :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len
    TYPE(c_ptr) :: f_ptr
    f_ptr = c_loc(value)
    name_len = LEN(name)
    hdferr = h5pinsert_c(plist, name , name_len, size, f_ptr)
  END SUBROUTINE h5pinsert_kind_8
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pinsert_kind_10
!DEC$endif
  SUBROUTINE h5pinsert_kind_10(plist, name, size, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(SIZE_T), INTENT(IN) :: size
    REAL(KIND=10),   INTENT(IN), TARGET :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len
    TYPE(c_ptr) :: f_ptr
    f_ptr = c_loc(value)
    name_len = LEN(name)
    hdferr = h5pinsert_c(plist, name , name_len, size, f_ptr)
  END SUBROUTINE h5pinsert_kind_10
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5pinsert_kind_16
!DEC$endif
  SUBROUTINE h5pinsert_kind_16(plist, name, size, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(SIZE_T), INTENT(IN) :: size
    REAL(KIND=16),   INTENT(IN), TARGET :: value
    INTEGER, INTENT(OUT) :: hdferr
    INTEGER :: name_len
    TYPE(c_ptr) :: f_ptr
    f_ptr = c_loc(value)
    name_len = LEN(name)
    hdferr = h5pinsert_c(plist, name , name_len, size, f_ptr)
  END SUBROUTINE h5pinsert_kind_16
END MODULE H5_gen
