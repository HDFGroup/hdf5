!****h* ROBODoc/H5LTff_gen.F90
!
! NAME
!  H5LTff_gen
! 
! PURPOSE
!  This module is generated at build by H5HL_buildiface.F90 to handle all the
!  detected REAL/INTEGER KINDs for APIs being passed those KINDs. Currently these 
!  are H5LT and H5TB APIs
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
!   H5HL_buildiface.F90
!
!*****

MODULE H5LT
  USE, INTRINSIC :: ISO_C_BINDING
  USE h5fortran_types
  USE H5LT_CONST
  IMPLICIT NONE
  INTERFACE h5ltmake_dataset_f
     MODULE PROCEDURE h5ltmake_dataset_real_kind_4_rank_0
     MODULE PROCEDURE h5ltmake_dataset_real_kind_4_rank_1
     MODULE PROCEDURE h5ltmake_dataset_real_kind_4_rank_2
     MODULE PROCEDURE h5ltmake_dataset_real_kind_4_rank_3
     MODULE PROCEDURE h5ltmake_dataset_real_kind_4_rank_4
     MODULE PROCEDURE h5ltmake_dataset_real_kind_4_rank_5
     MODULE PROCEDURE h5ltmake_dataset_real_kind_4_rank_6
     MODULE PROCEDURE h5ltmake_dataset_real_kind_4_rank_7
     MODULE PROCEDURE h5ltmake_dataset_real_kind_8_rank_0
     MODULE PROCEDURE h5ltmake_dataset_real_kind_8_rank_1
     MODULE PROCEDURE h5ltmake_dataset_real_kind_8_rank_2
     MODULE PROCEDURE h5ltmake_dataset_real_kind_8_rank_3
     MODULE PROCEDURE h5ltmake_dataset_real_kind_8_rank_4
     MODULE PROCEDURE h5ltmake_dataset_real_kind_8_rank_5
     MODULE PROCEDURE h5ltmake_dataset_real_kind_8_rank_6
     MODULE PROCEDURE h5ltmake_dataset_real_kind_8_rank_7
     MODULE PROCEDURE h5ltmake_dataset_real_kind_10_rank_0
     MODULE PROCEDURE h5ltmake_dataset_real_kind_10_rank_1
     MODULE PROCEDURE h5ltmake_dataset_real_kind_10_rank_2
     MODULE PROCEDURE h5ltmake_dataset_real_kind_10_rank_3
     MODULE PROCEDURE h5ltmake_dataset_real_kind_10_rank_4
     MODULE PROCEDURE h5ltmake_dataset_real_kind_10_rank_5
     MODULE PROCEDURE h5ltmake_dataset_real_kind_10_rank_6
     MODULE PROCEDURE h5ltmake_dataset_real_kind_10_rank_7
     MODULE PROCEDURE h5ltmake_dataset_real_kind_16_rank_0
     MODULE PROCEDURE h5ltmake_dataset_real_kind_16_rank_1
     MODULE PROCEDURE h5ltmake_dataset_real_kind_16_rank_2
     MODULE PROCEDURE h5ltmake_dataset_real_kind_16_rank_3
     MODULE PROCEDURE h5ltmake_dataset_real_kind_16_rank_4
     MODULE PROCEDURE h5ltmake_dataset_real_kind_16_rank_5
     MODULE PROCEDURE h5ltmake_dataset_real_kind_16_rank_6
     MODULE PROCEDURE h5ltmake_dataset_real_kind_16_rank_7
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_1_rank_0
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_1_rank_1
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_1_rank_2
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_1_rank_3
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_1_rank_4
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_1_rank_5
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_1_rank_6
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_1_rank_7
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_2_rank_0
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_2_rank_1
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_2_rank_2
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_2_rank_3
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_2_rank_4
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_2_rank_5
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_2_rank_6
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_2_rank_7
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_4_rank_0
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_4_rank_1
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_4_rank_2
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_4_rank_3
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_4_rank_4
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_4_rank_5
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_4_rank_6
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_4_rank_7
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_8_rank_0
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_8_rank_1
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_8_rank_2
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_8_rank_3
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_8_rank_4
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_8_rank_5
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_8_rank_6
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_8_rank_7
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_16_rank_0
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_16_rank_1
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_16_rank_2
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_16_rank_3
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_16_rank_4
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_16_rank_5
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_16_rank_6
     MODULE PROCEDURE h5ltmake_dataset_integer_kind_16_rank_7
  END INTERFACE
  INTERFACE h5ltread_dataset_f
     MODULE PROCEDURE h5ltread_dataset_real_kind_4_rank_0
     MODULE PROCEDURE h5ltread_dataset_real_kind_4_rank_1
     MODULE PROCEDURE h5ltread_dataset_real_kind_4_rank_2
     MODULE PROCEDURE h5ltread_dataset_real_kind_4_rank_3
     MODULE PROCEDURE h5ltread_dataset_real_kind_4_rank_4
     MODULE PROCEDURE h5ltread_dataset_real_kind_4_rank_5
     MODULE PROCEDURE h5ltread_dataset_real_kind_4_rank_6
     MODULE PROCEDURE h5ltread_dataset_real_kind_4_rank_7
     MODULE PROCEDURE h5ltread_dataset_real_kind_8_rank_0
     MODULE PROCEDURE h5ltread_dataset_real_kind_8_rank_1
     MODULE PROCEDURE h5ltread_dataset_real_kind_8_rank_2
     MODULE PROCEDURE h5ltread_dataset_real_kind_8_rank_3
     MODULE PROCEDURE h5ltread_dataset_real_kind_8_rank_4
     MODULE PROCEDURE h5ltread_dataset_real_kind_8_rank_5
     MODULE PROCEDURE h5ltread_dataset_real_kind_8_rank_6
     MODULE PROCEDURE h5ltread_dataset_real_kind_8_rank_7
     MODULE PROCEDURE h5ltread_dataset_real_kind_10_rank_0
     MODULE PROCEDURE h5ltread_dataset_real_kind_10_rank_1
     MODULE PROCEDURE h5ltread_dataset_real_kind_10_rank_2
     MODULE PROCEDURE h5ltread_dataset_real_kind_10_rank_3
     MODULE PROCEDURE h5ltread_dataset_real_kind_10_rank_4
     MODULE PROCEDURE h5ltread_dataset_real_kind_10_rank_5
     MODULE PROCEDURE h5ltread_dataset_real_kind_10_rank_6
     MODULE PROCEDURE h5ltread_dataset_real_kind_10_rank_7
     MODULE PROCEDURE h5ltread_dataset_real_kind_16_rank_0
     MODULE PROCEDURE h5ltread_dataset_real_kind_16_rank_1
     MODULE PROCEDURE h5ltread_dataset_real_kind_16_rank_2
     MODULE PROCEDURE h5ltread_dataset_real_kind_16_rank_3
     MODULE PROCEDURE h5ltread_dataset_real_kind_16_rank_4
     MODULE PROCEDURE h5ltread_dataset_real_kind_16_rank_5
     MODULE PROCEDURE h5ltread_dataset_real_kind_16_rank_6
     MODULE PROCEDURE h5ltread_dataset_real_kind_16_rank_7
     MODULE PROCEDURE h5ltread_dataset_integer_kind_1_rank_0
     MODULE PROCEDURE h5ltread_dataset_integer_kind_1_rank_1
     MODULE PROCEDURE h5ltread_dataset_integer_kind_1_rank_2
     MODULE PROCEDURE h5ltread_dataset_integer_kind_1_rank_3
     MODULE PROCEDURE h5ltread_dataset_integer_kind_1_rank_4
     MODULE PROCEDURE h5ltread_dataset_integer_kind_1_rank_5
     MODULE PROCEDURE h5ltread_dataset_integer_kind_1_rank_6
     MODULE PROCEDURE h5ltread_dataset_integer_kind_1_rank_7
     MODULE PROCEDURE h5ltread_dataset_integer_kind_2_rank_0
     MODULE PROCEDURE h5ltread_dataset_integer_kind_2_rank_1
     MODULE PROCEDURE h5ltread_dataset_integer_kind_2_rank_2
     MODULE PROCEDURE h5ltread_dataset_integer_kind_2_rank_3
     MODULE PROCEDURE h5ltread_dataset_integer_kind_2_rank_4
     MODULE PROCEDURE h5ltread_dataset_integer_kind_2_rank_5
     MODULE PROCEDURE h5ltread_dataset_integer_kind_2_rank_6
     MODULE PROCEDURE h5ltread_dataset_integer_kind_2_rank_7
     MODULE PROCEDURE h5ltread_dataset_integer_kind_4_rank_0
     MODULE PROCEDURE h5ltread_dataset_integer_kind_4_rank_1
     MODULE PROCEDURE h5ltread_dataset_integer_kind_4_rank_2
     MODULE PROCEDURE h5ltread_dataset_integer_kind_4_rank_3
     MODULE PROCEDURE h5ltread_dataset_integer_kind_4_rank_4
     MODULE PROCEDURE h5ltread_dataset_integer_kind_4_rank_5
     MODULE PROCEDURE h5ltread_dataset_integer_kind_4_rank_6
     MODULE PROCEDURE h5ltread_dataset_integer_kind_4_rank_7
     MODULE PROCEDURE h5ltread_dataset_integer_kind_8_rank_0
     MODULE PROCEDURE h5ltread_dataset_integer_kind_8_rank_1
     MODULE PROCEDURE h5ltread_dataset_integer_kind_8_rank_2
     MODULE PROCEDURE h5ltread_dataset_integer_kind_8_rank_3
     MODULE PROCEDURE h5ltread_dataset_integer_kind_8_rank_4
     MODULE PROCEDURE h5ltread_dataset_integer_kind_8_rank_5
     MODULE PROCEDURE h5ltread_dataset_integer_kind_8_rank_6
     MODULE PROCEDURE h5ltread_dataset_integer_kind_8_rank_7
     MODULE PROCEDURE h5ltread_dataset_integer_kind_16_rank_0
     MODULE PROCEDURE h5ltread_dataset_integer_kind_16_rank_1
     MODULE PROCEDURE h5ltread_dataset_integer_kind_16_rank_2
     MODULE PROCEDURE h5ltread_dataset_integer_kind_16_rank_3
     MODULE PROCEDURE h5ltread_dataset_integer_kind_16_rank_4
     MODULE PROCEDURE h5ltread_dataset_integer_kind_16_rank_5
     MODULE PROCEDURE h5ltread_dataset_integer_kind_16_rank_6
     MODULE PROCEDURE h5ltread_dataset_integer_kind_16_rank_7
  END INTERFACE
  INTERFACE h5ltread_dataset_int_f
     MODULE PROCEDURE h5ltread_dataset_int_kind_1_rank_0
     MODULE PROCEDURE h5ltread_dataset_int_kind_1_rank_1
     MODULE PROCEDURE h5ltread_dataset_int_kind_1_rank_2
     MODULE PROCEDURE h5ltread_dataset_int_kind_1_rank_3
     MODULE PROCEDURE h5ltread_dataset_int_kind_1_rank_4
     MODULE PROCEDURE h5ltread_dataset_int_kind_1_rank_5
     MODULE PROCEDURE h5ltread_dataset_int_kind_1_rank_6
     MODULE PROCEDURE h5ltread_dataset_int_kind_1_rank_7
     MODULE PROCEDURE h5ltread_dataset_int_kind_2_rank_0
     MODULE PROCEDURE h5ltread_dataset_int_kind_2_rank_1
     MODULE PROCEDURE h5ltread_dataset_int_kind_2_rank_2
     MODULE PROCEDURE h5ltread_dataset_int_kind_2_rank_3
     MODULE PROCEDURE h5ltread_dataset_int_kind_2_rank_4
     MODULE PROCEDURE h5ltread_dataset_int_kind_2_rank_5
     MODULE PROCEDURE h5ltread_dataset_int_kind_2_rank_6
     MODULE PROCEDURE h5ltread_dataset_int_kind_2_rank_7
     MODULE PROCEDURE h5ltread_dataset_int_kind_4_rank_0
     MODULE PROCEDURE h5ltread_dataset_int_kind_4_rank_1
     MODULE PROCEDURE h5ltread_dataset_int_kind_4_rank_2
     MODULE PROCEDURE h5ltread_dataset_int_kind_4_rank_3
     MODULE PROCEDURE h5ltread_dataset_int_kind_4_rank_4
     MODULE PROCEDURE h5ltread_dataset_int_kind_4_rank_5
     MODULE PROCEDURE h5ltread_dataset_int_kind_4_rank_6
     MODULE PROCEDURE h5ltread_dataset_int_kind_4_rank_7
     MODULE PROCEDURE h5ltread_dataset_int_kind_8_rank_0
     MODULE PROCEDURE h5ltread_dataset_int_kind_8_rank_1
     MODULE PROCEDURE h5ltread_dataset_int_kind_8_rank_2
     MODULE PROCEDURE h5ltread_dataset_int_kind_8_rank_3
     MODULE PROCEDURE h5ltread_dataset_int_kind_8_rank_4
     MODULE PROCEDURE h5ltread_dataset_int_kind_8_rank_5
     MODULE PROCEDURE h5ltread_dataset_int_kind_8_rank_6
     MODULE PROCEDURE h5ltread_dataset_int_kind_8_rank_7
     MODULE PROCEDURE h5ltread_dataset_int_kind_16_rank_0
     MODULE PROCEDURE h5ltread_dataset_int_kind_16_rank_1
     MODULE PROCEDURE h5ltread_dataset_int_kind_16_rank_2
     MODULE PROCEDURE h5ltread_dataset_int_kind_16_rank_3
     MODULE PROCEDURE h5ltread_dataset_int_kind_16_rank_4
     MODULE PROCEDURE h5ltread_dataset_int_kind_16_rank_5
     MODULE PROCEDURE h5ltread_dataset_int_kind_16_rank_6
     MODULE PROCEDURE h5ltread_dataset_int_kind_16_rank_7
  END INTERFACE
  INTERFACE h5ltmake_dataset_int_f
     MODULE PROCEDURE h5ltmake_dataset_int_kind_1_rank_0
     MODULE PROCEDURE h5ltmake_dataset_int_kind_1_rank_1
     MODULE PROCEDURE h5ltmake_dataset_int_kind_1_rank_2
     MODULE PROCEDURE h5ltmake_dataset_int_kind_1_rank_3
     MODULE PROCEDURE h5ltmake_dataset_int_kind_1_rank_4
     MODULE PROCEDURE h5ltmake_dataset_int_kind_1_rank_5
     MODULE PROCEDURE h5ltmake_dataset_int_kind_1_rank_6
     MODULE PROCEDURE h5ltmake_dataset_int_kind_1_rank_7
     MODULE PROCEDURE h5ltmake_dataset_int_kind_2_rank_0
     MODULE PROCEDURE h5ltmake_dataset_int_kind_2_rank_1
     MODULE PROCEDURE h5ltmake_dataset_int_kind_2_rank_2
     MODULE PROCEDURE h5ltmake_dataset_int_kind_2_rank_3
     MODULE PROCEDURE h5ltmake_dataset_int_kind_2_rank_4
     MODULE PROCEDURE h5ltmake_dataset_int_kind_2_rank_5
     MODULE PROCEDURE h5ltmake_dataset_int_kind_2_rank_6
     MODULE PROCEDURE h5ltmake_dataset_int_kind_2_rank_7
     MODULE PROCEDURE h5ltmake_dataset_int_kind_4_rank_0
     MODULE PROCEDURE h5ltmake_dataset_int_kind_4_rank_1
     MODULE PROCEDURE h5ltmake_dataset_int_kind_4_rank_2
     MODULE PROCEDURE h5ltmake_dataset_int_kind_4_rank_3
     MODULE PROCEDURE h5ltmake_dataset_int_kind_4_rank_4
     MODULE PROCEDURE h5ltmake_dataset_int_kind_4_rank_5
     MODULE PROCEDURE h5ltmake_dataset_int_kind_4_rank_6
     MODULE PROCEDURE h5ltmake_dataset_int_kind_4_rank_7
     MODULE PROCEDURE h5ltmake_dataset_int_kind_8_rank_0
     MODULE PROCEDURE h5ltmake_dataset_int_kind_8_rank_1
     MODULE PROCEDURE h5ltmake_dataset_int_kind_8_rank_2
     MODULE PROCEDURE h5ltmake_dataset_int_kind_8_rank_3
     MODULE PROCEDURE h5ltmake_dataset_int_kind_8_rank_4
     MODULE PROCEDURE h5ltmake_dataset_int_kind_8_rank_5
     MODULE PROCEDURE h5ltmake_dataset_int_kind_8_rank_6
     MODULE PROCEDURE h5ltmake_dataset_int_kind_8_rank_7
     MODULE PROCEDURE h5ltmake_dataset_int_kind_16_rank_0
     MODULE PROCEDURE h5ltmake_dataset_int_kind_16_rank_1
     MODULE PROCEDURE h5ltmake_dataset_int_kind_16_rank_2
     MODULE PROCEDURE h5ltmake_dataset_int_kind_16_rank_3
     MODULE PROCEDURE h5ltmake_dataset_int_kind_16_rank_4
     MODULE PROCEDURE h5ltmake_dataset_int_kind_16_rank_5
     MODULE PROCEDURE h5ltmake_dataset_int_kind_16_rank_6
     MODULE PROCEDURE h5ltmake_dataset_int_kind_16_rank_7
  END INTERFACE
  INTERFACE h5ltmake_dataset_float_f
     MODULE PROCEDURE h5ltmake_dataset_float_kind_4_rank_0
     MODULE PROCEDURE h5ltmake_dataset_float_kind_4_rank_1
     MODULE PROCEDURE h5ltmake_dataset_float_kind_4_rank_2
     MODULE PROCEDURE h5ltmake_dataset_float_kind_4_rank_3
     MODULE PROCEDURE h5ltmake_dataset_float_kind_4_rank_4
     MODULE PROCEDURE h5ltmake_dataset_float_kind_4_rank_5
     MODULE PROCEDURE h5ltmake_dataset_float_kind_4_rank_6
     MODULE PROCEDURE h5ltmake_dataset_float_kind_4_rank_7
     MODULE PROCEDURE h5ltmake_dataset_float_kind_8_rank_0
     MODULE PROCEDURE h5ltmake_dataset_float_kind_8_rank_1
     MODULE PROCEDURE h5ltmake_dataset_float_kind_8_rank_2
     MODULE PROCEDURE h5ltmake_dataset_float_kind_8_rank_3
     MODULE PROCEDURE h5ltmake_dataset_float_kind_8_rank_4
     MODULE PROCEDURE h5ltmake_dataset_float_kind_8_rank_5
     MODULE PROCEDURE h5ltmake_dataset_float_kind_8_rank_6
     MODULE PROCEDURE h5ltmake_dataset_float_kind_8_rank_7
     MODULE PROCEDURE h5ltmake_dataset_float_kind_10_rank_0
     MODULE PROCEDURE h5ltmake_dataset_float_kind_10_rank_1
     MODULE PROCEDURE h5ltmake_dataset_float_kind_10_rank_2
     MODULE PROCEDURE h5ltmake_dataset_float_kind_10_rank_3
     MODULE PROCEDURE h5ltmake_dataset_float_kind_10_rank_4
     MODULE PROCEDURE h5ltmake_dataset_float_kind_10_rank_5
     MODULE PROCEDURE h5ltmake_dataset_float_kind_10_rank_6
     MODULE PROCEDURE h5ltmake_dataset_float_kind_10_rank_7
     MODULE PROCEDURE h5ltmake_dataset_float_kind_16_rank_0
     MODULE PROCEDURE h5ltmake_dataset_float_kind_16_rank_1
     MODULE PROCEDURE h5ltmake_dataset_float_kind_16_rank_2
     MODULE PROCEDURE h5ltmake_dataset_float_kind_16_rank_3
     MODULE PROCEDURE h5ltmake_dataset_float_kind_16_rank_4
     MODULE PROCEDURE h5ltmake_dataset_float_kind_16_rank_5
     MODULE PROCEDURE h5ltmake_dataset_float_kind_16_rank_6
     MODULE PROCEDURE h5ltmake_dataset_float_kind_16_rank_7
  END INTERFACE
  INTERFACE h5ltmake_dataset_double_f
     MODULE PROCEDURE h5ltmake_dataset_double_kind_4_rank_0
     MODULE PROCEDURE h5ltmake_dataset_double_kind_4_rank_1
     MODULE PROCEDURE h5ltmake_dataset_double_kind_4_rank_2
     MODULE PROCEDURE h5ltmake_dataset_double_kind_4_rank_3
     MODULE PROCEDURE h5ltmake_dataset_double_kind_4_rank_4
     MODULE PROCEDURE h5ltmake_dataset_double_kind_4_rank_5
     MODULE PROCEDURE h5ltmake_dataset_double_kind_4_rank_6
     MODULE PROCEDURE h5ltmake_dataset_double_kind_4_rank_7
     MODULE PROCEDURE h5ltmake_dataset_double_kind_8_rank_0
     MODULE PROCEDURE h5ltmake_dataset_double_kind_8_rank_1
     MODULE PROCEDURE h5ltmake_dataset_double_kind_8_rank_2
     MODULE PROCEDURE h5ltmake_dataset_double_kind_8_rank_3
     MODULE PROCEDURE h5ltmake_dataset_double_kind_8_rank_4
     MODULE PROCEDURE h5ltmake_dataset_double_kind_8_rank_5
     MODULE PROCEDURE h5ltmake_dataset_double_kind_8_rank_6
     MODULE PROCEDURE h5ltmake_dataset_double_kind_8_rank_7
     MODULE PROCEDURE h5ltmake_dataset_double_kind_10_rank_0
     MODULE PROCEDURE h5ltmake_dataset_double_kind_10_rank_1
     MODULE PROCEDURE h5ltmake_dataset_double_kind_10_rank_2
     MODULE PROCEDURE h5ltmake_dataset_double_kind_10_rank_3
     MODULE PROCEDURE h5ltmake_dataset_double_kind_10_rank_4
     MODULE PROCEDURE h5ltmake_dataset_double_kind_10_rank_5
     MODULE PROCEDURE h5ltmake_dataset_double_kind_10_rank_6
     MODULE PROCEDURE h5ltmake_dataset_double_kind_10_rank_7
     MODULE PROCEDURE h5ltmake_dataset_double_kind_16_rank_0
     MODULE PROCEDURE h5ltmake_dataset_double_kind_16_rank_1
     MODULE PROCEDURE h5ltmake_dataset_double_kind_16_rank_2
     MODULE PROCEDURE h5ltmake_dataset_double_kind_16_rank_3
     MODULE PROCEDURE h5ltmake_dataset_double_kind_16_rank_4
     MODULE PROCEDURE h5ltmake_dataset_double_kind_16_rank_5
     MODULE PROCEDURE h5ltmake_dataset_double_kind_16_rank_6
     MODULE PROCEDURE h5ltmake_dataset_double_kind_16_rank_7
  END INTERFACE
  INTERFACE h5ltread_dataset_float_f
     MODULE PROCEDURE h5ltread_dataset_float_kind_4_rank_0
     MODULE PROCEDURE h5ltread_dataset_float_kind_4_rank_1
     MODULE PROCEDURE h5ltread_dataset_float_kind_4_rank_2
     MODULE PROCEDURE h5ltread_dataset_float_kind_4_rank_3
     MODULE PROCEDURE h5ltread_dataset_float_kind_4_rank_4
     MODULE PROCEDURE h5ltread_dataset_float_kind_4_rank_5
     MODULE PROCEDURE h5ltread_dataset_float_kind_4_rank_6
     MODULE PROCEDURE h5ltread_dataset_float_kind_4_rank_7
     MODULE PROCEDURE h5ltread_dataset_float_kind_8_rank_0
     MODULE PROCEDURE h5ltread_dataset_float_kind_8_rank_1
     MODULE PROCEDURE h5ltread_dataset_float_kind_8_rank_2
     MODULE PROCEDURE h5ltread_dataset_float_kind_8_rank_3
     MODULE PROCEDURE h5ltread_dataset_float_kind_8_rank_4
     MODULE PROCEDURE h5ltread_dataset_float_kind_8_rank_5
     MODULE PROCEDURE h5ltread_dataset_float_kind_8_rank_6
     MODULE PROCEDURE h5ltread_dataset_float_kind_8_rank_7
     MODULE PROCEDURE h5ltread_dataset_float_kind_10_rank_0
     MODULE PROCEDURE h5ltread_dataset_float_kind_10_rank_1
     MODULE PROCEDURE h5ltread_dataset_float_kind_10_rank_2
     MODULE PROCEDURE h5ltread_dataset_float_kind_10_rank_3
     MODULE PROCEDURE h5ltread_dataset_float_kind_10_rank_4
     MODULE PROCEDURE h5ltread_dataset_float_kind_10_rank_5
     MODULE PROCEDURE h5ltread_dataset_float_kind_10_rank_6
     MODULE PROCEDURE h5ltread_dataset_float_kind_10_rank_7
     MODULE PROCEDURE h5ltread_dataset_float_kind_16_rank_0
     MODULE PROCEDURE h5ltread_dataset_float_kind_16_rank_1
     MODULE PROCEDURE h5ltread_dataset_float_kind_16_rank_2
     MODULE PROCEDURE h5ltread_dataset_float_kind_16_rank_3
     MODULE PROCEDURE h5ltread_dataset_float_kind_16_rank_4
     MODULE PROCEDURE h5ltread_dataset_float_kind_16_rank_5
     MODULE PROCEDURE h5ltread_dataset_float_kind_16_rank_6
     MODULE PROCEDURE h5ltread_dataset_float_kind_16_rank_7
  END INTERFACE
  INTERFACE h5ltread_dataset_double_f
     MODULE PROCEDURE h5ltread_dataset_double_kind_4_rank_0
     MODULE PROCEDURE h5ltread_dataset_double_kind_4_rank_1
     MODULE PROCEDURE h5ltread_dataset_double_kind_4_rank_2
     MODULE PROCEDURE h5ltread_dataset_double_kind_4_rank_3
     MODULE PROCEDURE h5ltread_dataset_double_kind_4_rank_4
     MODULE PROCEDURE h5ltread_dataset_double_kind_4_rank_5
     MODULE PROCEDURE h5ltread_dataset_double_kind_4_rank_6
     MODULE PROCEDURE h5ltread_dataset_double_kind_4_rank_7
     MODULE PROCEDURE h5ltread_dataset_double_kind_8_rank_0
     MODULE PROCEDURE h5ltread_dataset_double_kind_8_rank_1
     MODULE PROCEDURE h5ltread_dataset_double_kind_8_rank_2
     MODULE PROCEDURE h5ltread_dataset_double_kind_8_rank_3
     MODULE PROCEDURE h5ltread_dataset_double_kind_8_rank_4
     MODULE PROCEDURE h5ltread_dataset_double_kind_8_rank_5
     MODULE PROCEDURE h5ltread_dataset_double_kind_8_rank_6
     MODULE PROCEDURE h5ltread_dataset_double_kind_8_rank_7
     MODULE PROCEDURE h5ltread_dataset_double_kind_10_rank_0
     MODULE PROCEDURE h5ltread_dataset_double_kind_10_rank_1
     MODULE PROCEDURE h5ltread_dataset_double_kind_10_rank_2
     MODULE PROCEDURE h5ltread_dataset_double_kind_10_rank_3
     MODULE PROCEDURE h5ltread_dataset_double_kind_10_rank_4
     MODULE PROCEDURE h5ltread_dataset_double_kind_10_rank_5
     MODULE PROCEDURE h5ltread_dataset_double_kind_10_rank_6
     MODULE PROCEDURE h5ltread_dataset_double_kind_10_rank_7
     MODULE PROCEDURE h5ltread_dataset_double_kind_16_rank_0
     MODULE PROCEDURE h5ltread_dataset_double_kind_16_rank_1
     MODULE PROCEDURE h5ltread_dataset_double_kind_16_rank_2
     MODULE PROCEDURE h5ltread_dataset_double_kind_16_rank_3
     MODULE PROCEDURE h5ltread_dataset_double_kind_16_rank_4
     MODULE PROCEDURE h5ltread_dataset_double_kind_16_rank_5
     MODULE PROCEDURE h5ltread_dataset_double_kind_16_rank_6
     MODULE PROCEDURE h5ltread_dataset_double_kind_16_rank_7
  END INTERFACE
CONTAINS
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_4_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_4_rank_0(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=4),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_4_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_4_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_4_rank_1(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_4_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_4_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_4_rank_2(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_4_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_4_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_4_rank_3(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_4_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_4_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_4_rank_4(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_4_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_4_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_4_rank_5(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_4_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_4_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_4_rank_6(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_4_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_4_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_4_rank_7(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_4_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_8_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_8_rank_0(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=8),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_8_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_8_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_8_rank_1(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_8_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_8_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_8_rank_2(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_8_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_8_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_8_rank_3(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_8_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_8_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_8_rank_4(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_8_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_8_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_8_rank_5(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_8_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_8_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_8_rank_6(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_8_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_8_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_8_rank_7(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_8_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_10_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_10_rank_0(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=10),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_10_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_10_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_10_rank_1(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_10_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_10_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_10_rank_2(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_10_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_10_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_10_rank_3(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_10_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_10_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_10_rank_4(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_10_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_10_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_10_rank_5(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_10_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_10_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_10_rank_6(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_10_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_10_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_10_rank_7(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_10_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_16_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_16_rank_0(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=16),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_16_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_16_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_16_rank_1(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_16_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_16_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_16_rank_2(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_16_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_16_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_16_rank_3(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_16_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_16_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_16_rank_4(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_16_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_16_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_16_rank_5(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_16_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_16_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_16_rank_6(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_16_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_real_kind_16_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_real_kind_16_rank_7(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,f_ptr)
  END SUBROUTINE h5ltmake_dataset_real_kind_16_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_4_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_4_rank_0(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_4_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_4_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_4_rank_1(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_4_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_4_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_4_rank_2(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_4_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_4_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_4_rank_3(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_4_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_4_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_4_rank_4(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_4_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_4_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_4_rank_5(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_4_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_4_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_4_rank_6(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_4_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_4_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_4_rank_7(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_4_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_8_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_8_rank_0(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_8_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_8_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_8_rank_1(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_8_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_8_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_8_rank_2(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_8_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_8_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_8_rank_3(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_8_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_8_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_8_rank_4(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_8_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_8_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_8_rank_5(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_8_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_8_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_8_rank_6(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_8_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_8_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_8_rank_7(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_8_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_10_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_10_rank_0(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_10_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_10_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_10_rank_1(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_10_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_10_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_10_rank_2(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_10_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_10_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_10_rank_3(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_10_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_10_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_10_rank_4(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_10_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_10_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_10_rank_5(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_10_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_10_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_10_rank_6(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_10_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_10_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_10_rank_7(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_10_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_16_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_16_rank_0(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_16_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_16_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_16_rank_1(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_16_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_16_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_16_rank_2(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_16_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_16_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_16_rank_3(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_16_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_16_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_16_rank_4(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_16_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_16_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_16_rank_5(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_16_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_16_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_16_rank_6(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_16_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_real_kind_16_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_real_kind_16_rank_7(loc_id,dset_name,type_id,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,f_ptr)
  END SUBROUTINE h5ltread_dataset_real_kind_16_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_4_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_4_rank_0(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_4_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_4_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_4_rank_1(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_4_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_4_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_4_rank_2(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_4_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_4_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_4_rank_3(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_4_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_4_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_4_rank_4(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_4_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_4_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_4_rank_5(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_4_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_4_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_4_rank_6(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_4_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_4_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_4_rank_7(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_4_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_8_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_8_rank_0(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_8_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_8_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_8_rank_1(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_8_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_8_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_8_rank_2(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_8_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_8_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_8_rank_3(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_8_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_8_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_8_rank_4(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_8_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_8_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_8_rank_5(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_8_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_8_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_8_rank_6(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_8_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_8_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_8_rank_7(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_8_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_10_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_10_rank_0(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_10_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_10_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_10_rank_1(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_10_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_10_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_10_rank_2(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_10_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_10_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_10_rank_3(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_10_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_10_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_10_rank_4(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_10_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_10_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_10_rank_5(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_10_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_10_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_10_rank_6(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_10_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_10_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_10_rank_7(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_10_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_16_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_16_rank_0(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_16_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_16_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_16_rank_1(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_16_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_16_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_16_rank_2(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_16_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_16_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_16_rank_3(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_16_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_16_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_16_rank_4(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_16_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_16_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_16_rank_5(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_16_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_16_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_16_rank_6(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_16_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_kind_16_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_float_kind_16_rank_7(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltmake_dataset_float_kind_16_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_4_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_4_rank_0(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_4_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_4_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_4_rank_1(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_4_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_4_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_4_rank_2(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_4_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_4_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_4_rank_3(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_4_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_4_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_4_rank_4(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_4_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_4_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_4_rank_5(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_4_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_4_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_4_rank_6(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_4_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_4_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_4_rank_7(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_4_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_8_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_8_rank_0(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_8_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_8_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_8_rank_1(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_8_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_8_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_8_rank_2(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_8_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_8_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_8_rank_3(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_8_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_8_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_8_rank_4(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_8_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_8_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_8_rank_5(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_8_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_8_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_8_rank_6(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_8_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_8_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_8_rank_7(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_8_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_10_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_10_rank_0(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_10_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_10_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_10_rank_1(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_10_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_10_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_10_rank_2(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_10_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_10_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_10_rank_3(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_10_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_10_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_10_rank_4(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_10_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_10_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_10_rank_5(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_10_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_10_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_10_rank_6(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_10_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_10_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_10_rank_7(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_10_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_16_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_16_rank_0(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_16_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_16_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_16_rank_1(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_16_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_16_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_16_rank_2(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_16_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_16_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_16_rank_3(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_16_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_16_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_16_rank_4(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_16_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_16_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_16_rank_5(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_16_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_16_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_16_rank_6(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_16_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_kind_16_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_float_kind_16_rank_7(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,f_ptr)
  END SUBROUTINE h5ltread_dataset_float_kind_16_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_4_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_4_rank_0(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_4_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_4_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_4_rank_1(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_4_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_4_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_4_rank_2(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_4_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_4_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_4_rank_3(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_4_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_4_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_4_rank_4(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_4_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_4_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_4_rank_5(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_4_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_4_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_4_rank_6(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_4_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_4_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_4_rank_7(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_4_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_8_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_8_rank_0(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_8_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_8_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_8_rank_1(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_8_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_8_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_8_rank_2(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_8_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_8_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_8_rank_3(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_8_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_8_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_8_rank_4(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_8_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_8_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_8_rank_5(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_8_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_8_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_8_rank_6(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_8_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_8_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_8_rank_7(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_8_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_10_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_10_rank_0(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_10_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_10_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_10_rank_1(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_10_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_10_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_10_rank_2(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_10_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_10_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_10_rank_3(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_10_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_10_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_10_rank_4(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_10_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_10_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_10_rank_5(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_10_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_10_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_10_rank_6(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_10_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_10_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_10_rank_7(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_10_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_16_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_16_rank_0(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_16_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_16_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_16_rank_1(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_16_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_16_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_16_rank_2(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_16_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_16_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_16_rank_3(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_16_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_16_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_16_rank_4(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_16_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_16_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_16_rank_5(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_16_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_16_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_16_rank_6(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_16_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_kind_16_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_double_kind_16_rank_7(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltmake_dataset_double_kind_16_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_4_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_4_rank_0(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_4_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_4_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_4_rank_1(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_4_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_4_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_4_rank_2(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_4_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_4_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_4_rank_3(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_4_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_4_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_4_rank_4(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_4_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_4_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_4_rank_5(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_4_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_4_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_4_rank_6(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_4_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_4_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_4_rank_7(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_4_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_8_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_8_rank_0(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_8_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_8_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_8_rank_1(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_8_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_8_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_8_rank_2(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_8_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_8_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_8_rank_3(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_8_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_8_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_8_rank_4(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_8_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_8_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_8_rank_5(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_8_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_8_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_8_rank_6(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_8_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_8_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_8_rank_7(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_8_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_10_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_10_rank_0(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_10_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_10_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_10_rank_1(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_10_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_10_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_10_rank_2(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_10_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_10_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_10_rank_3(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_10_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_10_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_10_rank_4(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_10_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_10_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_10_rank_5(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_10_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_10_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_10_rank_6(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_10_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_10_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_10_rank_7(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=10),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_10_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_16_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_16_rank_0(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_16_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_16_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_16_rank_1(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_16_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_16_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_16_rank_2(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_16_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_16_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_16_rank_3(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_16_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_16_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_16_rank_4(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_16_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_16_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_16_rank_5(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_16_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_16_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_16_rank_6(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_16_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_kind_16_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_double_kind_16_rank_7(loc_id,dset_name,buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    REAL(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,f_ptr)
  END SUBROUTINE h5ltread_dataset_double_kind_16_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_1_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_1_rank_0(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=1),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_1_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_1_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_1_rank_1(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_1_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_1_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_1_rank_2(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_1_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_1_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_1_rank_3(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_1_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_1_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_1_rank_4(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_1_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_1_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_1_rank_5(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_1_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_1_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_1_rank_6(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_1_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_1_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_1_rank_7(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_1_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_2_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_2_rank_0(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=2),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_2_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_2_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_2_rank_1(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_2_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_2_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_2_rank_2(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_2_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_2_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_2_rank_3(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_2_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_2_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_2_rank_4(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_2_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_2_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_2_rank_5(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_2_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_2_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_2_rank_6(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_2_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_2_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_2_rank_7(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_2_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_4_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_4_rank_0(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=4),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_4_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_4_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_4_rank_1(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_4_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_4_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_4_rank_2(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_4_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_4_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_4_rank_3(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_4_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_4_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_4_rank_4(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_4_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_4_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_4_rank_5(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_4_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_4_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_4_rank_6(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_4_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_4_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_4_rank_7(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_4_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_8_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_8_rank_0(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=8),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_8_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_8_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_8_rank_1(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_8_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_8_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_8_rank_2(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_8_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_8_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_8_rank_3(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_8_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_8_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_8_rank_4(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_8_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_8_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_8_rank_5(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_8_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_8_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_8_rank_6(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_8_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_8_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_8_rank_7(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_8_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_16_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_16_rank_0(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=16),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_16_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_16_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_16_rank_1(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_16_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_16_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_16_rank_2(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_16_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_16_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_16_rank_3(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_16_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_16_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_16_rank_4(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_16_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_16_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_16_rank_5(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_16_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_16_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_16_rank_6(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_16_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_integer_kind_16_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_integer_kind_16_rank_7(loc_id,dset_name,rank,dims,type_id,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_integer_kind_16_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_1_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_1_rank_0(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=1),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf               ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_1_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_1_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_1_rank_1(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1)            ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_1_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_1_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_1_rank_2(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1)          ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_1_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_1_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_1_rank_3(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1)        ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_1_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_1_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_1_rank_4(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1)      ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_1_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_1_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_1_rank_5(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1)    ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_1_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_1_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_1_rank_6(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1,1)  ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_1_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_1_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_1_rank_7(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=1),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1,1,1)), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_1_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_2_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_2_rank_0(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=2),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf               ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_2_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_2_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_2_rank_1(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1)            ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_2_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_2_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_2_rank_2(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1)          ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_2_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_2_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_2_rank_3(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1)        ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_2_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_2_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_2_rank_4(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1)      ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_2_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_2_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_2_rank_5(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1)    ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_2_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_2_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_2_rank_6(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1,1)  ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_2_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_2_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_2_rank_7(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=2),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1,1,1)), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_2_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_4_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_4_rank_0(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=4),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf               ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_4_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_4_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_4_rank_1(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1)            ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_4_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_4_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_4_rank_2(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1)          ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_4_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_4_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_4_rank_3(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1)        ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_4_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_4_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_4_rank_4(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1)      ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_4_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_4_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_4_rank_5(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1)    ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_4_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_4_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_4_rank_6(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1,1)  ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_4_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_4_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_4_rank_7(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=4),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1,1,1)), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_4_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_8_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_8_rank_0(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=8),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf               ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_8_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_8_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_8_rank_1(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1)            ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_8_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_8_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_8_rank_2(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1)          ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_8_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_8_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_8_rank_3(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1)        ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_8_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_8_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_8_rank_4(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1)      ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_8_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_8_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_8_rank_5(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1)    ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_8_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_8_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_8_rank_6(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1,1)  ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_8_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_8_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_8_rank_7(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=8),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1,1,1)), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_8_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_16_rank_0
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_16_rank_0(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=16),INTENT(IN), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf               ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_16_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_16_rank_1
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_16_rank_1(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1)            ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_16_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_16_rank_2
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_16_rank_2(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1)          ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_16_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_16_rank_3
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_16_rank_3(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1)        ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_16_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_16_rank_4
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_16_rank_4(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1)      ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_16_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_16_rank_5
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_16_rank_5(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1)    ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_16_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_16_rank_6
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_16_rank_6(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1,1)  ), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_16_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_kind_16_rank_7
!DEC$endif
  SUBROUTINE h5ltmake_dataset_int_kind_16_rank_7(loc_id,dset_name,rank,dims,buf,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER,          INTENT(IN) :: rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=16),INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1,1,1)), H5_INTEGER_KIND)
    errcode = h5ltmake_dataset_c(loc_id, namelen, dset_name, rank, dims, type_id, f_ptr)
  END SUBROUTINE h5ltmake_dataset_int_kind_16_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_1_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_1_rank_0(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=1),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_1_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_1_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_1_rank_1(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_1_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_1_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_1_rank_2(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_1_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_1_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_1_rank_3(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_1_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_1_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_1_rank_4(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_1_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_1_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_1_rank_5(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_1_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_1_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_1_rank_6(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_1_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_1_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_1_rank_7(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_1_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_2_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_2_rank_0(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=2),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_2_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_2_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_2_rank_1(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_2_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_2_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_2_rank_2(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_2_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_2_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_2_rank_3(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_2_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_2_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_2_rank_4(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_2_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_2_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_2_rank_5(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_2_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_2_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_2_rank_6(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_2_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_2_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_2_rank_7(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_2_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_4_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_4_rank_0(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=4),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_4_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_4_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_4_rank_1(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_4_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_4_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_4_rank_2(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_4_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_4_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_4_rank_3(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_4_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_4_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_4_rank_4(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_4_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_4_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_4_rank_5(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_4_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_4_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_4_rank_6(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_4_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_4_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_4_rank_7(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_4_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_8_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_8_rank_0(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=8),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_8_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_8_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_8_rank_1(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_8_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_8_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_8_rank_2(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_8_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_8_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_8_rank_3(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_8_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_8_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_8_rank_4(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_8_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_8_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_8_rank_5(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_8_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_8_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_8_rank_6(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_8_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_8_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_8_rank_7(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_8_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_16_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_16_rank_0(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=16),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_16_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_16_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_16_rank_1(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_16_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_16_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_16_rank_2(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_16_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_16_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_16_rank_3(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_16_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_16_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_16_rank_4(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_16_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_16_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_16_rank_5(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_16_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_16_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_16_rank_6(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_16_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_integer_kind_16_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_integer_kind_16_rank_7(loc_id,dset_name, type_id, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(hid_t),   INTENT(in) :: type_id
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_integer_kind_16_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_1_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_1_rank_0(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=1),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf               ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_1_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_1_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_1_rank_1(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1)            ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_1_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_1_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_1_rank_2(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1)          ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_1_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_1_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_1_rank_3(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1)        ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_1_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_1_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_1_rank_4(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1)      ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_1_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_1_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_1_rank_5(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1)    ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_1_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_1_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_1_rank_6(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1,1)  ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_1_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_1_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_1_rank_7(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=1),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1,1,1)), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_1_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_2_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_2_rank_0(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=2),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf               ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_2_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_2_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_2_rank_1(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1)            ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_2_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_2_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_2_rank_2(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1)          ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_2_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_2_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_2_rank_3(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1)        ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_2_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_2_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_2_rank_4(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1)      ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_2_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_2_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_2_rank_5(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1)    ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_2_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_2_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_2_rank_6(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1,1)  ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_2_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_2_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_2_rank_7(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=2),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1,1,1)), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_2_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_4_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_4_rank_0(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=4),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf               ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_4_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_4_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_4_rank_1(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1)            ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_4_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_4_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_4_rank_2(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1)          ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_4_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_4_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_4_rank_3(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1)        ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_4_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_4_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_4_rank_4(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1)      ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_4_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_4_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_4_rank_5(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1)    ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_4_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_4_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_4_rank_6(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1,1)  ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_4_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_4_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_4_rank_7(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=4),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1,1,1)), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_4_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_8_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_8_rank_0(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=8),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf               ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_8_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_8_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_8_rank_1(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1)            ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_8_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_8_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_8_rank_2(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1)          ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_8_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_8_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_8_rank_3(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1)        ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_8_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_8_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_8_rank_4(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1)      ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_8_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_8_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_8_rank_5(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1)    ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_8_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_8_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_8_rank_6(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1,1)  ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_8_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_8_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_8_rank_7(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=8),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1,1,1)), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_8_rank_7
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_16_rank_0
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_16_rank_0(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=16),INTENT(INOUT), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf               )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf               ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_16_rank_0
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_16_rank_1
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_16_rank_1(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1)            )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1)            ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_16_rank_1
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_16_rank_2
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_16_rank_2(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1)          )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1)          ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_16_rank_2
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_16_rank_3
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_16_rank_3(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1)        )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1)        ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_16_rank_3
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_16_rank_4
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_16_rank_4(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1)      )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1)      ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_16_rank_4
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_16_rank_5
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_16_rank_5(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1)    )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1)    ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_16_rank_5
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_16_rank_6
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_16_rank_6(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1)  )
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1,1)  ), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_16_rank_6
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_kind_16_rank_7
!DEC$endif
  SUBROUTINE h5ltread_dataset_int_kind_16_rank_7(loc_id,dset_name, buf,dims,errcode)
    IMPLICIT NONE
    INTEGER(hid_t)  , INTENT(IN) :: loc_id
    CHARACTER(LEN=*), INTENT(IN) :: dset_name
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims
    INTEGER(KIND=16),INTENT(INOUT), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)), TARGET :: buf
    INTEGER :: errcode 
    TYPE(C_PTR) :: f_ptr
    INTEGER(size_t) :: namelen
    INTEGER(hid_t) :: type_id
    f_ptr = C_LOC(buf(1,1,1,1,1,1,1))
    namelen = LEN(dset_name)
    type_id = h5kind_to_type(KIND(buf(1,1,1,1,1,1,1)), H5_INTEGER_KIND)
    errcode = h5ltread_dataset_c(loc_id, namelen, dset_name, type_id, f_ptr)
  END SUBROUTINE h5ltread_dataset_int_kind_16_rank_7
END MODULE H5LT
