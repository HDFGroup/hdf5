! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the COPYING file, which can be found at the root of the source code       *
!   distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
!   If you do not have access to either file, you may request a copy from     *
!   help@hdfgroup.org.                                                        *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! fortran/src/H5config_f.inc. Generated from fortran/src/H5config_f.inc.in by configure

! Define if we have parallel support
#cmakedefine01 H5_HAVE_PARALLEL
#if H5_HAVE_PARALLEL == 0
#undef H5_HAVE_PARALLEL
#endif

! Define if the intrinsic function STORAGE_SIZE exists
#define H5_FORTRAN_HAVE_STORAGE_SIZE @H5_FORTRAN_HAVE_STORAGE_SIZE@

! Define if the intrinsic function SIZEOF exists
#define H5_FORTRAN_HAVE_SIZEOF @H5_FORTRAN_HAVE_SIZEOF@

! Define if the intrinsic function C_SIZEOF exists
#define H5_FORTRAN_HAVE_C_SIZEOF @H5_FORTRAN_HAVE_C_SIZEOF@

! Define if the intrinsic C_LONG_DOUBLE exists
#define H5_FORTRAN_HAVE_C_LONG_DOUBLE @H5_FORTRAN_HAVE_C_LONG_DOUBLE@

! Define if Fortran C_LONG_DOUBLE is different from C_DOUBLE
#define H5_FORTRAN_C_LONG_DOUBLE_IS_UNIQUE @H5_FORTRAN_C_LONG_DOUBLE_IS_UNIQUE@

! Define if the intrinsic module ISO_FORTRAN_ENV exists
#define H5_HAVE_ISO_FORTRAN_ENV @H5_HAVE_ISO_FORTRAN_ENV@

! should this be ${HDF_PREFIX} instead of H5 MSB
#define H5_SIZEOF_DOUBLE @H5_SIZEOF_DOUBLE@

! should this be ${HDF_PREFIX} instead of H5 MSB
#define H5_SIZEOF_LONG_DOUBLE @H5_SIZEOF_LONG_DOUBLE@

! Define the maximum decimal precision for reals
#define H5_PAC_FC_MAX_REAL_PRECISION @H5_PAC_FC_MAX_REAL_PRECISION@

! If C has quad precision
#define H5_HAVE_FLOAT128 @H5_HAVE_FLOAT128@

! Define if INTEGER*16 is available
#define H5_HAVE_Fortran_INTEGER_SIZEOF_16 @H5_HAVE_Fortran_INTEGER_SIZEOF_16@

! Maximum decimal precision for C
#define H5_PAC_C_MAX_REAL_PRECISION @H5_PAC_C_MAX_REAL_PRECISION@

! number of valid REAL KINDs
#define H5_H5CONFIG_F_NUM_RKIND @H5_H5CONFIG_F_NUM_RKIND@

! valid REAL KINDs (need to have a matching C counter-part)
#define H5_H5CONFIG_F_RKIND @H5_H5CONFIG_F_RKIND@

! valid REAL KINDs (need to have a matching C counter-part)
#define H5_H5CONFIG_F_RKIND_SIZEOF @H5_H5CONFIG_F_RKIND_SIZEOF@

! number of valid INTEGER KINDs
#define H5_H5CONFIG_F_NUM_IKIND @H5_H5CONFIG_F_NUM_IKIND@

! valid INTEGER KINDs (need to have a matching C counter-part)
#define H5_H5CONFIG_F_IKIND @H5_H5CONFIG_F_IKIND@

! Fortran compiler id
#define H5_Fortran_COMPILER_ID @CMAKE_Fortran_COMPILER_ID@
