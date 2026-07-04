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
! fortran/H5config_f.inc. Generated from fortran/src/H5config_f.inc.cmake by CMake

! Define if there is parallel support
#cmakedefine H5_HAVE_PARALLEL

! Define if MPI supports mpi_f08 module
#cmakedefine H5_HAVE_MPI_F08

! Define if there is subfiling support
#cmakedefine H5_HAVE_SUBFILING_VFD

! Define if on APPLE
#cmakedefine H5_HAVE_DARWIN

! Define if the intrinsic function STORAGE_SIZE exists
#cmakedefine H5_FORTRAN_HAVE_STORAGE_SIZE

! Define if the intrinsic function SIZEOF exists
#cmakedefine H5_FORTRAN_HAVE_SIZEOF

! Define if the intrinsic function C_SIZEOF exists
#cmakedefine H5_FORTRAN_HAVE_C_SIZEOF

! Define if allocatable character is supported
#cmakedefine H5_FORTRAN_HAVE_CHAR_ALLOC

! Define if the intrinsic function C_LONG_DOUBLE exists
#cmakedefine H5_FORTRAN_HAVE_C_LONG_DOUBLE

! Define if Fortran C_LONG_DOUBLE is different from C_DOUBLE
#cmakedefine H5_FORTRAN_C_LONG_DOUBLE_IS_UNIQUE

! Define if Fortran C_BOOL is different from default LOGICAL
#cmakedefine H5_FORTRAN_C_BOOL_IS_UNIQUE

! Define  MPI Fortran KIND of LOGICAL
#cmakedefine H5_MPI_LOGICAL_KIND @H5_MPI_LOGICAL_KIND@

! Define if Fortran supports ISO_FORTRAN_ENV (F08)
#cmakedefine H5_HAVE_ISO_FORTRAN_ENV

! Define the size of C's double
#define H5_SIZEOF_DOUBLE @H5_SIZEOF_DOUBLE@

! Define the size of C's long double
#define H5_SIZEOF_LONG_DOUBLE @H5_SIZEOF_LONG_DOUBLE@

! Define the maximum decimal precision for reals
#define H5_PAC_FC_MAX_REAL_PRECISION @H5_PAC_FC_MAX_REAL_PRECISION@

! If C has quad precision
#cmakedefine H5_HAVE_FLOAT128

! Define if INTEGER*16 is available
#cmakedefine H5_HAVE_Fortran_INTEGER_SIZEOF_16

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

! Define if deprecated public API symbols are disabled
#cmakedefine H5_NO_DEPRECATED_SYMBOLS

! For major interface/format changes
#define H5_VERS_MAJOR @H5_VERS_MAJOR@

! For minor interface/format changes
#define H5_VERS_MINOR @H5_VERS_MINOR@

! For tweaks, bug-fixes, or development
#define H5_VERS_RELEASE @H5_VERS_RELEASE@

! macros for comparing versions
#define H5_VERSION_GE(Maj, Min, Rel)                                                   \
    (((H5_VERS_MAJOR == Maj) && (H5_VERS_MINOR == Min) && (H5_VERS_RELEASE >= Rel)) || \
     ((H5_VERS_MAJOR == Maj) && (H5_VERS_MINOR > Min)) || (H5_VERS_MAJOR > Maj))

#define H5_VERSION_LE(Maj, Min, Rel)                                                   \
    (((H5_VERS_MAJOR == Maj) && (H5_VERS_MINOR == Min) && (H5_VERS_RELEASE <= Rel)) || \
     ((H5_VERS_MAJOR == Maj) && (H5_VERS_MINOR < Min)) || (H5_VERS_MAJOR < Maj))

