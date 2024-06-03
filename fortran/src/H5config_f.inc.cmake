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
! fortran/H5config_f.inc. Generated from fortran/src/H5config_f.inc.cmake by CMake

! Define if there is parallel support
#cmakedefine01 CMAKE_H5_HAVE_PARALLEL
#if CMAKE_H5_HAVE_PARALLEL == 0
#undef H5_HAVE_PARALLEL
#else
#define H5_HAVE_PARALLEL
#endif

! Define if MPI supports mpi_f08 module
#cmakedefine01 CMAKE_H5_HAVE_MPI_F08
#if CMAKE_H5_HAVE_MPI_F08 == 0
#undef H5_HAVE_MPI_F08
#else
#define H5_HAVE_MPI_F08
#endif

! Define if there is subfiling support
#cmakedefine01 CMAKE_H5_HAVE_SUBFILING_VFD
#if CMAKE_H5_HAVE_SUBFILING_VFD == 0
#undef H5_HAVE_SUBFILING_VFD
#else
#define H5_HAVE_SUBFILING_VFD
#endif

! Define if on APPLE
#cmakedefine01 CMAKE_H5_HAVE_DARWIN
#if CMAKE_H5_HAVE_DARWIN == 0
#undef H5_HAVE_DARWIN
#else
#define H5_HAVE_DARWIN
#endif

! Define if the intrinsic function STORAGE_SIZE exists
#cmakedefine01 CMAKE_H5_FORTRAN_HAVE_STORAGE_SIZE
#if CMAKE_H5_FORTRAN_HAVE_STORAGE_SIZE == 0
#undef H5_FORTRAN_HAVE_STORAGE_SIZE
#else
#define H5_FORTRAN_HAVE_STORAGE_SIZE
#endif

! Define if the intrinsic function SIZEOF exists
#cmakedefine01 CMAKE_H5_FORTRAN_HAVE_SIZEOF
#if CMAKE_H5_FORTRAN_HAVE_SIZEOF == 0
#undef H5_FORTRAN_HAVE_SIZEOF
#else
#define H5_FORTRAN_HAVE_SIZEOF
#endif

! Define if the intrinsic function C_SIZEOF exists
#cmakedefine01 CMAKE_H5_FORTRAN_HAVE_C_SIZEOF
#if CMAKE_H5_FORTRAN_HAVE_C_SIZEOF == 0
#undef H5_FORTRAN_HAVE_C_SIZEOF
#else
#define H5_FORTRAN_HAVE_C_SIZEOF
#endif

! Define if allocatable character is supported
#define H5_FORTRAN_HAVE_CHAR_ALLOC @H5_FORTRAN_HAVE_CHAR_ALLOC@

! Define if the intrinsic function C_LONG_DOUBLE exists
#define H5_FORTRAN_HAVE_C_LONG_DOUBLE @H5_FORTRAN_HAVE_C_LONG_DOUBLE@

! Define if Fortran C_LONG_DOUBLE is different from C_DOUBLE
#define H5_FORTRAN_C_LONG_DOUBLE_IS_UNIQUE @H5_FORTRAN_C_LONG_DOUBLE_IS_UNIQUE@

! Define if Fortran C_BOOL is different from default LOGICAL
#define H5_FORTRAN_C_BOOL_IS_UNIQUE @H5_FORTRAN_C_BOOL_IS_UNIQUE@

! Define  MPI Fortran KIND of LOGICAL
#cmakedefine01 CMAKE_H5_MPI_LOGICAL_KIND
#if CMAKE_H5_MPI_LOGICAL_KIND == 0
#undef H5_MPI_LOGICAL_KIND
#else
#define H5_MPI_LOGICAL_KIND @H5_MPI_LOGICAL_KIND@
#endif

! Define if Fortran supports ISO_FORTRAN_ENV (F08)
#cmakedefine01 CMAKE_H5_HAVE_ISO_FORTRAN_ENV
#if CMAKE_H5_HAVE_ISO_FORTRAN_ENV == 0
#undef H5_HAVE_ISO_FORTRAN_ENV
#else
#define H5_HAVE_ISO_FORTRAN_ENV
#endif

! Define the size of C's double
#define H5_SIZEOF_DOUBLE @H5_SIZEOF_DOUBLE@

! Define the size of C's long double
#define H5_SIZEOF_LONG_DOUBLE @H5_SIZEOF_LONG_DOUBLE@

! Define the maximum decimal precision for reals
#define H5_PAC_FC_MAX_REAL_PRECISION @H5_PAC_FC_MAX_REAL_PRECISION@

! If C has quad precision
#cmakedefine01 CMAKE_H5_HAVE_FLOAT128
#if CMAKE_H5_HAVE_FLOAT128 == 0
#undef H5_HAVE_FLOAT128
#else
#define H5_HAVE_FLOAT128
#endif

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

! Define if deprecated public API symbols are disabled
#cmakedefine01 CMAKE_NO_DEPRECATED_SYMBOLS
#if CMAKE_NO_DEPRECATED_SYMBOLS == 0
#undef H5_NO_DEPRECATED_SYMBOLS
#else
#define H5_NO_DEPRECATED_SYMBOLS
#endif

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

