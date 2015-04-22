! fortran/src/H5config_f.inc. Generated from fortran/src/H5config_f.inc.in by configure

! Define if we have parallel support
#define H5_HAVE_PARALLEL @CMAKE_H5_HAVE_PARALLEL@

#if H5_HAVE_PARALLEL==0
#undef H5_HAVE_PARALLEL
#endif

! Define if the intrinsic function STORAGE_SIZE exists
#define H5_FORTRAN_HAVE_STORAGE_SIZE @FORTRAN_HAVE_STORAGE_SIZE@

#if H5_FORTRAN_HAVE_STORAGE_SIZE==0
#undef H5_FORTRAN_HAVE_STORAGE_SIZE
#endif

! Define if the intrinsic function SIZEOF exists
#define H5_FORTRAN_HAVE_SIZEOF @FORTRAN_HAVE_SIZEOF@

#if H5_FORTRAN_HAVE_SIZEOF==0
#undef H5_FORTRAN_HAVE_SIZEOF
#endif

! Define if the intrinsic function C_SIZEOF exists
#define H5_FORTRAN_HAVE_C_SIZEOF @FORTRAN_HAVE_C_SIZEOF@

#if H5_FORTRAN_HAVE_C_SIZEOF==0
#undef H5_FORTRAN_HAVE_C_SIZEOF
#endif
