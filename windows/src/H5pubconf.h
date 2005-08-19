/* H5pubconf.h is adapted from UNIX platform and manually maintained on the windows platform. */

/*#define H5_HAVE_TM_ZONE 1 windows do not use this constant.*/  
#define H5_MALLOC_WORKS 1

/* code warrior returns 0 in malloc(0) */
#if defined(__MWERKS__)
#undef H5_MALLOC_WORKS 
#endif

/* 
code warrior v.8 does not allow shared writing by default; 
the feature can be enabled by defining
_MSL_ALLOW_SHARED_WRITING to 1
in the file file_io.win32.c and including it on the projects
*/
#if defined(__MWERKS__)
#define H5_NO_SHARED_WRITING
#endif


#define H5_STDC_HEADERS 1
#define H5_HAVE_ATTRIBUTE 1
#undef H5_HAVE_ATTRIBUTE
#define H5_HAVE_LARGE_HSIZET 1 
#ifdef __MWERKS__ 
#define H5_PRINTF_LL_WIDTH "ll" 
#else 
#define H5_PRINTF_LL_WIDTH "I64" 
#endif
#define H5_HAVE___int64
#define H5_SIZEOF___INT64 8 
#define H5_SIZEOF_CHAR 1
#define H5_SIZEOF_DOUBLE 8
#define H5_SIZEOF_FLOAT 4
#define H5_SIZEOF_INT 4


#define H5_SIZEOF_LONG 4

/*#if defined __INTEL_COMPILER
#define H5_SIZEOF_LONG_DOUBLE 12
#else*/

#define H5_SIZEOF_LONG_DOUBLE 8
/*#endif*/
#define H5_SIZEOF_OFF_T 4
#define H5_SIZEOF_SHORT 2
#define H5_SIZEOF_SIZE_T 4


#define H5_HAVE_DIFFTIME 1
#define H5_HAVE_FORK 1
#define H5_HAVE_GETHOSTNAME 1
#define H5_HAVE_IOCTL 1
#define H5_HAVE_LONGJMP 1
#define H5_HAVE_SIGACTION 1
#define H5_HAVE_SIGNAL 1
#define H5_HAVE__SNPRINTF 1
#define H5_HAVE_STRDUP 1
#define H5_HAVE_SYSTEM 1
#define H5_HAVE__VSNPRINTF 1
#define  H5_HAVE_IO_H 1
#define H5_HAVE_SETJMP_H 1
#define H5_HAVE_STDDEF_H 1
#define H5_HAVE_SYS_STAT_H 1
#define H5_HAVE_SYS_TIMEB 1
#define H5_HAVE_SYS_TYPES_H 1
#define H5_HAVE_WINSOCK_H 1

/* comment the following line out if you are not using check sum filter*/
#define H5_HAVE_FILTER_FLETCHER32 1

/* comment the following line out if you are not using shuffle filter*/
#define H5_HAVE_FILTER_SHUFFLE 1

/* comment the following line out if you are not using N-bit filter*/
#define H5_HAVE_FILTER_NBIT 1

/* comment the following line out if you are not using N-bit filter*/
#define H5_HAVE_FILTER_SCALEOFFSET 1

/* comment the following two lines out if you are not using deflate(gzip) filter*/
#define H5_HAVE_FILTER_DEFLATE 1
#define H5_HAVE_ZLIB_H 1

/* comment the following two lines out if you are not using szip filter*/
#define H5_HAVE_SZLIB_H 1
#define H5_HAVE_FILTER_SZIP 1

/* comment the following line out if you are not using N-bit filter*/
/* #define H5_HAVE_FILTER_NBIT 1*/

#if defined(__MWERKS__) || defined(__cplusplus)
#define H5_inline inline
#else
#define H5_inline __inline
#endif

#if _MSC_VER >= 1300 /* .Net supports FUNCTION */
#define H5_HAVE_FUNCTION 1
#define H5_ULLONG_TO_FP_CAST_WORKS 1
#define H5_HW_FP_TO_LLONG_NOT_WORKS 1
#if defined __INTEL_COMPILER
#undef H5_LLONG_TO_FP_CAST_WORKS
#else
/*#define H5_LLONG_TO_FP_CAST_WORKS*/

#endif
#else
#undef H5_HAVE_FUNCTION
#undef H5_ULLONG_TO_FP_CAST_WORKS 
#define H5_LLONG_TO_FP_CAST_WORKS
#endif 

#define H5_FC_FUNC_(name, NAME) NAME
#define FC_FUNC_(name, NAME) NAME

#define H5_HAVE_TMPFILE 1

/*Users want to build and test hdf5 library with thread safe enabled,
  Make the following block active
*/
/* 
#if defined _DLL
#define H5_HAVE_THREADSAFE 1
#define H5_HAVE_SYSTEM_SCOPE_THREADS 1
#if defined TTSAFE_H
#define sleep Sleep
#endif
#endif
*/
