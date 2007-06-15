/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* H5pubconf.h is adapted from UNIX platform and manually maintained on the windows platform. */

#define H5_SIZEOF___INT64 8 
#define H5_SIZEOF_CHAR 1
#define H5_SIZEOF_DOUBLE 8
#define H5_SIZEOF_FLOAT 4
#define H5_SIZEOF_INT 4
#define H5_SIZEOF_LONG 4
#define H5_SIZEOF_LONG_DOUBLE 8
#define H5_SIZEOF_OFF_T 4
#define H5_SIZEOF_SHORT 2
#ifndef _WIN64
#define H5_SIZEOF_SIZE_T 4
#else
#define H5_SIZEOF_SIZE_T 8
#endif

/*#if defined __INTEL_COMPILER
#define H5_SIZEOF_LONG_DOUBLE 12
#else*/
/*#endif*/

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
/* #define H5_HAVE_ATTRIBUTE 1 */
#undef H5_HAVE_ATTRIBUTE
#define H5_HAVE_LARGE_HSIZET 1 
#ifdef __MWERKS__ 
#define H5_PRINTF_LL_WIDTH "ll" 
#else 
#define H5_PRINTF_LL_WIDTH "I64" 
#endif

#define H5_HAVE___int64


#define H5_HAVE_DIFFTIME 1
#define H5_HAVE_FORK 1
#define H5_HAVE_GETHOSTNAME 1
#define H5_HAVE_IOCTL 1
#define H5_HAVE_LONGJMP 1
#define H5_HAVE_SIGACTION 1
#define H5_HAVE_SIGNAL 1
#define H5_HAVE_SNPRINTF 1
#define H5_HAVE_STRDUP 1
#define H5_HAVE_SYSTEM 1
#define H5_HAVE_VSNPRINTF 1
#define H5_HAVE_IO_H 1
#define H5_HAVE_SETJMP_H 1
#define H5_HAVE_STDDEF_H 1
#define H5_HAVE_SYS_STAT_H 1
#define H5_HAVE_SYS_TIMEB 1
#define H5_HAVE_SYS_TYPES_H 1
#define H5_HAVE_WINSOCK_H 1
#define H5_HAVE_STATI64 1

 /* These 64-bit functions are only supported in  .NET Framework 2.0 or later */
 #if _MSC_VER >= 1400
 #define H5_HAVE_FSEEKI64 1
 #define H5_HAVE_CHSIZE_S 1
 #define H5_HAVE_FTELLI64 1
 #endif /* _MSC_VER >= 1400 */

/* comment the following line out if the memory buffers being written to 
   disk should not be cleared before writing. */
#define H5_CLEAR_MEMORY 1

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

/* change the following line if you would like to change the default file driver */
#define H5_DEFAULT_VFD H5FD_WINDOWS

/* comment the following line out if you don't want to build the windows file
   driver */
#define H5_HAVE_WINDOWS 1

#ifdef H5_HAVE_WINDOWS
/* uncomment the following line if you would like to use the buffered stdio
   functions in the Windows file driver. */
// #define WINDOWS_USE_STDIO 1
#endif /* H5_HAVE_WINDOWS */

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

/* (Remove the comment signs to enable thread safe on windows) 
#if defined _DLL
#define H5_HAVE_THREADSAFE 1
#define H5_HAVE_SYSTEM_SCOPE_THREADS 1
#if defined TTSAFE_H
#define sleep Sleep
#endif
#endif
*/


/* Data accuracy is prefered to speed during data conversions */
#define H5_WANT_DATA_ACCURACY 1

/* Check exception handling functions during data conversions */
#define H5_WANT_DCONV_EXCEPTION 1
#if _MSC_VER >=1400
/* visual studio 2005 doesn't support size of setvbuf to be less thn 1,This is a hacking, we would like to wait
visual studio 2005 to fix this problem.
*/

#define HDsetvbuf(F,S,M,Z) (((Z)>1)?setvbuf(F,S,M,Z):setvbuf(F,S,M,2))

#else
#define HDsetvbuf(F,S,M,Z) setvbuf(F,S,M,Z)
#endif


/* uncomment the following line if we want parallel HDF5 support */
/* #define H5_HAVE_PARALLEL  */

/* uncomment the following line if you need the library to perform "strict" 
   memory operations, which is useful when debugging with a memory checking 
   tool like Purify, etc. */
/* #define H5_USING_MEMCHECKER  1  */