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
/* src/H5config.h.  Generated from H5config.h.in by configure.  */
/* src/H5config.h.in.  Generated from configure.ac by autoheader.  */

/* Define if building universal (internal helper macro) */
/* #undef H5_AC_APPLE_UNIVERSAL_BUILD */

/* Define if your system generates wrong code for log2 routine. */
/* #undef H5_BAD_LOG2_CODE_GENERATED */

/* Define if the memory buffers being written to disk should be cleared before
   writing. */
#define H5_CLEAR_MEMORY 1

/* Define if your system can handle converting denormalized floating-point
   values. */
#define H5_CONVERT_DENORMAL_FLOAT 1

/* Define if C++ compiler recognizes offsetof */
#define H5_CXX_HAVE_OFFSETOF 1

/* Define the default virtual file driver to compile */
#define H5_DEFAULT_VFD H5FD_SEC2

/* Define if `dev_t' is a scalar */
#define H5_DEV_T_IS_SCALAR 1

/* Define to dummy `main' function (if any) required to link to the Fortran
   libraries. */
/* #undef H5_FC_DUMMY_MAIN */

/* Define if F77 and FC dummy `main' functions are identical. */
/* #undef H5_FC_DUMMY_MAIN_EQ_F77 */

/* Define to a macro mangling the given C identifier (in lower and upper
   case), which must not contain underscores, for linking with Fortran. */
/* #undef H5_FC_FUNC */

/* As FC_FUNC, but for C identifiers containing underscores. */
/* #undef H5_FC_FUNC_ */

/* Define if your system can handle overflow converting floating-point to
   integer values. */
#define H5_FP_TO_INTEGER_OVERFLOW_WORKS 1

/* Define if your system roundup accurately converting floating-point to
   unsigned long long values. */
#define H5_FP_TO_ULLONG_ACCURATE 1

/* Define if your system has right maximum convert floating-point to unsigned
   long long values. */
#define H5_FP_TO_ULLONG_RIGHT_MAXIMUM 1

/* Define if gettimeofday() populates the tz pointer passed in */
/* #undef H5_GETTIMEOFDAY_GIVES_TZ */

/* Define to 1 if you have the `alarm' function. */
#define H5_HAVE_ALARM 1

/* Define if the __attribute__(()) extension is present */
/* #undef H5_HAVE_ATTRIBUTE */

/* Define to 1 if you have the `BSDgettimeofday' function. */
/* #undef H5_HAVE_BSDGETTIMEOFDAY */

/* Define if the compiler understands C99 designated initialization of structs
   and unions */
#define H5_HAVE_C99_DESIGNATED_INITIALIZER 1

/* Define if the compiler understands the __func__ keyword */
#define H5_HAVE_C99_FUNC 1

/* Define to 1 if you have the `clock_gettime' function. */
#define H5_HAVE_CLOCK_GETTIME 1

/* Define if the function stack tracing code is to be compiled in */
/* #undef H5_HAVE_CODESTACK */

/* Define to 1 if you have the declaration of `tzname', and to 0 if you don't.
   */
/* #undef H5_HAVE_DECL_TZNAME */

/* Define to 1 if you have the `difftime' function. */
#define H5_HAVE_DIFFTIME 1

/* Define if the direct I/O virtual file driver should be compiled */
/* #undef H5_HAVE_DIRECT */

/* Define to 1 if you have the <dirent.h> header file. */
#define H5_HAVE_DIRENT_H 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#define H5_HAVE_DLFCN_H 1

/* Define to 1 if you have the <dmalloc.h> header file. */
/* #undef H5_HAVE_DMALLOC_H */

/* Define if library information should be embedded in the executables */
/* #undef H5_HAVE_EMBEDDED_LIBINFO */

/* Define to 1 if you have the <features.h> header file. */
/* #undef H5_HAVE_FEATURES_H */

/* Define if support for deflate (zlib) filter is enabled */
#define H5_HAVE_FILTER_DEFLATE 1

/* Define if support for Fletcher32 checksum is enabled */
#define H5_HAVE_FILTER_FLETCHER32 1

/* Define if support for nbit filter is enabled */
#define H5_HAVE_FILTER_NBIT 1

/* Define if support for scaleoffset filter is enabled */
#define H5_HAVE_FILTER_SCALEOFFSET 1

/* Define if support for shuffle filter is enabled */
#define H5_HAVE_FILTER_SHUFFLE 1

/* Define if support for szip filter is enabled */
/* #undef H5_HAVE_FILTER_SZIP */

/* Define to 1 if you have the `fork' function. */
/* #undef H5_HAVE_FORK */

/* Define to 1 if you have the `frexpf' function. */
#define H5_HAVE_FREXPF 1

/* Define to 1 if you have the `frexpl' function. */
#define H5_HAVE_FREXPL 1

/* Define to 1 if you have the `fseeko' function. */
#define H5_HAVE_FSEEKO 1

/* Define to 1 if you have the `fseek64' function. */
/* #undef H5_HAVE_FSEEKO64 */

/* Define to 1 if you have the `fstat64' function. */
/* #undef H5_HAVE_FSTAT64 */

/* Define to 1 if you have the `ftello' function. */
#define H5_HAVE_FTELLO 1

/* Define to 1 if you have the `ftruncate64' function. */
/* #undef H5_HAVE_FTRUNCATE64 */

/* Define if the compiler understands the __FUNCTION__ keyword */
/* #undef H5_HAVE_FUNCTION */

/* Define to 1 if you have the `GetConsoleScreenBufferInfo' function. */
/* #undef H5_HAVE_GETCONSOLESCREENBUFFERINFO */

/* Define to 1 if you have the `gethostname' function. */
#define H5_HAVE_GETHOSTNAME 1

/* Define to 1 if you have the `getpwuid' function. */
/* #undef H5_HAVE_GETPWUID */

/* Define to 1 if you have the `getrusage' function. */
/* #undef H5_HAVE_GETRUSAGE */

/* Define to 1 if you have the `gettextinfo' function. */
/* #undef H5_HAVE_GETTEXTINFO */

/* Define to 1 if you have the `gettimeofday' function. */
#define H5_HAVE_GETTIMEOFDAY 1

/* Define to 1 if you have the `get_fpc_csr' function. */
/* #undef H5_HAVE_GET_FPC_CSR */

/* Define if we have GPFS support */
/* #undef H5_HAVE_GPFS */

/* Define to 1 if you have the <gpfs.h> header file. */
/* #undef H5_HAVE_GPFS_H */

/* Define if library will contain instrumentation to detect correct
   optimization operation */
#define H5_HAVE_INSTRUMENTED_LIBRARY 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define H5_HAVE_INTTYPES_H 1

/* Define to 1 if you have the `ioctl' function. */
#define H5_HAVE_IOCTL 1

/* Define to 1 if you have the <io.h> header file. */
/* #undef H5_HAVE_IO_H */

/* Define to 1 if you have the `dl' library (-ldl). */
#define H5_HAVE_LIBDL 1

/* Define to 1 if you have the `dmalloc' library (-ldmalloc). */
/* #undef H5_HAVE_LIBDMALLOC */

/* Define to 1 if you have the `lmpe' library (-llmpe). */
/* #undef H5_HAVE_LIBLMPE */

/* Define to 1 if you have the `m' library (-lm). */
#define H5_HAVE_LIBM 1

/* Define to 1 if you have the `mpe' library (-lmpe). */
/* #undef H5_HAVE_LIBMPE */

/* Define to 1 if you have the `mpi' library (-lmpi). */
/* #undef H5_HAVE_LIBMPI */

/* Define to 1 if you have the `mpich' library (-lmpich). */
/* #undef H5_HAVE_LIBMPICH */

/* Define to 1 if you have the `mpio' library (-lmpio). */
/* #undef H5_HAVE_LIBMPIO */

/* Define to 1 if you have the `nsl' library (-lnsl). */
/* #undef H5_HAVE_LIBNSL */

/* Define to 1 if you have the `pthread' library (-lpthread). */
/* #undef H5_HAVE_LIBPTHREAD */

/* Define to 1 if you have the `socket' library (-lsocket). */
/* #undef H5_HAVE_LIBSOCKET */

/* Define to 1 if you have the `sz' library (-lsz). */
/* #undef H5_HAVE_LIBSZ */

/* Define to 1 if you have the `z' library (-lz). */
/* #undef H5_HAVE_LIBZ */

/* Define to 1 if you have the `longjmp' function. */
#define H5_HAVE_LONGJMP 1

/* Define to 1 if you have the `lseek64' function. */
/* #undef H5_HAVE_LSEEK64 */

/* Define to 1 if you have the `lstat' function. */
/* #undef H5_HAVE_LSTAT */

/* Define to 1 if you have the <mach/mach_time.h> header file. */
/* #undef H5_HAVE_MACH_MACH_TIME_H */

/* Define to 1 if you have the <memory.h> header file. */
/* #undef H5_HAVE_MEMORY_H */

/* Define if we have MPE support */
/* #undef H5_HAVE_MPE */

/* Define to 1 if you have the <mpe.h> header file. */
/* #undef H5_HAVE_MPE_H */

/* Define if MPI_File_get_size works correctly */
/* #undef H5_HAVE_MPI_GET_SIZE */

/* Define if `MPI_Comm_c2f' and `MPI_Comm_f2c' exists */
/* #undef H5_HAVE_MPI_MULTI_LANG_Comm */

/* Define if `MPI_Info_c2f' and `MPI_Info_f2c' exists */
/* #undef H5_HAVE_MPI_MULTI_LANG_Info */

/* Define if we have parallel support */
/* #undef H5_HAVE_PARALLEL */

/* Define to 1 if you have the <pthread.h> header file. */
/* #undef H5_HAVE_PTHREAD_H */

/* Define to 1 if you have the `random' function. */
#define H5_HAVE_RANDOM 1

/* Define to 1 if you have the `rand_r' function. */
/* #define H5_HAVE_RAND_R 1 */

/* Define to 1 if you have the `setjmp' function. */
#define H5_HAVE_SETJMP 1

/* Define to 1 if you have the <setjmp.h> header file. */
#define H5_HAVE_SETJMP_H 1

/* Define to 1 if you have the `setsysinfo' function. */
/* #undef H5_HAVE_SETSYSINFO */

/* Define to 1 if you have the `siglongjmp' function. */
#define H5_HAVE_SIGLONGJMP 1

/* Define to 1 if you have the `signal' function. */
#define H5_HAVE_SIGNAL 1

/* Define to 1 if you have the `sigprocmask' function. */
/* #undef H5_HAVE_SIGPROCMASK */

/* Define to 1 if you have the `sigsetjmp' function. */
/* #undef H5_HAVE_SIGSETJMP */

/* Define to 1 if you have the `snprintf' function. */
#define H5_HAVE_SNPRINTF 1

/* Define to 1 if you have the `srandom' function. */
#define H5_HAVE_SRANDOM 1

/* Define to 1 if you have the `stat64' function. */
/* #undef H5_HAVE_STAT64 */

/* Define if `struct stat' has the `st_blocks' field */
/* #undef H5_HAVE_STAT_ST_BLOCKS */

/* Define to 1 if you have the <stddef.h> header file. */
#define H5_HAVE_STDDEF_H 1

/* Define to 1 if you have the <stdint.h> header file. */
/* #undef H5_HAVE_STDINT_H */

/* Define to 1 if you have the <stdlib.h> header file. */
#define H5_HAVE_STDLIB_H 1

/* Define to 1 if you have the `strdup' function. */
#define H5_HAVE_STRDUP 1

/* Define to 1 if you have the <strings.h> header file. */
#define H5_HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define H5_HAVE_STRING_H 1

/* Define if `struct text_info' is defined */
/* #undef H5_HAVE_STRUCT_TEXT_INFO */

/* Define if `struct timezone' is defined */
/* #undef H5_HAVE_STRUCT_TIMEZONE */

/* Define to 1 if `tm_zone' is member of `struct tm'. */
#define H5_HAVE_STRUCT_TM_TM_ZONE 1

/* Define if `struct videoconfig' is defined */
/* #undef H5_HAVE_STRUCT_VIDEOCONFIG */

/* Define to 1 if you have the `symlink' function. */
/* #undef H5_HAVE_SYMLINK */

/* Define to 1 if you have the `system' function. */
#define H5_HAVE_SYSTEM 1

/* Define to 1 if you have the <sys/fpu.h> header file. */
/* #undef H5_HAVE_SYS_FPU_H */

/* Define to 1 if you have the <sys/ioctl.h> header file. */
#define H5_HAVE_SYS_IOCTL_H 1

/* Define to 1 if you have the <sys/proc.h> header file. */
/* #undef H5_HAVE_SYS_PROC_H */

/* Define to 1 if you have the <sys/resource.h> header file. */
#define H5_HAVE_SYS_RESOURCE_H 1

/* Define to 1 if you have the <sys/socket.h> header file. */
#define H5_HAVE_SYS_SOCKET_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define H5_HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/sysinfo.h> header file. */
/* #undef H5_HAVE_SYS_SYSINFO_H */

/* Define to 1 if you have the <sys/timeb.h> header file. */
#define H5_HAVE_SYS_TIMEB_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define H5_HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define H5_HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <szlib.h> header file. */
/* #undef H5_HAVE_SZLIB_H */

/* Define if we have thread safe support */
/* #undef H5_HAVE_THREADSAFE */

/* Define if `timezone' is a global variable */
#define H5_HAVE_TIMEZONE 1

/* Define if the ioctl TIOCGETD is defined */
#define H5_HAVE_TIOCGETD 1

/* Define if the ioctl TIOGWINSZ is defined */
#define H5_HAVE_TIOCGWINSZ 1

/* Define to 1 if you have the `tmpfile' function. */
#define H5_HAVE_TMPFILE 1

/* Define if `tm_gmtoff' is a member of `struct tm' */
/* #undef H5_HAVE_TM_GMTOFF */

/* Define to 1 if your `struct tm' has `tm_zone'. Deprecated, use
   `HAVE_STRUCT_TM_TM_ZONE' instead. */
#define H5_HAVE_TM_ZONE 1

/* Define to 1 if you don't have `tm_zone' but do have the external array
   `tzname'. */
/* #undef H5_HAVE_TZNAME */

/* Define to 1 if you have the <unistd.h> header file. */
#define H5_HAVE_UNISTD_H 1

/* Define to 1 if you have the `vasprintf' function. */
/* #undef H5_HAVE_VASPRINTF */

/* Define to 1 if you have the `vsnprintf' function. */
#define H5_HAVE_VSNPRINTF 1

/* Define to 1 if you have the `waitpid' function. */
#define H5_HAVE_WAITPID 1

/* Define if your system has OpenVMS path name. This macro is added by hand. */
#define H5_HAVE_VMS_PATH 1

/* Define if your system has window style path name. */
/* #undef H5_HAVE_WINDOW_PATH */

/* Define to 1 if you have the <winsock.h> header file. */
/* #undef H5_HAVE_WINSOCK2_H */

/* Define to 1 if you have the <zlib.h> header file. */
#define H5_HAVE_ZLIB_H 1

/* Define to 1 if you have the `_getvideoconfig' function. */
/* #undef H5_HAVE__GETVIDEOCONFIG */

/* Define to 1 if you have the `_scrsize' function. */
/* #undef H5_HAVE__SCRSIZE */

/* Define if `__tm_gmtoff' is a member of `struct tm' */
/* #undef H5_HAVE___TM_GMTOFF */

/* Define if your system can't handle converting floating-point values to long
   long. */
/* #undef H5_HW_FP_TO_LLONG_NOT_WORKS */

/* Define if HDF5's high-level library headers should be included in hdf5.h */
#define H5_INCLUDE_HL 1

/* Define if your system can accurately convert from integers to long double
   values. */
#define H5_INTEGER_TO_LDOUBLE_ACCURATE 1

/* Define if your system can convert long double to integers accurately. */
#define H5_LDOUBLE_TO_INTEGER_ACCURATE 1

/* Define if your system can convert from long double to integer values. */
#define H5_LDOUBLE_TO_INTEGER_WORKS 1

/* Define if your system can convert long double to (unsigned) long long
   values correctly. */
#define H5_LDOUBLE_TO_LLONG_ACCURATE 1

/* Define if your system converts long double to (unsigned) long values with
   special algorithm. */
/* #undef H5_LDOUBLE_TO_LONG_SPECIAL */

/* Define if your system can convert long double to unsigned int values
   correctly. */
#define H5_LDOUBLE_TO_UINT_ACCURATE 1

/* Define if your system can compile long long to floating-point casts. */
#define H5_LLONG_TO_FP_CAST_WORKS 1

/* Define if your system can convert (unsigned) long long to long double
   values correctly. */
#define H5_LLONG_TO_LDOUBLE_CORRECT 1

/* Define if your system can convert (unsigned) long to long double values
   with special algorithm. */
/* #undef H5_LONG_TO_LDOUBLE_SPECIAL */

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */
#define H5_LT_OBJDIR ".libs/"

/* Define if the metadata trace file code is to be compiled in */
/* #undef H5_METADATA_TRACE_FILE */

/* Define if your system's `MPI_File_set_size' function works for files over
   2GB. */
/* #undef H5_MPI_FILE_SET_SIZE_BIG */

/* Define if we can violate pointer alignment restrictions */
#define H5_NO_ALIGNMENT_RESTRICTIONS 1

/* Define if deprecated public API symbols are disabled */
/* #undef H5_NO_DEPRECATED_SYMBOLS */

/* Define if shared writing must be disabled (CodeWarrior only) */
#define H5_NO_SHARED_WRITING 1

/* Name of package */
#define H5_PACKAGE "hdf5"

/* Define to the address where bug reports for this package should be sent. */
#define H5_PACKAGE_BUGREPORT "help@hdfgroup.org"

/* Define to the full name of this package. */
#define H5_PACKAGE_NAME "HDF5"

/* Define to the full name and version of this package. */
#define H5_PACKAGE_STRING "HDF5 1.9.170-swmr_chksum5"

/* Define to the one symbol short name of this package. */
#define H5_PACKAGE_TARNAME "hdf5"

/* Define to the home page for this package. */
#define H5_PACKAGE_URL ""

/* Define to the version of this package. */
#define H5_PACKAGE_VERSION "1.9.170-swmr_chksum5"

/* Width for printf() for type `long long' or `__int64', use `ll' */
#define H5_PRINTF_LL_WIDTH "ll"

/* The size of `char', as computed by sizeof. */
#define H5_SIZEOF_CHAR 1

/* The size of `double', as computed by sizeof. */
#define H5_SIZEOF_DOUBLE 8

/* The size of `float', as computed by sizeof. */
#define H5_SIZEOF_FLOAT 4

/* The size of `int', as computed by sizeof. */
#define H5_SIZEOF_INT 4

/* The size of `int16_t', as computed by sizeof. */
#define H5_SIZEOF_INT16_T 2

/* The size of `int32_t', as computed by sizeof. */
#define H5_SIZEOF_INT32_T 4

/* The size of `int64_t', as computed by sizeof. */
#define H5_SIZEOF_INT64_T 8

/* The size of `int8_t', as computed by sizeof. */
#define H5_SIZEOF_INT8_T 1

/* The size of `int_fast16_t', as computed by sizeof. */
/* #define H5_SIZEOF_INT_FAST16_T 0 */

/* The size of `int_fast32_t', as computed by sizeof. */
/* #define H5_SIZEOF_INT_FAST32_T 0 */

/* The size of `int_fast64_t', as computed by sizeof. */
/* #define H5_SIZEOF_INT_FAST64_T 0 */

/* The size of `int_fast8_t', as computed by sizeof. */
/* #define H5_SIZEOF_INT_FAST8_T 0 */

/* The size of `int_least16_t', as computed by sizeof. */
/* #define H5_SIZEOF_INT_LEAST16_T 0 */

/* The size of `int_least32_t', as computed by sizeof. */
/* #define H5_SIZEOF_INT_LEAST32_T 0 */

/* The size of `int_least64_t', as computed by sizeof. */
/* #define H5_SIZEOF_INT_LEAST64_T 0 */

/* The size of `int_least8_t', as computed by sizeof. */
/* #define H5_SIZEOF_INT_LEAST8_T 0 */

/* The size of `long', as computed by sizeof. */
#define H5_SIZEOF_LONG 4

/* The size of `long double', as computed by sizeof. */
#define H5_SIZEOF_LONG_DOUBLE 16

/* The size of `long long', as computed by sizeof. */
#define H5_SIZEOF_LONG_LONG 8

/* The size of `off64_t', as computed by sizeof. */
#define H5_SIZEOF_OFF64_T 0

/* The size of `off_t', as computed by sizeof. */
#define H5_SIZEOF_OFF_T 8

/* The size of `ptrdiff_t', as computed by sizeof. */
#define H5_SIZEOF_PTRDIFF_T 4

/* The size of `short', as computed by sizeof. */
#define H5_SIZEOF_SHORT 2

/* The size of `size_t', as computed by sizeof. */
#define H5_SIZEOF_SIZE_T 4

/* The size of `ssize_t', as computed by sizeof. */
#define H5_SIZEOF_SSIZE_T 4

/* The size of `uint16_t', as computed by sizeof. */
#define H5_SIZEOF_UINT16_T 2

/* The size of `uint32_t', as computed by sizeof. */
#define H5_SIZEOF_UINT32_T 4

/* The size of `uint64_t', as computed by sizeof. */
#define H5_SIZEOF_UINT64_T 8

/* The size of `uint8_t', as computed by sizeof. */
#define H5_SIZEOF_UINT8_T 1

/* The size of `uint_fast16_t', as computed by sizeof. */
/* #define H5_SIZEOF_UINT_FAST16_T 0 */

/* The size of `uint_fast32_t', as computed by sizeof. */
/* #define H5_SIZEOF_UINT_FAST32_T 0 */

/* The size of `uint_fast64_t', as computed by sizeof. */
/* #define H5_SIZEOF_UINT_FAST64_T 0 */

/* The size of `uint_fast8_t', as computed by sizeof. */
/* #define H5_SIZEOF_UINT_FAST8_T 0 */

/* The size of `uint_least16_t', as computed by sizeof. */
/* #define H5_SIZEOF_UINT_LEAST16_T 0 */

/* The size of `uint_least32_t', as computed by sizeof. */
/* #define H5_SIZEOF_UINT_LEAST32_T 0 */

/* The size of `uint_least64_t', as computed by sizeof. */
/* #define H5_SIZEOF_UINT_LEAST64_T 0 */

/* The size of `uint_least8_t', as computed by sizeof. */
/* #define H5_SIZEOF_UINT_LEAST8_T 0 */

/* The size of `unsigned', as computed by sizeof. */
#define H5_SIZEOF_UNSIGNED 4

/* The size of `__int64', as computed by sizeof. */
#define H5_SIZEOF___INT64 0

/* Define to 1 if you have the ANSI C header files. */
#define H5_STDC_HEADERS 1

/* Define if strict file format checks are enabled */
/* #undef H5_STRICT_FORMAT_CHECKS */

/* Define if your system supports pthread_attr_setscope(&attribute,
   PTHREAD_SCOPE_SYSTEM) call. */
#define H5_SYSTEM_SCOPE_THREADS 1

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
#define H5_TIME_WITH_SYS_TIME 1

/* Define to 1 if your <sys/time.h> declares `struct tm'. */
/* #undef H5_TM_IN_SYS_TIME */

/* Define if your system can compile unsigned long long to floating-point
   casts. */
#define H5_ULLONG_TO_FP_CAST_WORKS 1

/* Define if your system can convert unsigned long long to long double with
   correct precision. */
#define H5_ULLONG_TO_LDOUBLE_PRECISION 1

/* Define if your system accurately converting unsigned long to float values.
   */
/* #undef H5_ULONG_TO_FLOAT_ACCURATE */

/* Define if your system can accurately convert unsigned (long) long values to
   floating-point values. */
#define H5_ULONG_TO_FP_BOTTOM_BIT_ACCURATE 1

/* Define using v1.6 public API symbols by default */
/* #define H5_USE_16_API_DEFAULT 1 */

/* Define if a memory checking tool will be used on the library, to cause
   library to be very picky about memory operations and also disable the
   internal free list manager code. */
/* #undef H5_USING_MEMCHECKER */

/* Version number of package */
#define H5_VERSION "1.9.170-swmr_chksum5"

/* Define if vsnprintf() returns the correct value for formatted strings that
   don't fit into size allowed */
#define H5_VSNPRINTF_WORKS 1

/* Data accuracy is prefered to speed during data conversions */
#define H5_WANT_DATA_ACCURACY 1

/* Check exception handling functions during data conversions */
#define H5_WANT_DCONV_EXCEPTION 1

/* Define to 1 if your processor stores words with the most significant byte
   first (like Motorola and SPARC, unlike Intel and VAX). */
/* #undef H5_WORDS_BIGENDIAN */

/* Number of bits in a file offset, on hosts where this is settable. */
/* #undef H5__FILE_OFFSET_BITS */

/* Define for large files, on AIX-style hosts. */
/* #undef H5__LARGE_FILES */

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef H5_const */

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
/* #undef H5_inline */
#endif

/* Define to `long int' if <sys/types.h> does not define. */
/* #undef H5_off_t */

/* Define to `long' if <sys/types.h> does not define. */
/* #undef H5_ptrdiff_t */

/* Define to `unsigned long' if <sys/types.h> does not define. */
/* #undef H5_size_t */

/* Define to `long' if <sys/types.h> does not define. */
/* #undef H5_ssize_t */
#define H5_HAVE_FILE_VERSIONS 1
#define H5_CANNOT_OPEN_TWICE  1
