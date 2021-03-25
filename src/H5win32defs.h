/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Programmer:  Scott Wegner
 *              June 3, 2008
 *
 * Purpose: This file is used to map HDF macros to Windows functions.  This
 *          should get included H5private mappings, so as to override them.
 *          Any macro not mapped here, however, will receive a similar mapping
 *          inside H5private.h
 *
 */
#ifndef H5_HAVE_INTTYPES_H
/* The following definitions should be suitable for 64-bit Windows, which is
 * LLP64, and for 32-bit Windows, which is ILP32.  Those are the only
 * platforms where <inttypes.h> is likely to be missing.  VS2015 and later
 * *may* provide these definitions.
 */
#ifdef _WIN64
#define PRIdPTR "lld"
#define PRIoPTR "llo"
#define PRIuPTR "llu"
#define PRIxPTR "llx"
#else /* _WIN64 */
#define PRIdPTR "ld"
#define PRIoPTR "lo"
#define PRIuPTR "lu"
#define PRIxPTR "lx"
#endif /* _WIN64 */

#define PRId8   "d"
#define PRIo8   "o"
#define PRIu8   "u"
#define PRIx8   "x"
#define PRId16  "d"
#define PRIo16  "o"
#define PRIu16  "u"
#define PRIx16  "x"
#define PRId32  "d"
#define PRIo32  "o"
#define PRIu32  "u"
#define PRIx32  "x"
#define PRId64  "lld"
#define PRIo64  "llo"
#define PRIu64  "llu"
#define PRIx64  "llx"
#define PRIdMAX "lld"
#define PRIoMAX "llo"
#define PRIuMAX "llu"
#define PRIxMAX "llx"
#endif

/*
 * _MSC_VER = 1900 VS2015
 * _MSC_VER = 1800 VS2013
 * _MSC_VER = 1700 VS2012
 */
#ifdef H5_HAVE_WIN32_API

typedef struct _stati64 h5_stat_t;
typedef __int64         h5_stat_size_t;

#define HDaccess(F, M) _access(F, M)
#define HDchdir(S)     _chdir(S)
#define HDclose(F)     _close(F)
#define HDcreat(S, M)  Wopen_utf8(S, O_CREAT | O_TRUNC | O_RDWR, M)
#define HDdup(F)       _dup(F)
#define HDfdopen(N, S) _fdopen(N, S)
#define HDfileno(F)    _fileno(F)
#define HDfstat(F, B)  _fstati64(F, B)
#define HDisatty(F)    _isatty(F)

#define HDgetcwd(S, Z)     _getcwd(S, Z)
#define HDgetdcwd(D, S, Z) _getdcwd(D, S, Z)
#define HDgetdrive()       _getdrive()
#define HDlseek(F, O, W)   _lseeki64(F, O, W)
#define HDlstat(S, B)      _lstati64(S, B)
#define HDmkdir(S, M)      _mkdir(S)
#define HDoff_t            __int64

/* Note that the variadic HDopen macro is using a VC++ extension
 * where the comma is dropped if nothing is passed to the ellipsis.
 */
#ifndef H5_HAVE_MINGW
#define HDopen(S, F, ...) Wopen_utf8(S, F, __VA_ARGS__)
#else
#define HDopen(S, F, ...) Wopen_utf8(S, F, ##__VA_ARGS__)
#endif
#define HDread(F, M, Z)       _read(F, M, Z)
#define HDremove(S)           Wremove_utf8(S)
#define HDrmdir(S)            _rmdir(S)
#define HDsetvbuf(F, S, M, Z) setvbuf(F, S, M, (Z > 1 ? Z : 2))
#define HDsleep(S)            Sleep(S * 1000)
#define HDstat(S, B)          _stati64(S, B)
#define HDstrcasecmp(A, B)    _stricmp(A, B)
#define HDstrdup(S)           _strdup(S)
#define HDstrtok_r(X, Y, Z)   strtok_s(X, Y, Z)
#define HDtzset()             _tzset()
#define HDunlink(S)           _unlink(S)
#define HDwrite(F, M, Z)      _write(F, M, Z)

#ifdef H5_HAVE_VISUAL_STUDIO

/*
 * The (void*) cast just avoids a compiler warning in H5_HAVE_VISUAL_STUDIO
 */
#define HDmemset(X, C, Z) memset((void *)(X), C, Z)

struct timezone {
    int tz_minuteswest;
    int tz_dsttime;
};

#endif /* H5_HAVE_VISUAL_STUDIO */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */
H5_DLL int    Wgettimeofday(struct timeval *tv, struct timezone *tz);
H5_DLL int    Wsetenv(const char *name, const char *value, int overwrite);
H5_DLL int    Wflock(int fd, int operation);
H5_DLL char * Wgetlogin(void);
H5_DLL herr_t H5_expand_windows_env_vars(char **env_var);
H5_DLL const wchar_t *H5_get_utf16_str(const char *s);
H5_DLL int            Wopen_utf8(const char *path, int oflag, ...);
H5_DLL int            Wremove_utf8(const char *path);
H5_DLL int            H5_get_win32_times(H5_timevals_t *tvs);
#ifdef __cplusplus
}
#endif /* __cplusplus */

#define HDgettimeofday(V, Z) Wgettimeofday(V, Z)
#define HDsetenv(N, V, O)    Wsetenv(N, V, O)
#define HDflock(F, L)        Wflock(F, L)
#define HDgetlogin()         Wgetlogin()

/* Non-POSIX functions */

#ifndef H5_HAVE_MINGW
#define HDftruncate(F, L) _chsize_s(F, L)
#define HDfseek(F, O, W)  _fseeki64(F, O, W)
#endif /* H5_HAVE_MINGW */

#endif /* H5_HAVE_WIN32_API */
