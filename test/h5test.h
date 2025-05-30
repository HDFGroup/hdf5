/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose:     Test support stuff.
 */
#ifndef H5TEST_H
#define H5TEST_H

/*
 * Include required headers.  This file tests internal library functions,
 * so we include the private headers here.
 */
#include "hdf5.h"
#include "H5private.h"
#include "H5Eprivate.h"

/*
 * This contains the filename prefix specified as command line option for
 * the parallel test files.
 */
H5TEST_DLLVAR char *paraprefix;
#ifdef H5_HAVE_PARALLEL
H5TEST_DLLVAR MPI_Info h5_io_info_g; /* MPI INFO object for IO */
#endif

/*
 * Print the current location on the standard output stream.
 */
#define AT()                                                                                                 \
    do {                                                                                                     \
        printf("   at %s:%d in %s()...\n", __FILE__, __LINE__, __func__);                                    \
    } while (0)

/*
 * The name of the test is printed by saying TESTING("something") which will
 * result in the string `Testing something' being flushed to standard output.
 * If a test passes, fails, or is skipped then the PASSED(), H5_FAILED(), or
 * SKIPPED() macro should be called.  After H5_FAILED() or SKIPPED() the caller
 * should print additional information to stdout indented by at least four
 * spaces.  If the h5_errors() is used for automatic error handling then
 * the H5_FAILED() macro is invoked automatically when an API function fails.
 */
#define TESTING(WHAT)                                                                                        \
    do {                                                                                                     \
        printf("Testing %-62s", WHAT);                                                                       \
        fflush(stdout);                                                                                      \
        n_tests_run_g++;                                                                                     \
    } while (0)
#define TESTING_2(WHAT)                                                                                      \
    do {                                                                                                     \
        printf("  Testing %-60s", WHAT);                                                                     \
        fflush(stdout);                                                                                      \
        n_tests_run_g++;                                                                                     \
    } while (0)
#define PASSED()                                                                                             \
    do {                                                                                                     \
        puts(" PASSED");                                                                                     \
        fflush(stdout);                                                                                      \
        n_tests_passed_g++;                                                                                  \
    } while (0)
#define H5_FAILED()                                                                                          \
    do {                                                                                                     \
        puts("*FAILED*");                                                                                    \
        fflush(stdout);                                                                                      \
        n_tests_failed_g++;                                                                                  \
    } while (0)
#define H5_WARNING()                                                                                         \
    do {                                                                                                     \
        puts("*WARNING*");                                                                                   \
        fflush(stdout);                                                                                      \
    } while (0)
#define SKIPPED()                                                                                            \
    do {                                                                                                     \
        puts(" -SKIP-");                                                                                     \
        fflush(stdout);                                                                                      \
        n_tests_skipped_g++;                                                                                 \
    } while (0)
#define PUTS_ERROR(s)                                                                                        \
    do {                                                                                                     \
        puts(s);                                                                                             \
        AT();                                                                                                \
        goto error;                                                                                          \
    } while (0)
#define TEST_ERROR                                                                                           \
    do {                                                                                                     \
        H5_FAILED();                                                                                         \
        AT();                                                                                                \
        goto error;                                                                                          \
    } while (0)
#define STACK_ERROR                                                                                          \
    do {                                                                                                     \
        H5Eprint2(H5E_DEFAULT, stdout);                                                                      \
        goto error;                                                                                          \
    } while (0)
#define FAIL_STACK_ERROR                                                                                     \
    do {                                                                                                     \
        H5_FAILED();                                                                                         \
        AT();                                                                                                \
        H5Eprint2(H5E_DEFAULT, stdout);                                                                      \
        goto error;                                                                                          \
    } while (0)
#define FAIL_PUTS_ERROR(s)                                                                                   \
    do {                                                                                                     \
        H5_FAILED();                                                                                         \
        AT();                                                                                                \
        puts(s);                                                                                             \
        goto error;                                                                                          \
    } while (0)

/*
 * Testing macros used for multi-part tests.
 */
#define TESTING_MULTIPART(WHAT)                                                                              \
    do {                                                                                                     \
        printf("Testing %-62s", WHAT);                                                                       \
        puts("");                                                                                            \
        fflush(stdout);                                                                                      \
    } while (0)

/*
 * Begin and end an entire section of multi-part tests. By placing all the
 * parts of a test between these macros, skipping to the 'error' cleanup
 * section of a test is deferred until all parts have finished.
 */
#define BEGIN_MULTIPART                                                                                      \
    {                                                                                                        \
        int part_nerrors = 0;

#define END_MULTIPART                                                                                        \
    if (part_nerrors > 0)                                                                                    \
        goto error;                                                                                          \
    }

/*
 * Begin, end and handle errors within a single part of a multi-part test.
 * The PART_END macro creates a goto label based on the given "part name".
 * When a failure occurs in the current part, the PART_ERROR macro uses
 * this label to skip to the next part of the multi-part test. The PART_ERROR
 * macro also increments the error count so that the END_MULTIPART macro
 * knows to skip to the test's 'error' label once all test parts have finished.
 */
#define PART_BEGIN(part_name) {
#define PART_END(part_name)                                                                                  \
    }                                                                                                        \
    part_##part_name##_end:
#define PART_ERROR(part_name)                                                                                \
    do {                                                                                                     \
        n_tests_failed_g++;                                                                                  \
        part_nerrors++;                                                                                      \
        goto part_##part_name##_end;                                                                         \
    } while (0)
#define PART_TEST_ERROR(part_name)                                                                           \
    do {                                                                                                     \
        H5_FAILED();                                                                                         \
        AT();                                                                                                \
        part_nerrors++;                                                                                      \
        goto part_##part_name##_end;                                                                         \
    } while (0)

/*
 * Simply skips to the goto label for this test part and moves on to the
 * next test part. Useful for when a test part needs to be skipped for
 * some reason or is currently unimplemented and empty.
 */
#define PART_EMPTY(part_name)                                                                                \
    do {                                                                                                     \
        goto part_##part_name##_end;                                                                         \
    } while (0)

/* Flags for h5_fileaccess_flags() */
#define H5_FILEACCESS_VFD    0x01
#define H5_FILEACCESS_LIBVER 0x02

/* Flags for h5_driver_uses_multiple_files() */
#define H5_EXCLUDE_MULTIPART_DRIVERS     0x01
#define H5_EXCLUDE_NON_MULTIPART_DRIVERS 0x02

/* Fill an array on the heap with an increasing count value.  BUF
 * is expected to point to a `struct { TYPE arr[...][...]; }`.
 */
#define H5TEST_FILL_2D_HEAP_ARRAY(BUF, TYPE)                                                                 \
    do {                                                                                                     \
        /* Prefix with h5tfa to avoid shadow warnings */                                                     \
        size_t h5tfa_i     = 0;                                                                              \
        size_t h5tfa_j     = 0;                                                                              \
        TYPE   h5tfa_count = 0;                                                                              \
                                                                                                             \
        for (h5tfa_i = 0; h5tfa_i < NELMTS((BUF)->arr); h5tfa_i++)                                           \
            for (h5tfa_j = 0; h5tfa_j < NELMTS((BUF)->arr[0]); h5tfa_j++) {                                  \
                (BUF)->arr[h5tfa_i][h5tfa_j] = h5tfa_count;                                                  \
                h5tfa_count++;                                                                               \
            }                                                                                                \
    } while (0)

/* Macros for the different TestExpress levels for expediting tests */
#define H5_TEST_EXPRESS_EXHAUSTIVE 0 /** Exhaustive run; tests should take as long as necessary */
#define H5_TEST_EXPRESS_FULL       1 /** Full run; tests should take no more than 20 minutes    */
#define H5_TEST_EXPRESS_QUICK      2 /** Quick run; tests should take no more than 10 minutes   */
#define H5_TEST_EXPRESS_SMOKE_TEST 3 /** Smoke test; tests should take no more than 1 minute    */

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Ugly hack to cast away const for freeing const-qualified pointers.
 * Should only be used sparingly, where the alternative (like keeping
 * an equivalent non-const pointer around) is far messier.
 */
#ifndef h5_free_const
#define h5_free_const(mem) free((void *)(uintptr_t)mem)
#endif

/* Extern global variables */
H5TEST_DLLVAR size_t   n_tests_run_g;
H5TEST_DLLVAR size_t   n_tests_passed_g;
H5TEST_DLLVAR size_t   n_tests_failed_g;
H5TEST_DLLVAR size_t   n_tests_skipped_g;
H5TEST_DLLVAR uint64_t vol_cap_flags_g;

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Performs test framework initialization
 *
 * \return nothing
 *
 * \details h5_test_init() performs test initialization actions, such as
 *          setting the TestExpress level setting, and should be called
 *          toward the beginning of the main() function in a test program.
 *
 */
H5TEST_DLL void h5_test_init(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Restores HDF5's default error handling function
 *
 * \return nothing
 *
 * \details h5_restore_err() restores HDF5's default error handling function
 *          after its temporary replacement by h5_test_init(), which sets the
 *          error handling function to its own error handling callback
 *          function instead of the default function.
 *
 * \see h5_test_init()
 *
 */
H5TEST_DLL void h5_restore_err(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Returns the current TestExpress level setting
 *
 * \return Returns the current TestExpress level setting
 *
 * \details h5_get_testexpress() returns the current TestExpress level
 *          setting, which determines whether a test program should expedite
 *          testing by skipping some tests. If the TestExpress level has not
 *          yet been set, it will be initialized to the default value
 *          (currently level 1, unless overridden at configuration time). The
 *          different TestExpress level settings have the following meanings:
 *
 *          + 0 / #H5_TEST_EXPRESS_EXHAUSTIVE - Tests should take as long as
 *                                              necessary
 *          + 1 / #H5_TEST_EXPRESS_FULL       - Tests should take no more
 *                                              than 30 minutes
 *          + 2 / #H5_TEST_EXPRESS_QUICK      - Tests should take no more
 *                                              than 10 minutes
 *          + 3 / #H5_TEST_EXPRESS_SMOKE_TEST - Tests should take no more
 *                                              than 1 minute
 *
 *          If the TestExpress level setting is not yet initialized, this
 *          function will first set a local variable to the value of the
 *          #H5_TEST_EXPRESS_LEVEL_DEFAULT macro, if it has been defined. If
 *          the environment variable "HDF5TestExpress" is defined, its value
 *          will override the local variable's value. Acceptable values for
 *          the environment variable are the strings "0", "1" and "2"; any
 *          other string will cause the variable to be set to the value
 *          3 / H5_TEST_EXPRESS_SMOKE_TEST. Once the value for the local
 *          variable has been determined, h5_get_testexpress() returns that
 *          value.
 *
 *          The limitation imposed by the TestExpress functionality applies
 *          to the total runtime of a test executable, even if it contains
 *          multiple sub-tests.
 *
 *          The standard system for test times is a Linux machine running in
 *          NFS space (to catch tests that involve a great deal of disk I/O).
 *
 * \see h5_set_testexpress()
 *
 */
H5TEST_DLL int h5_get_testexpress(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Sets the current TestExpress level setting
 *
 * \param[in]  new_val The new level to set the TestExpress setting to
 *
 * \return nothing
 *
 * \details h5_set_testexpress() sets the current TestExpress level setting
 *          to the value specified in \p new_val. If \p new_val is negative,
 *          the TestExpress level is set to the default value (currently
 *          level 1, unless overridden at configuration time). If \p new_val
 *          is greater than the highest TestExpress level (3), it is set to
 *          the highest TestExpress level.
 *
 * \see h5_get_testexpress()
 *
 */
H5TEST_DLL void h5_set_testexpress(int new_val);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Creates a File Access Property List for use in tests
 *
 * \return \hid_t{file access property list}
 *
 * \details h5_fileaccess() is a wrapper function around h5_get_vfd_fapl()
 *          and h5_get_libver_fapl() which creates and returns a File Access
 *          Property List that has potentially been configured with a
 *          non-default file driver and library version bounds setting.
 *          Calling h5_fileaccess() is the primary way that tests should
 *          obtain a File Access Property List to use, unless the usage of
 *          non-default VFDs would be problematic for that test.
 *
 * \see h5_fileaccess_flags(), h5_get_vfd_fapl(), h5_get_libver_fapl()
 *
 */
H5TEST_DLL hid_t h5_fileaccess(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Creates a File Access Property List for use in tests
 *
 * \param[in]  flags Flags controlling which parts of the created File Access
 *                   Property List will be modified
 *
 * \return \hid_t{file access property list}
 *
 * \details h5_fileaccess_flags() is a counterpart to h5_fileaccess() which
 *          allows the caller to specify which parts of the File Access
 *          Property List should be modified after it is created. \p flags
 *          may be specified as one of the following macro values:
 *
 *          #H5_FILEACCESS_VFD - specifies that the file driver of the FAPL
 *                               should be modified
 *          #H5_FILEACCESS_LIBVER - specifies that the library version bounds
 *                                  setting of the FAPL should be modified
 *
 *          \p flags can also be specified as a bit-wise OR of these values,
 *          in which case behavior is equivalent to h5_fileaccess().
 *
 * \see h5_fileaccess(), h5_get_vfd_fapl(), h5_get_libver_fapl()
 *
 */
H5TEST_DLL hid_t h5_fileaccess_flags(unsigned flags);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Modifies the Virtual File Driver set on a File Access Property List
 *
 * \param[in]  fapl_id File Access Property List to modify
 *
 * \return \herr_t
 *
 * \details h5_get_vfd_fapl() modifies the File Access Property List
 *          specified in \p fapl_id by setting a new Virtual File Driver on
 *          it with default configuration values. The Virtual File Driver to
 *          be used is chosen according to the value set for either the
 *          HDF5_DRIVER or HDF5_TEST_DRIVER environment variable, with
 *          preference given to HDF5_DRIVER. These environment variables may
 *          be set to one of the following strings:
 *
 *          + "sec2" - Default sec2 driver; no extra configuration supplied
 *          + "stdio" - C STDIO driver; no configuration supplied
 *          + "core" - In-memory driver; 1MiB increment size and backing
 *                     store enabled
 *          + "core_paged" - In-memory driver; 1MiB increment size and
 *                           backing store enabled; write tracking enabled
 *                           and 4KiB page size
 *          + "split" - Multi driver with the file's metadata being placed
 *                      into a file with a ".meta" suffix and the file's raw
 *                      data being placed into a file with a ".raw" suffix
 *          + "multi" - Multi driver with the file data being placed into
 *                      several different files with the suffixes "m.h5"
 *                      (metadata), "s.h5" (superblock, userblock and driver
 *                      info data), "b.h5" (b-tree data), "r.h5" (dataset raw
 *                      data), "g.h5" (global heap data), "l.h5" (local heap
 *                      data) and "o.h5" (object header data)
 *          + "family" - Family driver with a family file size of 100MiB and
 *                       with a default File Access Property List used for
 *                       accessing the family file members. A different
 *                       family file size can be specified in the environment
 *                       variable as an integer value of bytes separated from
 *                       the string "family" with whitespace, e.g.
 *                       "family 52428800" would specify a family file size
 *                       of 50MiB.
 *          + "log" - Log driver using stderr for output, with the
 *                    #H5FD_LOG_LOC_IO and #H5FD_LOG_ALLOC flags set and 0
 *                    for the buffer size. A different set of flags may be
 *                    specified for the driver as an integer value
 *                    (corresponding to a bit-wise OR of flags) separated
 *                    from the string "log" with whitespace, e.g. "log 14"
 *                    would equate to the flag #H5FD_LOG_LOC_IO}.
 *          + "direct" - Direct driver with a 4KiB block size, a 32KiB copy
 *                       buffer size and a 1KiB memory alignment setting. If
 *                       the direct driver is not enabled when HDF5 is built,
 *                       h5_get_vfd_fapl() will return an error.
 *          + "splitter" - Splitter driver using the default (sec2) VFD for
 *                         both the read/write and write-only channels, an
 *                         empty log file path and set to not ignore errors
 *                         on the write-only channel
 *          + "onion" - support not currently implemented; will cause
 *                      h5_get_vfd_fapl() to return an error.
 *          + "subfiling" - Subfiling driver with a default configuration of
 *                          1 I/O concentrator per node, a 32MiB stripe size
 *                          and using the IOC driver with 4 worker threads.
 *                          MPI_COMM_WORLD and MPI_INFO_NULL are used for the
 *                          MPI parameters.
 *          + "mpio" - MPI I/O driver with MPI_COMM_WORLD and MPI_INFO_NULL
 *                     used for the MPI parameters.
 *          + "mirror" - support not currently implemented; will cause
 *                       h5_get_vfd_fapl() to return an error.
 *          + "hdfs" - support not currently implemented; will cause
 *                     h5_get_vfd_fapl() to return an error.
 *          + "ros3" - support not currently implemented; will cause
 *                     h5_get_vfd_fapl() to return an error.
 *
 *          Other values for the environment variables will cause
 *          h5_get_vfd_fapl() to return an error.
 *
 */
H5TEST_DLL herr_t h5_get_vfd_fapl(hid_t fapl_id);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Modifies the Library Version bounds set on a File Access Property
 *        List
 *
 * \param[in]  fapl_id File Access Property List to modify
 *
 * \return \herr_t
 *
 * \details h5_get_libver_fapl() modifies the File Access Property List
 *          specified in \p fapl_id by setting library version bound values
 *          on it according to the value set for the HDF5_LIBVER_BOUNDS
 *          environment variable. Currently, the only valid value for this
 *          environment variable is the string "latest", which will cause
 *          this function to set the low and high version bounds both to
 *          #H5F_LIBVER_LATEST. Other values for the environment variable
 *          will cause this function to fail and return a negative value.
 *
 */
H5TEST_DLL herr_t h5_get_libver_fapl(hid_t fapl_id);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Cleans up temporary testing files
 *
 * \param[in]  base_name Array of filenames, without suffixes, to clean up
 * \param[in]  fapl File Access Property List to use when deleting files
 *
 * \return Non-zero if cleanup actions were performed; zero otherwise.
 *
 * \details h5_cleanup() is used to clean up temporary testing files created
 *          by a test program. \p base_name is an array of filenames, without
 *          suffixes, which should be cleaned up. The last entry in
 *          \p base_name must be NULL. For each filename specified in
 *          \p base_name, h5_cleanup() will generate a VFD-dependent filename
 *          with h5_fixname() according to the given File Access Property List
 *          \p fapl, then call H5Fdelete() on the resulting filename. \p fapl
 *          will be closed after all files are deleted. If the environment
 *          variable HDF5_NOCLEANUP has been defined, this function will have
 *          no effect and \p fapl will be left open.
 *
 * \see h5_fixname()
 *
 */
H5TEST_DLL int h5_cleanup(const char *base_name[], hid_t fapl);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Cleans up temporary testing files
 *
 * \param[in]  base_name Array of filenames, without suffixes, to clean up
 * \param[in]  fapl File Access Property List to use when deleting files
 *
 * \return nothing
 *
 * \details h5_delete_all_test_files() is used to clean up temporary testing
 *          files created by a test program. \p base_name is an array of
 *          filenames, without suffixes, which should be cleaned up. The last
 *          entry in \p base_name must be NULL. For each filename specified in
 *          \p base_name, this function will generate a VFD-dependent filename
 *          with h5_fixname() according to the given File Access Property List
 *          \p fapl, then call H5Fdelete() on the resulting filename. \p fapl
 *          will <b>NOT</b> be closed after all files are deleted and must be
 *          closed by the caller. h5_delete_all_test_files() always performs
 *          file cleanup, regardless of if the HDF5_NOCLEANUP environment
 *          variable has been defined.
 *
 * \see h5_fixname()
 *
 */
H5TEST_DLL void h5_delete_all_test_files(const char *base_name[], hid_t fapl);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Cleans up a single temporary testing file
 *
 * \param[in]  base_name Filename, without a suffix, to clean up
 * \param[in]  fapl File Access Property List to use when deleting the file
 *
 * \return nothing
 *
 * \details h5_delete_test_file() is used to clean up a single temporary
 *          testing file created by a test program. \p base_name is a
 *          filename, without a suffix, which should be cleaned up. This
 *          function will generate a VFD-dependent filename with h5_fixname()
 *          according to the given File Access Property List \p fapl, then
 *          call H5Fdelete() on the resulting filename. \p fapl will
 *          <b>NOT</b> be closed after the file is deleted and must be
 *          closed by the caller. h5_delete_test_file() always performs
 *          file cleanup, regardless of if the HDF5_NOCLEANUP environment
 *          variable has been defined.
 *
 * \see h5_fixname()
 *
 */
H5TEST_DLL void h5_delete_test_file(const char *base_name, hid_t fapl);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Creates a VFD-dependent filename from a base filename without a
 *        suffix and a File Access Property List
 *
 * \param[in]  base_name Filename, without a suffix, to use as a base when
 *                       creating the resulting filename
 * \param[in]  fapl File Access Property List to use when creating the
 *                  resulting filename
 * \param[out]  fullname Buffer to store the resulting filename in
 * \param[in]  size Size in bytes of the \p fullname output buffer
 *
 * \return A pointer to the resulting filename on success; NULL otherwise
 *
 * \details Given a base filename without a suffix, \p base_name, and a File
 *          Access Property List, \p fapl, h5_fixname() generates a filename
 *          according to the configuration set on \p fapl. The resulting
 *          filename is copied to \p fullname, which is \p size bytes in
 *          size, including space for the NUL terminator.
 *
 *          h5_fixname() is the primary way that tests should create
 *          filenames, as it accounts for the possibility of a test being run
 *          with a non-default Virtual File Driver that may require a
 *          specialized filename (e.g., the family driver). It also allows
 *          tests to easily output test files to a different directory by
 *          setting the HDF5_PREFIX (for serial tests) or HDF5_PARAPREFIX
 *          (for parallel tests) environment variable. When one of these
 *          environment variables is set, the contents of the variable are
 *          prepended to the resulting filename and separated from the
 *          base filename with a slash.
 *
 */
H5TEST_DLL char *h5_fixname(const char *base_name, hid_t fapl, char *fullname, size_t size);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Creates a VFD-dependent filename for a superblock file from a base
 *        filename without a suffix and a File Access Property List
 *
 * \param[in]  base_name Filename, without a suffix, to use as a base when
 *                       creating the resulting filename
 * \param[in]  fapl File Access Property List to use when creating the
 *                  resulting filename
 * \param[out]  fullname Buffer to store the resulting filename in
 * \param[in]  size Size in bytes of the \p fullname output buffer
 *
 * \return A pointer to the resulting filename on success; NULL otherwise
 *
 * \details Given a base filename without a suffix, \p base_name, and a File
 *          Access Property List, \p fapl, h5_fixname_superblock() generates
 *          a filename according to the configuration set on \p fapl. The
 *          resulting filename is copied to \p fullname, which is \p size
 *          bytes in size, including space for the NUL terminator.
 *
 *          h5_fixname_superblock() is similar to h5_fixname(), but it
 *          generates the filename that would need to be opened to find the
 *          logical HDF5 file's superblock. It is useful for when a file is
 *          to be opened with open(2) but the h5_fixname() string contains
 *          stuff like format strings.
 *
 * \see h5_fixname()
 *
 */
H5TEST_DLL char *h5_fixname_superblock(const char *base_name, hid_t fapl, char *fullname, size_t size);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Creates a VFD-dependent filename without a suffix from a base
 *        filename without a suffix and a File Access Property List
 *
 * \param[in]  base_name Filename, without a suffix, to use as a base when
 *                       creating the resulting filename
 * \param[in]  fapl File Access Property List to use when creating the
 *                  resulting filename
 * \param[out]  fullname Buffer to store the resulting filename in
 * \param[in]  size Size in bytes of the \p fullname output buffer
 *
 * \return A pointer to the resulting filename on success; NULL otherwise
 *
 * \details Given a base filename without a suffix, \p base_name, and a File
 *          Access Property List, \p fapl, h5_fixname_no_suffix() generates
 *          a filename according to the configuration set on \p fapl. The
 *          resulting filename is copied to \p fullname, which is \p size
 *          bytes in size, including space for the NUL terminator.
 *
 *          h5_fixname_no_suffix() is similar to h5_fixname(), but generates
 *          a filename that has no suffix, where the filename from
 *          h5_fixname() would typically have ".h5". This function is mostly
 *          useful for getting a new base filename that has been specialized
 *          according to the VFD set on \p fapl.
 *
 * \see h5_fixname()
 *
 */
H5TEST_DLL char *h5_fixname_no_suffix(const char *base_name, hid_t fapl, char *fullname, size_t size);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Creates a VFD-dependent printf-style filename from a base filename
 *        without a suffix and a File Access Property List
 *
 * \param[in]  base_name Filename, without a suffix, to use as a base when
 *                       creating the resulting filename
 * \param[in]  fapl File Access Property List to use when creating the
 *                  resulting filename
 * \param[out]  fullname Buffer to store the resulting filename in
 * \param[in]  size Size in bytes of the \p fullname output buffer
 *
 * \return A pointer to the resulting filename on success; NULL otherwise
 *
 * \details Given a base filename without a suffix, \p base_name, and a File
 *          Access Property List, \p fapl, h5_fixname_printf() generates
 *          a filename according to the configuration set on \p fapl. The
 *          resulting filename is copied to \p fullname, which is \p size
 *          bytes in size, including space for the NUL terminator.
 *
 *          h5_fixname_printf() is similar to h5_fixname(), but generates
 *          a filename that can be passed through a printf-style function to
 *          obtain the final, processed filename. Essentially, this function
 *          replaces all % characters that would be used by a file driver
 *          with %%.
 *
 * \see h5_fixname()
 *
 */
H5TEST_DLL char *h5_fixname_printf(const char *base_name, hid_t fapl, char *fullname, size_t size);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Temporarily turn off hardware datatype conversion pathways during
 *        testing
 *
 * \return nothing
 *
 * \details h5_no_hwconv() temporarily turns off hardware datatype
 *          conversions in HDF5 during testing by calling H5Tunregister() to
 *          unregister all the hard conversion pathways. This is useful for
 *          verifying that datatype conversions for different datatypes still
 *          work correctly when emulated by the library.
 *
 */
H5TEST_DLL void h5_no_hwconv(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief "Removes" a ':'-delimited prefix from a filename
 *
 * \param[in]  filename The filename to remove a ':'-delimited prefix from
 *
 * \return Adjusted pointer into original \p filename buffer
 *
 * \details h5_rmprefix() "removes" a ':'-delimited prefix from a filename by
 *          simply searching for the first occurrence of ':' and returning a
 *          pointer into \p filename just past that occurrence. No actual
 *          changes are made to the \p filename buffer.
 *
 *          For example,
 *
 *          Input                  Return
 *          pfs:/scratch1/dataX    /scratch1/dataX
 *          /scratch2/dataY        /scratch2/dataY
 *
 */
H5TEST_DLL const char *h5_rmprefix(const char *filename);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Prints out hostname(1)-like information
 *
 * \return nothing
 *
 * \details h5_show_hostname() prints out hostname(1)-like information to
 *          stdout. It also prints out each MPI process' rank value if HDF5
 *          is built with parallel functionality enabled and MPI is
 *          initialized. Otherwise, if HDF5 is built with thread-safe
 *          functionality enabled and MPI is not initialized or HDF5 is not
 *          built with parallel functionality enabled, it also prints out
 *          thread ID values.
 *
 */
H5TEST_DLL void h5_show_hostname(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Calculates the size of a given file in bytes
 *
 * \param[in]  filename The filename of the file to retrieve the size of
 * \param[in]  fapl File Access Property List to use when accessing the file
 *
 * \return The size of the file in bytes
 *
 * \details h5_get_file_size() returns the size in bytes of the file with the
 *          given filename \p filename. A File Access Property List specified
 *          for \p fapl will modify how the file size is calculated. If
 *          H5P_DEFAULT is passed for \p fapl, stat(2) or the platform
 *          equivalent is used to determine the file size. Otherwise, the
 *          calculation depends on the file driver set on \p fapl. For
 *          example, a FAPL setup with the MPI I/O driver will cause
 *          h5_get_file_size() to use MPI_File_get_size(), while a FAPL setup
 *          with the family driver will cause h5_get_file_size() to sum the
 *          sizes of each of the family files in the overall logical file.
 *
 */
H5TEST_DLL h5_stat_size_t h5_get_file_size(const char *filename, hid_t fapl);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Creates a byte-for-byte copy of a file
 *
 * \param[in]  origfilename The filename of the file to make a copy of
 * \param[in]  local_copy_name The filename for the resulting file that is
 *                             created
 *
 * \return Non-negative on success; negative on failure
 *
 * \details Given a file with the filename \p origfilename,
 *          h5_make_local_copy() makes a byte-for-byte copy of the file, which
 *          is then named \p local_copy_name, using POSIX I/O. This function
 *          is useful for making copies of test files that are under version
 *          control. Tests should make a copy of the original file and then
 *          operate on the copy.
 *
 */
H5TEST_DLL int h5_make_local_copy(const char *origfilename, const char *local_copy_name);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Creates a byte-for-byte copy of a file
 *
 * \param[in]  orig The filename of the file to make a copy of
 * \param[in]  dest The filename for the resulting file that is created
 *
 * \return Non-negative on success; negative on failure
 *
 * \details Given a file with the filename \p orig,
 *          h5_duplicate_file_by_bytes() makes a byte-for-byte copy of the
 *          file, which is then named \p dest, using C stdio functions. This
 *          function is useful for making copies of test files that are under
 *          version control. Tests should make a copy of the original file
 *          and then operate on the copy.
 *
 */
H5TEST_DLL int h5_duplicate_file_by_bytes(const char *orig, const char *dest);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Performs a byte-for-byte comparison between two files
 *
 * \param[in]  fname1 The filename of the first file for comparison
 * \param[in]  fname2 The filename of the second file for comparison
 *
 * \return 0 if the files are identical; non-zero otherwise
 *
 * \details h5_compare_file_bytes() performs a byte-for-byte comparison
 *          between two files, \p fname1 and \p fname2.
 *
 */
H5TEST_DLL int h5_compare_file_bytes(char *fname1, char *fname2);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Checks a list of files to ensure that groups in those files have
 *        their symbol table information cached, if present and if their
 *        parent group also uses a symbol table.
 *
 * \param[in]  base_name Array of filenames, without suffixes, to check
 * \param[in]  fapl File Access Property List to use when checking files
 *
 * \return \herr_t
 *
 * \details h5_verify_cached_stabs() verifies that all groups in a set of
 *          files have their symbol table information cached, if present and
 *          if their parent group also uses a symbol table. This function
 *          does not check that the root group's symbol table information is
 *          cached in the superblock. \p base_name is an array of filenames
 *          without suffixes, where the last entry must be NULL. \p fapl is
 *          the File Access Property List used to open each of the files in
 *          \p base_name.
 *
 */
H5TEST_DLL herr_t h5_verify_cached_stabs(const char *base_name[], hid_t fapl);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Returns a "dummy" VFD class for use in testing
 *
 * \return On success, returns a pointer to a generally non-functional
 *         "dummy" VFD class which must be freed by the caller with free();
 *         on failure, returns NULL
 *
 * \details h5_get_dummy_vfd_class() allocates and returns a pointer to a
 *          "dummy" Virtual File Driver class which is generally
 *          non-functional. The caller must free the returned pointer with
 *          free() once it is no longer needed.
 *
 *          In some of the test code, we need a disposable VFD but we don't
 *          want to mess with the real VFDs and we also don't have access to
 *          the internals of the real VFDs (which use static globals and
 *          functions) to easily duplicate them (e.g.: for testing VFD ID
 *          handling).
 *
 *          This API call will return a pointer to a VFD class that can be
 *          used to construct a test VFD using H5FDregister().
 *
 */
H5TEST_DLL H5FD_class_t *h5_get_dummy_vfd_class(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Returns a "dummy" VOL class for use in testing
 *
 * \return On success, returns a pointer to a generally non-functional
 *         "dummy" VOL class which must be freed by the caller with free();
 *         on failure, returns NULL
 *
 * \details h5_get_dummy_vol_class() allocates and returns a pointer to a
 *          "dummy" Virtual Object Layer connector class which is generally
 *          non-functional. The caller must free the returned pointer with
 *          free() once it is no longer needed.
 *
 *          In some of the test code, we need a disposable VOL connector but
 *          we don't want to mess with the real VFDs and we also don't have
 *          access to the internals of the real VOL connectors (which use
 *          static globals and functions) to easily duplicate them (e.g.: for
 *          testing VOL connector ID handling).
 *
 *          This API call will return a pointer to a VOL class that can be
 *          used to construct a test VOL using H5VLregister_connector().
 *
 */
H5TEST_DLL H5VL_class_t *h5_get_dummy_vol_class(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Translates a given library version bound value into a canonical
 *        string.
 *
 * \param[in]  libver The library version bound value to translate into a
 *                    canonical string
 *
 * \return A pointer to a string for the canonical name of the given library
 *         bound version
 *
 * \details h5_get_version_string() translates a given library bound version,
 *          \p libver, into a pointer to a canonical string value which is
 *          returned. For example, specifying the version H5F_LIBVER_V114
 *          would return a pointer to a string "v114".
 *
 */
H5TEST_DLL const char *h5_get_version_string(H5F_libver_t libver);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Determines whether file locking is enabled on the system
 *
 * \param[out]  are_enabled Boolean value for whether file locking is enabled
 *                          on the system
 *
 * \return \herr_t
 *
 * \details h5_check_if_file_locking_enabled() checks if file locking is
 *          enabled on the system by creating a temporary file and calling
 *          flock() or the platform equivalent on it. If this function
 *          succeeds and \p are_enabled is set to true, file locking is
 *          enabled on the system. Otherwise, it should be assumed the file
 *          locking is not enabled or is problematic.
 *
 */
H5TEST_DLL herr_t h5_check_if_file_locking_enabled(bool *are_enabled);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Parses the HDF5_USE_FILE_LOCKING environment variable
 *
 * \param[out]  use_locks Tri-value for whether file locking should be used
 * \param[out]  ignore_disabled_locks Tri-value for whether failures should
 *                                    be ignored when attempting to use file
 *                                    locking
 *
 * \return nothing
 *
 * \details h5_check_file_locking_env_var() parses the value of the
 *          HDF5_USE_FILE_LOCKING environment variable, if set, and returns
 *          whether or not file locking should be used and whether or not
 *          failures should be ignored when attempting to use file locking on
 *          a system where it is disabled.
 *
 *          When this function returns, \p use_locks is set to one of the
 *          following values:
 *
 *          true  - file locking should be used
 *          false - file locking should not be used
 *          FAIL  - the HDF5_USE_FILE_LOCKING environment variable was not
 *                  set
 *
 *          When this function returns, \p ignore_disabled_locks is set to
 *          one of the following values:
 *
 *          true  - failures should be ignored when attempting to use file
 *                  locking
 *          false - failures should NOT be ignored when attempting to use
 *                  file locking
 *          FAIL  - the HDF5_USE_FILE_LOCKING environment variable was not
 *                  set or file locking was specified to not be used
 *
 */
H5TEST_DLL void h5_check_file_locking_env_var(htri_t *use_locks, htri_t *ignore_disabled_locks);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Check if the VOL connector being used for testing is (or the VOL
 *        connector stack being used resolves to) the library's native VOL
 *        connector
 *
 * \param[in]  fapl_id File Access Property List to use when checking for
 *                     native VOL connector usage
 * \param[in]  obj_id ID of an HDF5 object to use when checking for native
 *                    VOL connector usage
 * \param[out] is_native_vol Boolean value for whether the native VOL
 *                           connector is being used
 *
 * \return \herr_t
 *
 * \details h5_using_native_vol() checks if the VOL connector being used for
 *          testing is (or the VOL connector stack being used resolves to)
 *          the native VOL connector. Either or both of \p fapl_id and
 *          \p obj_id may be provided, but checking of \p obj_id takes
 *          precedence. #H5I_INVALID_HID should be specified for the
 *          parameter that is not provided, if any.
 *
 *          \p obj_id must be the ID of an HDF5 object that is accessed with
 *          the VOL connector to check. If \p obj_id is provided, the entire
 *          VOL connector stack is checked to see if it resolves to the
 *          native VOL connector. If only \p fapl_id is provided, only the
 *          top-most VOL connector set on \p fapl_id is checked against the
 *          native VOL connector.
 *
 *          The HDF5_VOL_CONNECTOR environment variable is not checked here,
 *          as that only overrides the setting for the default File Access
 *          Property List, which may not be the File Access Property List
 *          used for accessing \p obj_id. There is also complexity in
 *          determining whether the connector stack resolves to the native
 *          VOL connector when the only information available is a string.
 *
 */
H5TEST_DLL herr_t h5_using_native_vol(hid_t fapl_id, hid_t obj_id, bool *is_native_vol);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Returns a pointer to the name of the VFD being used for testing
 *
 * \return A pointer to the name of the VFD being used for testing
 *
 * \details h5_get_test_driver_name() returns a pointer to the name of the
 *          VFD being used for testing. If the environment variable
 *          HDF5_DRIVER or HDF5_TEST_DRIVER has been set, the value set for
 *          that variable is returned, with preference given to the
 *          HDF5_DRIVER environment variable if both are set. Otherwise, the
 *          name of the library's default VFD is returned.
 *
 */
H5TEST_DLL const char *h5_get_test_driver_name(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Determines whether the given VFD name matches the name of the
 *        library's default VFD.
 *
 * \param[in]  drv_name Name of the VFD to check against the library's
 *                      default VFD name
 *
 * \return True if the given VFD name matches the name of the library's
 *         default VFD; false otherwise
 *
 * \details h5_using_default_driver() determines whether a given VFD name,
 *          \p drv_name, matches the name of the library's default VFD. If
 *          \p drv_name is NULL, h5_get_test_driver_name() is first used to
 *          obtain the name of the VFD being used for testing.
 *
 * \see h5_get_test_driver_name()
 *
 */
H5TEST_DLL bool h5_using_default_driver(const char *drv_name);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Determines whether the VFD set on the given File Access Property
 *        List is a parallel-enabled VFD.
 *
 * \param[in]  fapl_id File Access Property List to use when checking for a
 *                     parallel-enabled VFD
 * \param[out] driver_is_parallel Boolean value for whether the VFD set on
 *                                \p fapl_id is a parallel-enabled VFD
 *
 * \return \herr_t
 *
 * \details h5_using_parallel_driver() checks if the VFD set on \p fapl_id
 *          is a parallel-enabled VFD that supports MPI. A VFD must have set
 *          the H5FD_FEAT_HAS_MPI feature flag to be considered as a
 *          parallel-enabled VFD. \p fapl_id may be H5P_DEFAULT.
 *
 */
H5TEST_DLL herr_t h5_using_parallel_driver(hid_t fapl_id, bool *driver_is_parallel);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Determines whether the VFD set on the given File Access Property
 *        List creates files that are compatible with the library's default
 *        VFD.
 *
 * \param[in]  fapl_id File Access Property List to use when checking for a
 *                     VFD that is compatible with the library's default VFD
 * \param[out] default_vfd_compatible Boolean value for whether the VFD set
 *                                    on \p fapl_id is compatible with the
 *                                    library's default VFD
 *
 * \return \herr_t
 *
 * \details h5_driver_is_default_vfd_compatible() checks if the VFD set on
 *          \p fapl_id creates files that are compatible with the library's
 *          default VFD. For example, the core and MPI I/O drivers create
 *          files that are compatible with the library's default VFD, while
 *          the multi and family drivers do not since they split the HDF5
 *          file into several different files. This check is helpful for
 *          skipping tests that use pre-generated testing files. VFDs that
 *          create files which aren't compatible with the default VFD will
 *          generally not be able to open these pre-generated files and those
 *          particular tests will fail.
 *
 */
H5TEST_DLL herr_t h5_driver_is_default_vfd_compatible(hid_t fapl_id, bool *default_vfd_compatible);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Determines whether the VFD with the given name matches the name of
 *        a VFD which stores data using multiple files.
 *
 * \param[in]  drv_name Name of the VFD to check
 * \param[in]  flags Bitfield of flags controlling how specific the check for
 *                   usage of multiple files is
 *
 * \return True if the VFD with the given name matches a VFD which stores
 *         data using multiple files; false otherwise
 *
 * \details h5_driver_uses_multiple_files() checks if the given VFD name,
 *          \p drv_name, matches the name of a VFD which stores data using
 *          multiple files, according to the specified \p flags. If
 *          \p drv_name is NULL, h5_get_test_driver_name() is called to
 *          obtain the name of the VFD in use before making the comparison.
 *          The values for \p flags are as follows:
 *
 *          + H5_EXCLUDE_MULTIPART_DRIVERS - If specified, this flag excludes
 *            any drivers which store data using multiple files which,
 *            together, make up a single logical file. These are drivers like
 *            the split, multi and family drivers.
 *          + H5_EXCLUDE_NON_MULTIPART_DRIVERS - If specified, this flag
 *            excludes any drivers which store data using multiple files
 *            which are separate logical files. The splitter driver is an
 *            example of this type of driver.
 *
 * \see h5_get_test_driver_name()
 *
 */
H5TEST_DLL bool h5_driver_uses_multiple_files(const char *drv_name, unsigned flags);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Returns a random integer value; analogous to rand().
 *
 * \return A random integer value
 *
 * \details h5_local_rand() is a counterpart for rand() which doesn't modify
 *          the underlying C/POSIX library rand/random state, as this can
 *          cause spurious test failures. The state is always initialized to
 *          the same value so that each process in the parallel tests always
 *          gets the same sequence. This function is adapted from the example
 *          code in the POSIX.1-2001 standard.
 *
 */
H5TEST_DLL int h5_local_rand(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Seeds the random number sequence for h5_local_rand(); analogous to
 *        srand().
 *
 * \param[in]  seed Value to use for seeding h5_local_rand()'s random number
 *                  sequence
 *
 * \return nothing
 *
 * \details h5_local_srand() seeds the random number sequence generated by
 *          h5_local_rand().
 *
 * \see h5_local_rand()
 *
 */
H5TEST_DLL void h5_local_srand(unsigned int seed);

#ifdef H5_HAVE_FILTER_SZIP
/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Determines whether the library's SZIP filter has encoding/decoding
 *        functionality enabled.
 *
 * \return 1 if both encoding and decoding are enabled; 0 if only decoding
 *         is enabled; negative if only encoding is enabled or if neither
 *         encoding or decoding are enabled
 *
 * \details h5_szip_can_encode() returns a value that indicates whether or
 *          not the library's SZIP filter has encoding/decoding enabled.
 *
 */
H5TEST_DLL int h5_szip_can_encode(void);
#endif /* H5_HAVE_FILTER_SZIP */

#ifdef H5_HAVE_PARALLEL
/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Parses the HDF5_MPI_INFO environment variable to create and setup
 *        an MPI Info object for use in parallel testing
 *
 * \return 0 on success; non-zero on failure
 *
 * \details h5_set_info_object() parses the HDF5_MPI_INFO environment
 *          variable for ";"-delimited key=value pairs and sets them on the
 *          h5_io_info_g MPI Info global variable for later use by testing.
 *
 */
H5TEST_DLL int h5_set_info_object(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Prints out the key-value pairs in an MPI Info object
 *
 * \param[in]  info MPI Info object to dump the contents of
 *
 * \return nothing
 *
 * \details h5_dump_info_object() iterates through all the keys set on the
 *          MPI info object, \p info, and prints out each key-value pair to
 *          stdout.
 *
 */
H5TEST_DLL void h5_dump_info_object(MPI_Info info);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Retrieves the value of an environment variable and broadcasts it to
 *        other MPI processes to ensure all processes see the same value
 *
 * \param[in]  comm MPI communicator object to use for collective operations
 * \param[in]  root MPI rank value to get the value of the environment
 *                  variable on and to broadcast the value from
 * \param[in]  name Name of the environment variable to retrieve the value of
 *
 * \return A pointer to the value of the environment variable if that
 *         environment variable is set; NULL otherwise
 *
 * \details getenv_all() retrieves the value of the environment variable
 *          \p name, if set, on the MPI process with rank value \p root on
 *          the MPI Communicator \p comm, then broadcasts the result to other
 *          MPI processes in \p comm. This function is collective across the
 *          MPI Communicator specified in \p comm. If MPI is not initialized,
 *          this function simply calls getenv(name) and returns a pointer to
 *          the result.
 *
 *          <b>Note:</b> the pointer returned by this function is only valid
 *          until the next call to getenv_all() and the data stored there
 *          must be copied somewhere else before any further calls to
 *          getenv_all() take place.
 *
 */
H5TEST_DLL char *getenv_all(MPI_Comm comm, int root, const char *name);
#endif

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief "Sends" a message to another testing process using a temporary file
 *
 * \param[in]  file Name of the temporary file to be created
 * \param[in]  arg1 First argument to be written to the temporary file
 * \param[in]  arg2 Second argument to be written to the temporary file
 *
 * \return nothing
 *
 * \details h5_send_message() facilitates inter-process communication in
 *          tests by "sending" a message to another process using a temporary
 *          file. \p file is the name of the temporary file to be created.
 *          \p arg1 and \p arg2 are strings to be written to the first and
 *          second lines of the file, respectively, and may both be NULL.
 *
 *          When there are multiple test processes that need to communicate
 *          with each other, they do so by writing and reading signal files
 *          on disk, the names and contents of which are used to inform a
 *          process about when it can proceed and what it should do next.
 *
 * \see h5_wait_message()
 *
 */
H5TEST_DLL void h5_send_message(const char *file, const char *arg1, const char *arg2);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Waits for a message from another testing process to be available
 *
 * \param[in]  file Name of the temporary file to read
 *
 * \return \herr_t
 *
 * \details h5_wait_message() facilitates inter-process communication in
 *          tests by waiting until a temporary file written by the
 *          h5_send_message() function is available for reading. \p file is
 *          the name of the file being waited on and should match the
 *          filename provided to h5_send_message(). This function repeatedly
 *          attempts to open a file with the given filename until it is
 *          either successful or times out. The temporary file is removed
 *          once it has been successfully opened.
 *
 *          When there are multiple test processes that need to communicate
 *          with each other, they do so by writing and reading signal files
 *          on disk, the names and contents of which are used to inform a
 *          process about when it can proceed and what it should do next.
 *
 * \see h5_send_message()
 *
 */
H5TEST_DLL herr_t h5_wait_message(const char *file);

#ifdef H5_HAVE_ROS3_VFD
/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Loads AWS credentials from environment variables
 *
 * \param[out] values_found              Whether or not any values were found
 *                                       from environment variables
 * \param[out] key_id_out                AWS access key ID loaded, if any
 * \param[in]  key_id_out_len            Size of buffer for AWS access key ID
 * \param[out] secret_access_key_out     AWS secret access key loaded, if any
 * \param[in]  secret_access_key_out_len Size of buffer for AWS secret access
 *                                       key
 * \param[out] aws_region_out            AWS region loaded, if any
 * \param[in]  aws_region_out_len        Size of buffer for AWS region
 * \param[out] session_token_out         Session token for temporary
 *                                       credentials loaded, if any
 * \param[in]  aws_region_out_len        Size of buffer for session token
 *
 * \return \herr_t
 *
 * \details h5_load_aws_environment() attempts to load AWS credentials from
 *          the standard environment variables used by AWS tools. This
 *          function is primarily used for testing the ROS3 VFD.
 *
 */
H5TEST_DLL herr_t h5_load_aws_environment(bool *values_found, char *key_id_out, size_t key_id_out_len,
                                          char *secret_access_key_out, size_t secret_access_key_out_len,
                                          char *aws_region_out, size_t aws_region_out_len,
                                          char *session_token_out, size_t session_token_out_len);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Loads AWS credentials from ~/.aws/config and ~/.aws/credentials
 *
 * \param[in]  profile_name              Name of the AWS profile to load
 *                                       credentials from
 * \param[out] profile_found             Whether or not an AWS profile was
 *                                       found from the profile files
 * \param[out] key_id_out                AWS access key ID loaded, if any
 * \param[in]  key_id_out_len            Size of buffer for AWS access key
 *                                       ID
 * \param[out] secret_access_key_out     AWS secret access key loaded, if any
 * \param[in]  secret_access_key_out_len Size of buffer for AWS secret access
 *                                       key
 * \param[out] aws_region_out            AWS region loaded, if any
 * \param[in]  aws_region_out_len        Size of buffer for AWS region
 *
 * \return \herr_t
 *
 * \details h5_load_aws_profile() attempts to load AWS credentials from the
 *          standard files used by AWS tools. This function is primarily
 *          used for testing the ROS3 VFD.
 *
 */
H5TEST_DLL herr_t h5_load_aws_profile(const char *profile_name, bool *profile_found, char *key_id_out,
                                      size_t key_id_out_len, char *secret_access_key_out,
                                      size_t secret_access_key_out_len, char *aws_region_out,
                                      size_t aws_region_out_len);
#endif

#ifdef __cplusplus
}
#endif
#endif
