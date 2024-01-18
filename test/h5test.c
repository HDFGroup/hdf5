/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose:  Provides support functions for most of the hdf5 tests cases.
 *
 */

#undef NDEBUG /*override -DNDEBUG      */

#include "h5test.h"
#include "H5srcdir.h"
#include "H5srcdir_str.h"

/* Necessary for h5_verify_cached_stabs() */
#define H5G_FRIEND /*suppress error about including H5Gpkg      */
#define H5G_TESTING
#include "H5Gpkg.h"

#ifdef H5_HAVE_WIN32_API
#include <process.h>
#endif /* H5_HAVE_WIN32_API */

/*
 * Define these environment variables or constants to influence functions in
 * this test support library.  The environment variable is used in preference
 * to the cpp constant.  If neither is defined then use some default value.
 *
 * HDF5_DRIVER:    This string describes what low level file driver to
 *      use for HDF5 file access.
 *
 * HDF5_LIBVER_BOUNDS:    This string describes what library version bounds to
 *      use for HDF5 file access.  See h5_get_libver_fapl() for details.
 *
 * HDF5_PREFIX:    A string to add to the beginning of all serial test
 *      file names.  This can be used to run tests in a
 *      different file system (e.g., "/tmp" or "/tmp/myname").
 *      The prefix will be separated from the base file name
 *      by a slash. See h5_fixname() for details.
 *
 * HDF5_PARAPREFIX:  A string to add to the beginning of all parallel test
 *      file names.  This can be used to tell MPIO what driver
 *      to use (e.g., "gfs:", "ufs:", or "nfs:") or to use a
 *      different file system (e.g., "/tmp" or "/tmp/myname").
 *      The prefix will be separated from the base file name
 *      by a slash. See h5_fixname() for details.
 *
 */
/*
 * In a parallel machine, the filesystem suitable for compiling is
 * unlikely a parallel file system that is suitable for parallel I/O.
 * There is no standard pathname for the parallel file system.  /tmp
 * is about the best guess.
 */
#ifndef HDF5_PARAPREFIX
#define HDF5_PARAPREFIX ""
#endif
char *paraprefix = NULL; /* for command line option para-prefix */
#ifdef H5_HAVE_PARALLEL
MPI_Info h5_io_info_g = MPI_INFO_NULL; /* MPI INFO object for IO */
#endif

#define READ_BUF_SIZE 65536

/*
 * These are the letters that are appended to the file name when generating
 * names for the split and multi drivers. They are:
 *
 *   m: All meta data when using the split driver.
 *  s: The userblock, superblock, and driver info block
 *  b: B-tree nodes
 *  r: Dataset raw data
 *  g: Global heap
 *  l: local heap (object names)
 *  o: object headers
 */
static const char *multi_letters = "msbrglo";

/* Temporary file for sending signal messages */
#define TMP_SIGNAL_FILE "tmp_signal_file"

/* The # of seconds to wait for the message file--used by h5_wait_message() */
#define MESSAGE_TIMEOUT 300 /* Timeout in seconds */

/* Buffer to construct path in and return pointer to */
static char srcdir_path[1024];

/* Buffer to construct file in and return pointer to */
static char srcdir_testpath[1024];

/*  The strings that correspond to library version bounds H5F_libver_t in H5Fpublic.h */
/*  This is used by h5_get_version_string() */
const char *LIBVER_NAMES[] = {"earliest", /* H5F_LIBVER_EARLIEST = 0  */
                              "v18",      /* H5F_LIBVER_V18 = 1       */
                              "v110",     /* H5F_LIBVER_V110 = 2      */
                              "v112",     /* H5F_LIBVER_V112 = 3      */
                              "v114",     /* H5F_LIBVER_V114 = 4      */
                              "latest",   /* H5F_LIBVER_V116 = 5      */
                              NULL};

/* Previous error reporting function */
static H5E_auto2_t err_func = NULL;

/* Global variables for testing */
size_t   n_tests_run_g     = 0;
size_t   n_tests_passed_g  = 0;
size_t   n_tests_failed_g  = 0;
size_t   n_tests_skipped_g = 0;
uint64_t vol_cap_flags_g   = H5VL_CAP_FLAG_NONE;

static herr_t h5_errors(hid_t estack, void *client_data);
static char  *h5_fixname_real(const char *base_name, hid_t fapl, const char *_suffix, char *fullname,
                              size_t size, bool nest_printf, bool subst_for_superblock);

/*-------------------------------------------------------------------------
 * Function:  h5_errors
 *
 * Purpose:  Displays the error stack after printing "*FAILED*".
 *
 * Return:  Success:  0
 *
 *    Failure:  -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
h5_errors(hid_t estack, void H5_ATTR_UNUSED *client_data)
{
    H5_FAILED();
    H5Eprint2(estack, stdout);
    return 0;
}

/*-------------------------------------------------------------------------
 * Function:  h5_clean_files
 *
 * Purpose:  Cleanup temporary test files (always).
 *    base_name contains the list of test file names.
 *
 * Return:  void
 *
 *-------------------------------------------------------------------------
 */
void
h5_clean_files(const char *base_name[], hid_t fapl)
{
    int i;

    for (i = 0; base_name[i]; i++) {
        h5_delete_test_file(base_name[i], fapl);
    }

    /* Close the FAPL used to access the file */
    H5Pclose(fapl);
} /* end h5_clean_files() */

/*-------------------------------------------------------------------------
 * Function:    h5_delete_test_file
 *
 * Purpose      Clean up temporary test files.
 *
 *              When a test calls h5_fixname() to get a VFD-dependent
 *              test file name, this function can be used to clean it up.
 *
 * Return:      void
 *
 *              Since this is a cleanup file, we don't care if it fails.
 *
 *-------------------------------------------------------------------------
 */
void
h5_delete_test_file(const char *base_name, hid_t fapl)
{
    char filename[1024]; /* VFD-dependent filename to delete */

    /* Get the VFD-dependent filename */
    if (NULL == h5_fixname(base_name, fapl, filename, sizeof(filename)))
        return;

    H5E_BEGIN_TRY
    {
        H5Fdelete(filename, fapl);
    }
    H5E_END_TRY

} /* end h5_delete_test_file() */

/*-------------------------------------------------------------------------
 * Function:    h5_delete_all_test_files
 *
 * Purpose      Clean up temporary test files.
 *
 *              When a test calls h5_fixname() get a VFD-dependent
 *              test file name, this function can be used to clean it up.
 *
 *              This function takes an array of filenames that ends with
 *              a NULL string and cleans them all.
 *
 * Return:      void
 *
 *              Since this is a cleanup file, we don't care if it fails.
 *
 *-------------------------------------------------------------------------
 */
void
h5_delete_all_test_files(const char *base_name[], hid_t fapl)
{
    int i; /* iterator                             */

    for (i = 0; base_name[i]; i++) {
        h5_delete_test_file(base_name[i], fapl);
    } /* end for */

} /* end h5_delete_all_test_files() */

/*-------------------------------------------------------------------------
 * Function:  h5_cleanup
 *
 * Purpose:  Cleanup temporary test files.
 *    base_name contains the list of test file names.
 *    The file access property list is also closed.
 *
 * Return:  Non-zero if cleanup actions were performed; zero otherwise.
 *
 *-------------------------------------------------------------------------
 */
int
h5_cleanup(const char *base_name[], hid_t fapl)
{
    int retval = 0;

    if (GetTestCleanup()) {
        /* Clean up files in base_name, and the FAPL */
        h5_clean_files(base_name, fapl);

        retval = 1;
    } /* end if */

    /* Restore the original error reporting routine */
    h5_restore_err();

    return retval;
} /* end h5_cleanup() */

/*-------------------------------------------------------------------------
 * Function:    h5_test_shutdown
 *
 * Purpose:     Performs any special test cleanup required before the test
 *              ends.
 *
 *              NOTE: This function should normally only be called once
 *              in a given test, usually just before leaving main(). It
 *              is intended for use in the single-file unit tests, not
 *              testhdf5.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
h5_test_shutdown(void)
{

    /* Restore the original error reporting routine */
    h5_restore_err();
} /* end h5_test_shutdown() */

/*-------------------------------------------------------------------------
 * Function:    h5_restore_err
 *
 * Purpose:     Restore the default error handler.
 *
 * Return:      N/A
 *
 *-------------------------------------------------------------------------
 */
void
h5_restore_err(void)
{
    /* Restore the original error reporting routine */
    assert(err_func != NULL);
    H5Eset_auto2(H5E_DEFAULT, err_func, NULL);
    err_func = NULL;
}

/*-------------------------------------------------------------------------
 * Function:    h5_reset
 *
 * Purpose:     Reset the library by closing it
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
void
h5_reset(void)
{
    fflush(stdout);
    fflush(stderr);
    H5close();

    /* Save current error stack reporting routine and redirect to our local one */
    assert(err_func == NULL);
    H5Eget_auto2(H5E_DEFAULT, &err_func, NULL);
    H5Eset_auto2(H5E_DEFAULT, h5_errors, NULL);
}

/*-------------------------------------------------------------------------
 * Function:    h5_test_init
 *
 * Purpose:     Performs any special actions before the test begins.
 *
 *              NOTE: This function should normally only be called once
 *              in a given test, usually at the beginning of main(). It
 *              is intended for use in the single-file unit tests, not
 *              testhdf5.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
h5_test_init(void)
{
    fflush(stdout);
    fflush(stderr);
    H5close();

    /* Save current error stack reporting routine and redirect to our local one */
    assert(err_func == NULL);
    H5Eget_auto2(H5E_DEFAULT, &err_func, NULL);
    H5Eset_auto2(H5E_DEFAULT, h5_errors, NULL);
} /* end h5_test_init() */

/*-------------------------------------------------------------------------
 * Function:  h5_fixname
 *
 * Purpose:  Create a file name from a file base name like `test' and
 *    return it through the FULLNAME (at most SIZE characters
 *    counting the null terminator). The full name is created by
 *    prepending the contents of HDF5_PREFIX (separated from the
 *    base name by a slash) and appending a file extension based on
 *    the driver supplied, resulting in something like
 *    `ufs:/u/matzke/test.h5'.
 *
 * Return:  Success:  The FULLNAME pointer.
 *
 *    Failure:  NULL if BASENAME or FULLNAME is the null
 *        pointer or if FULLNAME isn't large enough for
 *        the result.
 *
 *-------------------------------------------------------------------------
 */
char *
h5_fixname(const char *base_name, hid_t fapl, char *fullname, size_t size)
{
    return (h5_fixname_real(base_name, fapl, ".h5", fullname, size, false, false));
}

/*-------------------------------------------------------------------------
 * Function:    h5_fixname_superblock
 *
 * Purpose:     Like h5_fixname() but returns the name of the file you'd
 *              open to find the superblock. Useful for when you have to
 *              open a file with open(2) but the h5_fixname() string
 *              contains stuff like format strings.
 *
 * Return:      Success:    The FULLNAME pointer.
 *
 *              Failure:    NULL if BASENAME or FULLNAME is the null
 *                          pointer or if FULLNAME isn't large enough for
 *                          the result.
 *
 *-------------------------------------------------------------------------
 */
char *
h5_fixname_superblock(const char *base_name, hid_t fapl_id, char *fullname, size_t size)
{
    return (h5_fixname_real(base_name, fapl_id, ".h5", fullname, size, false, true));
}

/*-------------------------------------------------------------------------
 * Function:  h5_fixname_no_suffix
 *
 * Purpose:  Same as h5_fixname but with no suffix appended
 *
 * Return:  Success:  The FULLNAME pointer.
 *
 *    Failure:  NULL if BASENAME or FULLNAME is the null
 *        pointer or if FULLNAME isn't large enough for
 *        the result.
 *
 *-------------------------------------------------------------------------
 */
char *
h5_fixname_no_suffix(const char *base_name, hid_t fapl, char *fullname, size_t size)
{
    return (h5_fixname_real(base_name, fapl, NULL, fullname, size, false, false));
}

/*-------------------------------------------------------------------------
 * Function:  h5_fixname_printf
 *
 * Purpose:  Same as h5_fixname but returns a filename that can be passed
 *    through a printf-style function once before being passed to the file
 *    driver.  Basically, replaces all % characters used by the file
 *    driver with %%.
 *
 * Return:  Success:  The FULLNAME pointer.
 *
 *    Failure:  NULL if BASENAME or FULLNAME is the null
 *        pointer or if FULLNAME isn't large enough for
 *        the result.
 *
 *-------------------------------------------------------------------------
 */
char *
h5_fixname_printf(const char *base_name, hid_t fapl, char *fullname, size_t size)
{
    return (h5_fixname_real(base_name, fapl, ".h5", fullname, size, true, false));
}

/*-------------------------------------------------------------------------
 * Function:  h5_fixname_real
 *
 * Purpose:  Create a file name from a file base name like `test' and
 *    return it through the FULLNAME (at most SIZE characters
 *    counting the null terminator). The full name is created by
 *    prepending the contents of HDF5_PREFIX (separated from the
 *    base name by a slash) and appending a file extension based on
 *    the driver supplied, resulting in something like
 *    `ufs:/u/matzke/test.h5'.
 *
 * Return:  Success:  The FULLNAME pointer.
 *
 *    Failure:  NULL if BASENAME or FULLNAME is the null
 *        pointer or if FULLNAME isn't large enough for
 *        the result.
 *
 *-------------------------------------------------------------------------
 */
static char *
h5_fixname_real(const char *base_name, hid_t fapl, const char *_suffix, char *fullname, size_t size,
                bool nest_printf, bool subst_for_superblock)
{
    const char *prefix         = NULL;
    const char *driver_env_var = NULL; /* HDF5_DRIVER environment variable     */
    char       *ptr, last = '\0';
    const char *suffix = _suffix;
    size_t      i, j;
    hid_t       driver     = H5I_INVALID_HID;
    bool        isppdriver = false; /* if the driver is MPI parallel */

    if (!base_name || !fullname || size < 1)
        return NULL;

    memset(fullname, 0, size);

    /* Determine if driver is set by environment variable. If it is,
     * only generate a suffix if fixing the filename for the superblock
     * file. */
    driver_env_var = getenv(HDF5_DRIVER);
    if (driver_env_var && (H5P_DEFAULT == fapl) && subst_for_superblock)
        fapl = H5P_FILE_ACCESS_DEFAULT;

    /* figure out the suffix */
    if (H5P_DEFAULT != fapl) {
        if ((driver = H5Pget_driver(fapl)) < 0)
            return NULL;

        if (suffix) {
            if (H5FD_FAMILY == driver) {
                if (subst_for_superblock)
                    suffix = "-000000.h5";
                else {
                    if (nest_printf) {
                        suffix = "-%%06d.h5";
                    }
                    else {
                        suffix = "-%06d.h5";
                    }
                }
            }
            else if (H5FD_MULTI == driver) {

                /* Check the HDF5_DRIVER environment variable in case
                 * we are using the split driver since both of those
                 * use the multi VFD under the hood.
                 */
#ifdef HDF5_DRIVER
                /* Use the environment variable, then the compile-time constant */
                if (!driver_env_var)
                    driver_env_var = HDF5_DRIVER;
#endif
                if (driver_env_var && !strcmp(driver_env_var, "split")) {
                    /* split VFD */
                    if (subst_for_superblock)
                        suffix = ".h5.meta";
                }
                else {
                    /* multi VFD */
                    if (subst_for_superblock)
                        suffix = "-s.h5";
                    else
                        suffix = NULL;
                }
            }
        }
    }

    if (h5_using_parallel_driver(fapl, &isppdriver) < 0)
        return NULL;

    /* Check HDF5_NOCLEANUP environment setting.
     * (The #ifdef is needed to prevent compile failure in case MPI is not
     * configured.)
     */
    if (isppdriver) {
#ifdef H5_HAVE_PARALLEL
        if (getenv_all(MPI_COMM_WORLD, 0, HDF5_NOCLEANUP))
            SetTestNoCleanup();
#endif /* H5_HAVE_PARALLEL */
    }
    else {
        if (getenv(HDF5_NOCLEANUP))
            SetTestNoCleanup();
    }

    /* Check what prefix to use for test files. Process HDF5_PARAPREFIX and
     * HDF5_PREFIX.
     * Use different ones depending on parallel or serial driver used.
     * (The #ifdef is needed to prevent compile failure in case MPI is not
     * configured.)
     */
    if (isppdriver) {
#ifdef H5_HAVE_PARALLEL
        /*
         * For parallel:
         *      First use command line option, then the environment
         *      variable, then try the constant
         */
        static int explained = 0;

        prefix = (paraprefix ? paraprefix : getenv_all(MPI_COMM_WORLD, 0, "HDF5_PARAPREFIX"));

        if (!prefix && !explained) {
            /* print hint by process 0 once. */
            int mpi_rank;

            MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

            if (mpi_rank == 0)
                printf("*** Hint ***\n"
                       "You can use environment variable HDF5_PARAPREFIX to "
                       "run parallel test files in a\n"
                       "different directory or to add file type prefix. e.g.,\n"
                       "   HDF5_PARAPREFIX=pfs:/PFS/user/me\n"
                       "   export HDF5_PARAPREFIX\n"
                       "*** End of Hint ***\n");

            explained = true;
#ifdef HDF5_PARAPREFIX
            prefix = HDF5_PARAPREFIX;
#endif /* HDF5_PARAPREFIX */
        }
#endif /* H5_HAVE_PARALLEL */
    }
    else {
        /*
         * For serial:
         *      First use the environment variable, then try the constant
         */
        prefix = getenv("HDF5_PREFIX");

#ifdef HDF5_PREFIX
        if (!prefix)
            prefix = HDF5_PREFIX;
#endif /* HDF5_PREFIX */
    }

    /* Prepend the prefix value to the base name */
    if (prefix && *prefix) {
        if (isppdriver) {
            /* This is a parallel system */
            char *subdir;

            if (!strcmp(prefix, HDF5_PARAPREFIX)) {
                /*
                 * If the prefix specifies the HDF5_PARAPREFIX directory, then
                 * default to using the "/tmp/$USER" or "/tmp/$LOGIN"
                 * directory instead.
                 */
                char *user, *login;

                user   = getenv("USER");
                login  = getenv("LOGIN");
                subdir = (user ? user : login);

                if (subdir) {
                    for (i = 0; i < size && prefix[i]; i++)
                        fullname[i] = prefix[i];

                    fullname[i++] = '/';

                    for (j = 0; i < size && subdir[j]; ++i, ++j)
                        fullname[i] = subdir[j];
                }
            }

            if (!fullname[0]) {
                /* We didn't append the prefix yet */
                strncpy(fullname, prefix, size);
                fullname[size - 1] = '\0';
            }

            if (strlen(fullname) + strlen(base_name) + 1 < size) {
                /*
                 * Append the base_name with a slash first. Multiple
                 * slashes are handled below.
                 */
                h5_stat_t buf;

                if (HDstat(fullname, &buf) < 0)
                    /* The directory doesn't exist just yet */
                    if (HDmkdir(fullname, (mode_t)0755) < 0 && errno != EEXIST)
                        /*
                         * We couldn't make the "/tmp/${USER,LOGIN}"
                         * subdirectory.  Default to PREFIX's original
                         * prefix value.
                         */
                        strcpy(fullname, prefix);

                strcat(fullname, "/");
                strcat(fullname, base_name);
            }
            else {
                /* Buffer is too small */
                return NULL;
            }
        }
        else {
            if (snprintf(fullname, size, "%s/%s", prefix, base_name) == (int)size)
                /* Buffer is too small */
                return NULL;
        }
    }
    else if (strlen(base_name) >= size) {
        /* Buffer is too small */
        return NULL;
    }
    else {
        strcpy(fullname, base_name);
    }

    /* Append a suffix */
    if (suffix) {
        if (strlen(fullname) + strlen(suffix) >= size)
            return NULL;

        strcat(fullname, suffix);
    }

    /* Remove any double slashes in the filename */
    for (ptr = fullname, i = j = 0; ptr && i < size; i++, ptr++) {
        if (*ptr != '/' || last != '/')
            fullname[j++] = *ptr;

        last = *ptr;
    }

    return fullname;
}

/*-------------------------------------------------------------------------
 * Function:  h5_rmprefix
 *
 * Purpose:  This "removes" the MPIO driver prefix part of the file name
 *    by returning a pointer that points at the non-prefix component
 *              part of the file name.  E.g.,
 *        Input      Return
 *        pfs:/scratch1/dataX    /scratch1/dataX
 *        /scratch2/dataY           /scratch2/dataY
 *    Note that there is no change to the original file name.
 *
 * Return:  Success:  a pointer at the non-prefix part.
 *
 *-------------------------------------------------------------------------
 */
H5_ATTR_PURE const char *
h5_rmprefix(const char *filename)
{
    const char *ret_ptr;

    if ((ret_ptr = strstr(filename, ":")) == NULL)
        ret_ptr = filename;
    else
        ret_ptr++;

    return (ret_ptr);
}

/*-------------------------------------------------------------------------
 * Function:    h5_fileaccess
 *
 * Purpose:     Returns a file access template which is the default template
 *              but with a file driver, VOL connector, or libver bound set
 *              according to a constant or environment variable
 *
 * Return:      Success:    A file access property list
 *              Failure:    H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
h5_fileaccess(void)
{
    hid_t fapl_id = H5I_INVALID_HID;

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;

    /* Check for libver bounds */
    if (h5_get_libver_fapl(fapl_id) < 0)
        goto error;

    return fapl_id;

error:
    if (fapl_id != H5I_INVALID_HID)
        H5Pclose(fapl_id);
    return H5I_INVALID_HID;
} /* end h5_fileaccess() */

/*-------------------------------------------------------------------------
 * Function:    h5_fileaccess_flags
 *
 * Purpose:     Returns a file access template which is the default template
 *              but with a file driver, VOL connector, or libver bound set
 *              according to a constant or environment variable
 *
 * Return:      Success:    A file access property list
 *              Failure:    H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
h5_fileaccess_flags(unsigned flags)
{
    hid_t fapl_id = H5I_INVALID_HID;

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;

    /* Check for libver bounds */
    if ((flags & H5_FILEACCESS_LIBVER) && h5_get_libver_fapl(fapl_id) < 0)
        goto error;

    return fapl_id;

error:
    if (fapl_id != H5I_INVALID_HID)
        H5Pclose(fapl_id);
    return H5I_INVALID_HID;
} /* end h5_fileaccess_flags() */

/*-------------------------------------------------------------------------
 * Function:    h5_get_libver_fapl
 *
 * Purpose:     Sets the library version bounds for a FAPL according to the
 *              value in the constant or environment variable "HDF5_LIBVER_BOUNDS".
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
herr_t
h5_get_libver_fapl(hid_t fapl)
{
    const char *env   = NULL; /* HDF5_DRIVER environment variable     */
    const char *tok   = NULL; /* strtok pointer                       */
    char       *lasts = NULL; /* Context pointer for strtok_r() call */
    char        buf[1024];    /* buffer for tokenizing HDF5_DRIVER    */

    /* Get the environment variable, if it exists */
    env = getenv("HDF5_LIBVER_BOUNDS");
#ifdef HDF5_LIBVER_BOUNDS
    /* Use the environment variable, then the compile-time constant */
    if (!env)
        env = HDF5_LIBVER_BOUNDS;
#endif

    /* If the environment variable was not set, just return
     * without modifying the FAPL.
     */
    if (!env || !*env)
        goto done;

    /* Get the first 'word' of the environment variable.
     * If it's nothing (environment variable was whitespace)
     * just return the default fapl.
     */
    strncpy(buf, env, sizeof(buf));
    buf[sizeof(buf) - 1] = '\0';
    if (NULL == (tok = HDstrtok_r(buf, " \t\n\r", &lasts)))
        goto done;

    if (!strcmp(tok, "latest")) {
        /* use the latest format */
        if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            goto error;
    } /* end if */
    else {
        /* Unknown setting */
        goto error;
    } /* end else */

done:
    return 0;

error:
    return -1;
} /* end h5_get_libver_fapl() */

/*-------------------------------------------------------------------------
 * Function:  h5_no_hwconv
 *
 * Purpose:  Turn off hardware data type conversions.
 *
 * Return:  void
 *
 *-------------------------------------------------------------------------
 */
void
h5_no_hwconv(void)
{
    H5Tunregister(H5T_PERS_HARD, NULL, (hid_t)-1, (hid_t)-1, NULL);
}

/*-------------------------------------------------------------------------
 * Function:  h5_show_hostname
 *
 * Purpose:  Show hostname.  Show process ID if in MPI environment.
 *
 * Return:  void
 *
 *-------------------------------------------------------------------------
 */
void
h5_show_hostname(void)
{
    char hostname[80];
#ifdef H5_HAVE_WIN32_API
    WSADATA wsaData;
    int     err;
#endif
#ifdef H5_HAVE_PARALLEL
    int mpi_rank, mpi_initialized, mpi_finalized;
#endif

    /* try show the process or thread id in multiple processes cases*/
#ifdef H5_HAVE_PARALLEL
    MPI_Initialized(&mpi_initialized);
    MPI_Finalized(&mpi_finalized);

    if (mpi_initialized && !mpi_finalized) {
        /* Prevent output here from getting mixed with later output */
        MPI_Barrier(MPI_COMM_WORLD);
        MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
        printf("MPI-process %d.", mpi_rank);
    }
    else
        printf("thread 0.");
#else
    printf("thread %" PRIu64 ".", H5TS_thread_id());
#endif
#ifdef H5_HAVE_WIN32_API

    err = WSAStartup(MAKEWORD(2, 2), &wsaData);
    if (err != 0) {
        /* could not find a usable WinSock DLL */
        return;
    }

    /* Confirm that the WinSock DLL supports 2.2.*/
    /* Note that if the DLL supports versions greater    */
    /* than 2.2 in addition to 2.2, it will still return */
    /* 2.2 in wVersion since that is the version we      */
    /* requested.                                        */

    if (LOBYTE(wsaData.wVersion) != 2 || HIBYTE(wsaData.wVersion) != 2) {
        /* could not find a usable WinSock DLL */
        WSACleanup();
        return;
    }

#endif
#ifdef H5_HAVE_GETHOSTNAME
    if (gethostname(hostname, (size_t)80) < 0)
        printf(" gethostname failed\n");
    else
        printf(" hostname=%s\n", hostname);
#else
    printf(" gethostname not supported\n");
#endif
#ifdef H5_HAVE_WIN32_API
    WSACleanup();
#endif
#ifdef H5_HAVE_PARALLEL
    /* Prevent output here from getting mixed with later output */
    if (mpi_initialized && !mpi_finalized)
        MPI_Barrier(MPI_COMM_WORLD);
#endif
}

#ifdef H5_HAVE_PARALLEL
/*
 * Function:    h5_set_info_object
 * Purpose:     Process environment variables setting to set up MPI Info
 *              object.
 * Return:      0 if all is fine; otherwise non-zero.
 */
int
h5_set_info_object(void)
{
    char *envp; /* environment pointer */
    int   ret_value = 0;

    /* handle any MPI INFO hints via $HDF5_MPI_INFO */
    if ((envp = getenv("HDF5_MPI_INFO")) != NULL) {
        char *next, *valp;

        valp = envp = next = strdup(envp);

        if (!valp)
            return 0;

        /* create an INFO object if not created yet */
        if (h5_io_info_g == MPI_INFO_NULL)
            MPI_Info_create(&h5_io_info_g);

        do {
            size_t len;
            char  *key_val, *endp, *namep;

            if (*valp == ';')
                valp++;

            /* copy key/value pair into temporary buffer */
            len  = strcspn(valp, ";");
            next = &valp[len];
            if (NULL == (key_val = (char *)calloc(1, len + 1)))
                return -1;

            /* increment the next pointer past the terminating semicolon */
            if (*next == ';')
                ++next;

            namep = strncpy(key_val, valp, len);

            /* pass up any beginning whitespaces */
            while (*namep && (*namep == ' ' || *namep == '\t'))
                namep++;

            if (!*namep)
                continue; /* was all white space, so move to next k/v pair */

            /* eat up any ending white spaces */
            endp = &namep[strlen(namep) - 1];

            while (endp && (*endp == ' ' || *endp == '\t'))
                *endp-- = '\0';

            /* find the '=' */
            valp = strchr(namep, '=');

            if (valp != NULL) { /* it's a valid key/value pairing */
                char *tmp_val = valp + 1;

                /* change '=' to \0, move valp down one */
                *valp-- = '\0';

                /* eat up ending whitespace on the "key" part */
                while (*valp == ' ' || *valp == '\t')
                    *valp-- = '\0';

                valp = tmp_val;

                /* eat up beginning whitespace on the "value" part */
                while (*valp == ' ' || *valp == '\t')
                    *valp++ = '\0';

                /* actually set the darned thing */
                if (MPI_SUCCESS != MPI_Info_set(h5_io_info_g, namep, valp)) {
                    printf("MPI_Info_set failed\n");
                    ret_value = -1;
                }
            }

            valp = next;
            free(key_val);
        } while (next && *next);

        free(envp);
    }

    return ret_value;
}

/*
 * Function:    h5_dump_info_object
 * Purpose:     Display content of an MPI Info object
 * Return:      void
 */
void
h5_dump_info_object(MPI_Info info)
{
    char key[MPI_MAX_INFO_KEY + 1];
    char value[MPI_MAX_INFO_VAL + 1];
    int  flag;
    int  i, nkeys;

    printf("Dumping MPI Info Object (up to %d bytes per item):\n", MPI_MAX_INFO_VAL);
    if (info == MPI_INFO_NULL) {
        printf("object is MPI_INFO_NULL\n");
    }
    else {
        MPI_Info_get_nkeys(info, &nkeys);
        printf("object has %d items\n", nkeys);
        for (i = 0; i < nkeys; i++) {
            MPI_Info_get_nthkey(info, i, key);
            MPI_Info_get(info, key, MPI_MAX_INFO_VAL, value, &flag);
            printf("%s=%s\n", key, value);
        }
    }
}
#endif /* H5_HAVE_PARALLEL */

/*-------------------------------------------------------------------------
 * Function:  h5_get_file_size
 *
 * Purpose:  Get the current size of a file (in bytes)
 *
 * Return:  Success:  Size of file in bytes
 *    Failure:  -1
 *
 *-------------------------------------------------------------------------
 */
/* Disable warning for "format not a string literal" here -QAK */
/*
 *      This pragma only needs to surround the snprintf() calls with
 *      temp in the code below, but early (4.4.7, at least) gcc only
 *      allows diagnostic pragmas to be toggled outside of functions.
 */
H5_GCC_CLANG_DIAG_OFF("format-nonliteral")
h5_stat_size_t
h5_get_file_size(const char *filename, hid_t fapl)
{
    char      temp[2048]; /* Temporary buffer for file names */
    h5_stat_t sb;         /* Structure for querying file info */
    int       j = 0;

    if (fapl == H5P_DEFAULT) {
        /* Get the file's statistics */
        if (0 == HDstat(filename, &sb))
            return ((h5_stat_size_t)sb.st_size);
    } /* end if */
    else {
        hid_t driver; /* VFD used for file */

        /* Get the driver used when creating the file */
        if ((driver = H5Pget_driver(fapl)) < 0)
            return (-1);

        /* Check for simple cases */
        if (driver == H5FD_SEC2 || driver == H5FD_STDIO || driver == H5FD_CORE ||
#ifdef H5_HAVE_WINDOWS
            driver == H5FD_WINDOWS ||
#endif /* H5_HAVE_WINDOWS */
#ifdef H5_HAVE_DIRECT
            driver == H5FD_DIRECT ||
#endif /* H5_HAVE_DIRECT */
            driver == H5FD_LOG || driver == H5FD_SPLITTER) {
            /* Get the file's statistics */
            if (0 == HDstat(filename, &sb))
                return ((h5_stat_size_t)sb.st_size);
        } /* end if */
        else if (driver == H5FD_MULTI) {
            H5FD_mem_t     mt;
            h5_stat_size_t tot_size       = 0;
            char          *driver_env_var = NULL;

            driver_env_var = getenv(HDF5_DRIVER);
            if (driver_env_var && !strcmp(driver_env_var, "split")) {
                for (mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; mt++) {
                    if (mt != H5FD_MEM_DRAW && mt != H5FD_MEM_SUPER)
                        continue;

                    /* Create the filename to query */
                    if (mt == H5FD_MEM_DRAW) {
                        snprintf(temp, sizeof temp, "%s.raw", filename);
                    }
                    else {
                        snprintf(temp, sizeof temp, "%s.meta", filename);
                    }

                    /* Check for existence of file */
                    if (0 == HDaccess(temp, F_OK)) {
                        /* Get the file's statistics */
                        if (0 != HDstat(temp, &sb))
                            return (-1);

                        /* Add to total size */
                        tot_size += (h5_stat_size_t)sb.st_size;
                    } /* end if */
                }     /* end for */
            }
            else {
                assert(strlen(multi_letters) == H5FD_MEM_NTYPES);
                for (mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; mt++) {
                    /* Create the filename to query */
                    snprintf(temp, sizeof temp, "%s-%c.h5", filename, multi_letters[mt]);

                    /* Check for existence of file */
                    if (0 == HDaccess(temp, F_OK)) {
                        /* Get the file's statistics */
                        if (0 != HDstat(temp, &sb))
                            return (-1);

                        /* Add to total size */
                        tot_size += (h5_stat_size_t)sb.st_size;
                    } /* end if */
                }     /* end for */
            }

            /* Return total size */
            return (tot_size);
        } /* end if */
#ifdef H5_HAVE_PARALLEL
        else if (driver == H5FD_MPIO) {
            MPI_File   fh; /* MPI file handle used to open the file and verify its size */
            int        mpi_ret;
            MPI_Offset file_size;

            mpi_ret = MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_RDONLY, MPI_INFO_NULL, &fh);
            if (mpi_ret != MPI_SUCCESS)
                return -1;
            mpi_ret = MPI_File_get_size(fh, &file_size);
            if (mpi_ret != MPI_SUCCESS)
                return -1;
            mpi_ret = MPI_File_close(&fh);
            if (mpi_ret != MPI_SUCCESS)
                return -1;

            return file_size;
        }
#endif /* H5_HAVE_PARALLEL */
        else if (driver == H5FD_FAMILY) {
            h5_stat_size_t tot_size = 0;

            /* Try all filenames possible, until we find one that's missing */
            for (j = 0; /*void*/; j++) {
                /* Create the filename to query */
                snprintf(temp, sizeof temp, filename, j);

                /* Check for existence of file */
                if (HDaccess(temp, F_OK) < 0)
                    break;

                /* Get the file's statistics */
                if (0 != HDstat(temp, &sb))
                    return (-1);

                /* Add to total size */
                tot_size += (h5_stat_size_t)sb.st_size;
            } /* end for */

            /* Return total size */
            return (tot_size);
        } /* end if */
        else if (driver == H5FD_SUBFILING) {
            hsize_t size;
            hid_t   fid = H5I_INVALID_HID;

            if ((fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
                return -1;
            if (H5Fget_filesize(fid, &size) < 0) {
                H5Fclose(fid);
                return -1;
            }

            if (H5Fclose(fid) < 0)
                return -1;

            return (h5_stat_size_t)size;
        }
        else {
            /* Get the file's statistics */
            if (0 == HDstat(filename, &sb))
                return ((h5_stat_size_t)sb.st_size);
        } /* end else */
    }     /* end else */

    return (-1);
} /* end get_file_size() */
H5_GCC_CLANG_DIAG_ON("format-nonliteral")

/*
 * This routine is designed to provide equivalent functionality to 'printf'
 * and allow easy replacement for environments which don't have stdin/stdout
 * available. (i.e. Windows & the Mac)
 */
H5_ATTR_FORMAT(printf, 1, 2)
int
print_func(const char *format, ...)
{
    va_list arglist;
    int     ret_value;

    va_start(arglist, format);
    ret_value = vprintf(format, arglist);
    va_end(arglist);
    return ret_value;
}

#ifdef H5_HAVE_FILTER_SZIP

/*-------------------------------------------------------------------------
 * Function:  h5_szip_can_encode
 *
 * Purpose:  Retrieve the filter config flags for szip, tell if
 *              encoder is available.
 *
 * Return:  1:  decode+encode is enabled
 *    0:  only decode is enabled
 *              -1: other
 *
 *-------------------------------------------------------------------------
 */
int
h5_szip_can_encode(void)
{
    unsigned int filter_config_flags;

    H5Zget_filter_info(H5Z_FILTER_SZIP, &filter_config_flags);
    if ((filter_config_flags & (H5Z_FILTER_CONFIG_ENCODE_ENABLED | H5Z_FILTER_CONFIG_DECODE_ENABLED)) == 0) {
        /* filter present but neither encode nor decode is supported (???) */
        return -1;
    }
    else if ((filter_config_flags & (H5Z_FILTER_CONFIG_ENCODE_ENABLED | H5Z_FILTER_CONFIG_DECODE_ENABLED)) ==
             H5Z_FILTER_CONFIG_DECODE_ENABLED) {
        /* decoder only: read but not write */
        return 0;
    }
    else if ((filter_config_flags & (H5Z_FILTER_CONFIG_ENCODE_ENABLED | H5Z_FILTER_CONFIG_DECODE_ENABLED)) ==
             H5Z_FILTER_CONFIG_ENCODE_ENABLED) {
        /* encoder only: write but not read (???) */
        return -1;
    }
    else if ((filter_config_flags & (H5Z_FILTER_CONFIG_ENCODE_ENABLED | H5Z_FILTER_CONFIG_DECODE_ENABLED)) ==
             (H5Z_FILTER_CONFIG_ENCODE_ENABLED | H5Z_FILTER_CONFIG_DECODE_ENABLED)) {
        return 1;
    }
    return (-1);
}
#endif /* H5_HAVE_FILTER_SZIP */

#ifdef H5_HAVE_PARALLEL
/*-------------------------------------------------------------------------
 * Function:  getenv_all
 *
 * Purpose:  Used to get the environment that the root MPI task has.
 *     name specifies which environment variable to look for
 *     val is the string to which the value of that environment
 *     variable will be copied.
 *
 *     NOTE: The pointer returned by this function is only
 *     valid until the next call to getenv_all and the data
 *     stored there must be copied somewhere else before any
 *     further calls to getenv_all take place.
 *
 * Return:  pointer to a string containing the value of the environment variable
 *     NULL if the variable doesn't exist in task 'root's environment.
 *
 *-------------------------------------------------------------------------
 */
char *
getenv_all(MPI_Comm comm, int root, const char *name)
{
    int          mpi_size, mpi_rank, mpi_initialized, mpi_finalized;
    int          len;
    static char *env = NULL;

    assert(name);

    MPI_Initialized(&mpi_initialized);
    MPI_Finalized(&mpi_finalized);

    if (mpi_initialized && !mpi_finalized) {
        MPI_Comm_rank(comm, &mpi_rank);
        MPI_Comm_size(comm, &mpi_size);
        assert(root < mpi_size);

        /* The root task does the getenv call
         * and sends the result to the other tasks */
        if (mpi_rank == root) {
            env = getenv(name);
            if (env) {
                len = (int)strlen(env);
                MPI_Bcast(&len, 1, MPI_INT, root, comm);
                MPI_Bcast(env, len, MPI_CHAR, root, comm);
            }
            else {
                /* len -1 indicates that the variable was not in the environment */
                len = -1;
                MPI_Bcast(&len, 1, MPI_INT, root, comm);
            }
        }
        else {
            MPI_Bcast(&len, 1, MPI_INT, root, comm);
            if (len >= 0) {
                if (env == NULL)
                    env = (char *)malloc((size_t)len + 1);
                else if (strlen(env) < (size_t)len)
                    env = (char *)realloc(env, (size_t)len + 1);

                MPI_Bcast(env, len, MPI_CHAR, root, comm);
                env[len] = '\0';
            }
            else {
                if (env)
                    free(env);
                env = NULL;
            }
        }
#ifndef NDEBUG
        MPI_Barrier(comm);
#endif
    }
    else {
        /* use original getenv */
        if (env)
            free(env);
        env = getenv(name);
    } /* end if */

    return env;
}

#endif

/*-------------------------------------------------------------------------
 * Function:    h5_make_local_copy
 *
 * Purpose:     Make copy of file.  Some tests write to data files under that
 *              are under version control.  Those tests should make a copy of
 *              the versioned file and write to the copy.  This function
 *              prepends srcdir to the name of the file to be copied and uses
 *              the name of the copy as is.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 *-------------------------------------------------------------------------
 */
int
h5_make_local_copy(const char *origfilename, const char *local_copy_name)
{
    int         fd_old = (-1), fd_new = (-1);                    /* File descriptors for copying data */
    ssize_t     nread;                                           /* Number of bytes read in */
    void       *buf      = NULL;                                 /* Buffer for copying data */
    const char *filename = H5_get_srcdir_filename(origfilename); /* Get the test file name to copy */

    if (!filename)
        goto error;

    /* Allocate copy buffer */
    if (NULL == (buf = calloc((size_t)1, (size_t)READ_BUF_SIZE)))
        goto error;

    /* Copy old file into temporary file */
    if ((fd_old = HDopen(filename, O_RDONLY)) < 0)
        goto error;
    if ((fd_new = HDopen(local_copy_name, O_RDWR | O_CREAT | O_TRUNC, H5_POSIX_CREATE_MODE_RW)) < 0)
        goto error;

    /* Copy data */
    while ((nread = HDread(fd_old, buf, (size_t)READ_BUF_SIZE)) > 0)
        if (HDwrite(fd_new, buf, (size_t)nread) < 0)
            goto error;

    /* Close files */
    if (HDclose(fd_old) < 0)
        goto error;
    if (HDclose(fd_new) < 0)
        goto error;

    /* Release memory */
    free(buf);

    return 0;

error:
    /* ignore return values since we're already noted the problem */
    if (fd_old > 0)
        HDclose(fd_old);
    if (fd_new > 0)
        HDclose(fd_new);
    free(buf);
    return -1;
} /* end h5_make_local_copy() */

/*-------------------------------------------------------------------------
 * Function:    h5_verify_cached_stabs_cb
 *
 * Purpose:     Callback function for h5_verify_cached_stabs.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
h5_verify_cached_stabs_cb(hid_t oid, const char H5_ATTR_UNUSED *name, const H5O_info2_t *oinfo,
                          void H5_ATTR_UNUSED *udata)
{
    if (oinfo->type == H5O_TYPE_GROUP)
        return H5G__verify_cached_stabs_test(oid);
    else
        return SUCCEED;
} /* end h5_verify_cached_stabs_cb() */

/*-------------------------------------------------------------------------
 * Function:    h5_verify_cached_stabs
 *
 * Purpose:     Verifies that all groups in every file in base_name have
 *              their symbol table information cached (if present, and if
 *              the parent group also uses a symbol table).  Does not
 *              check that the root group's symbol table information is
 *              cached in the superblock.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 *-------------------------------------------------------------------------
 */
herr_t
h5_verify_cached_stabs(const char *base_name[], hid_t fapl)
{
    hid_t file = H5I_INVALID_HID;
    char  filename[1024];
    int   i = 0;

    while (base_name[i]) {
        if (h5_fixname(base_name[i], fapl, filename, sizeof(filename)) == NULL)
            continue;

        H5E_BEGIN_TRY
        {
            file = H5Fopen(filename, H5F_ACC_RDONLY, fapl);
        }
        H5E_END_TRY
        if (file < 0) {
            i++;
            continue;
        } /* end if */

        if (H5Ovisit3(file, H5_INDEX_NAME, H5_ITER_NATIVE, h5_verify_cached_stabs_cb, NULL, H5O_INFO_BASIC) <
            0)
            goto error;

        if (H5Fclose(file) < 0)
            goto error;
        file = -1;

        i++;
    } /* end while */

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(file);
    }
    H5E_END_TRY

    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    h5_send_message
 *
 * Purpose:     Sends the specified signal.
 *
 *              In terms of this test framework, a signal consists of a file
 *              on disk. Since there are multiple processes that need to
 *              communicate with each other, they do so by writing and
 *              reading signal files on disk, the names and contents of
 *              which are used to inform a process about when it can
 *              proceed and what it should do next.
 *
 *              This function writes a signal file. The first argument is
 *              the name of the signal file, and the second and third
 *              arguments are the contents of the first two lines of the
 *              signal file. The last two arguments may be NULL.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
h5_send_message(const char *send, const char *arg1, const char *arg2)
{
    FILE *signalfile = NULL;

    /* Create signal file (which will send signal to some other process) */
    signalfile = fopen(TMP_SIGNAL_FILE, "w+");

    /* Write messages to signal file, if provided */
    if (arg2 != NULL) {
        assert(arg1);
        fprintf(signalfile, "%s\n%s\n", arg1, arg2);
    } /* end if */
    else if (arg1 != NULL) {
        assert(arg2 == NULL);
        fprintf(signalfile, "%s\n", arg1);
    } /* end if */
    else {
        assert(arg1 == NULL);
        assert(arg2 == NULL);
    } /* end else */

    fclose(signalfile);

    HDrename(TMP_SIGNAL_FILE, send);
} /* h5_send_message() */

/*-------------------------------------------------------------------------
 * Function:    h5_wait_message
 *
 * Purpose:     Waits for the specified signal.
 *
 *              In terms of this test framework, a signal consists of a file
 *              on disk. Since there are multiple processes that need to
 *              communicate with each other, they do so by writing and
 *              reading signal files on disk, the names and contents of
 *              which are used to inform a process about when it can
 *              proceed and what it should do next.
 *
 *              This function continuously attempts to read the specified
 *              signal file from disk, and only continues once it has
 *              successfully done so (i.e., only after another process has
 *              called the "h5_send_message" function to write the signal file).
 *              This function will then immediately remove the file (i.e.,
 *              to indicate that it has been received and can be reused),
 *              and then exits, allowing the calling function to continue.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
herr_t
h5_wait_message(const char *waitfor)
{
    FILE  *returnfile;
    time_t t0, t1;

    /* Start timer. If this function runs for too long (i.e.,
        expected signal is never received), it will
        return failure */
    HDtime(&t0);

    /* Wait for return signal from some other process */
    while ((returnfile = fopen(waitfor, "r")) == NULL) {

        /* make note of current time. */
        HDtime(&t1);

        /* If we've been waiting for a signal for too long, then
            it was likely never sent and we should fail rather
            than loop infinitely */
        if (HDdifftime(t1, t0) > MESSAGE_TIMEOUT) {
            fprintf(stdout, "Error communicating between processes. Make sure test script is running.\n");
            TEST_ERROR;
        } /* end if */
    }     /* end while */

    fclose(returnfile);
    HDunlink(waitfor);

    return SUCCEED;

error:
    return FAIL;
} /* h5_wait_message() */

/* Functions for the dummy VFD class (see below).
 *
 * Useful for testing things like ID handling where we shouldn't mess with the
 * real VFDs.
 */
static H5FD_t *dummy_vfd_open(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr);
static H5FD_t *
dummy_vfd_open(const char H5_ATTR_UNUSED *name, unsigned H5_ATTR_UNUSED flags, hid_t H5_ATTR_UNUSED fapl_id,
               haddr_t H5_ATTR_UNUSED maxaddr)
{
    return NULL;
}

static herr_t dummy_vfd_close(H5FD_t *_file);
static herr_t
dummy_vfd_close(H5FD_t H5_ATTR_UNUSED *_file)
{
    return FAIL;
}

static haddr_t dummy_vfd_get_eoa(const H5FD_t *file, H5FD_mem_t type);
static haddr_t
dummy_vfd_get_eoa(const H5FD_t H5_ATTR_UNUSED *file, H5FD_mem_t H5_ATTR_UNUSED type)
{
    return HADDR_UNDEF;
}

static herr_t dummy_vfd_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t addr);
static herr_t
dummy_vfd_set_eoa(H5FD_t H5_ATTR_UNUSED *_file, H5FD_mem_t H5_ATTR_UNUSED type, haddr_t H5_ATTR_UNUSED addr)
{
    return FAIL;
}

static haddr_t dummy_vfd_get_eof(const H5FD_t *file, H5FD_mem_t type);
static haddr_t
dummy_vfd_get_eof(const H5FD_t H5_ATTR_UNUSED *file, H5FD_mem_t H5_ATTR_UNUSED type)
{
    return HADDR_UNDEF;
}

static herr_t dummy_vfd_read(H5FD_t *_file, H5FD_mem_t type, hid_t fapl_id, haddr_t addr, size_t size,
                             void *buf);
static herr_t
dummy_vfd_read(H5FD_t H5_ATTR_UNUSED *_file, H5FD_mem_t H5_ATTR_UNUSED type, hid_t H5_ATTR_UNUSED fapl_id,
               haddr_t H5_ATTR_UNUSED addr, size_t H5_ATTR_UNUSED size, void H5_ATTR_UNUSED *buf)
{
    return FAIL;
}

static herr_t dummy_vfd_write(H5FD_t *_file, H5FD_mem_t type, hid_t fapl_id, haddr_t addr, size_t size,
                              const void *buf);
static herr_t
dummy_vfd_write(H5FD_t H5_ATTR_UNUSED *_file, H5FD_mem_t H5_ATTR_UNUSED type, hid_t H5_ATTR_UNUSED fapl_id,
                haddr_t H5_ATTR_UNUSED addr, size_t H5_ATTR_UNUSED size, const void H5_ATTR_UNUSED *buf)
{
    return FAIL;
}

/* Dummy VFD with the minimum parameters to make a VFD that can be registered */
#define DUMMY_VFD_VALUE (H5FD_class_value_t)155
static const H5FD_class_t H5FD_dummy_g = {
    H5FD_CLASS_VERSION,  /* struct version  */
    DUMMY_VFD_VALUE,     /* value           */
    "dummy",             /* name            */
    1,                   /* maxaddr         */
    H5F_CLOSE_WEAK,      /* fc_degree       */
    NULL,                /* terminate       */
    NULL,                /* sb_size         */
    NULL,                /* sb_encode       */
    NULL,                /* sb_decode       */
    0,                   /* fapl_size       */
    NULL,                /* fapl_get        */
    NULL,                /* fapl_copy       */
    NULL,                /* fapl_free       */
    0,                   /* dxpl_size       */
    NULL,                /* dxpl_copy       */
    NULL,                /* dxpl_free       */
    dummy_vfd_open,      /* open            */
    dummy_vfd_close,     /* close           */
    NULL,                /* cmp             */
    NULL,                /* query           */
    NULL,                /* get_type_map    */
    NULL,                /* alloc           */
    NULL,                /* free            */
    dummy_vfd_get_eoa,   /* get_eoa         */
    dummy_vfd_set_eoa,   /* set_eoa         */
    dummy_vfd_get_eof,   /* get_eof         */
    NULL,                /* get_handle      */
    dummy_vfd_read,      /* read            */
    dummy_vfd_write,     /* write           */
    NULL,                /* read_vector     */
    NULL,                /* write_vector    */
    NULL,                /* read_selection  */
    NULL,                /* write_selection */
    NULL,                /* flush           */
    NULL,                /* truncate        */
    NULL,                /* lock            */
    NULL,                /* unlock          */
    NULL,                /* del             */
    NULL,                /* ctl             */
    H5FD_FLMAP_DICHOTOMY /* fl_map          */
};

/*-------------------------------------------------------------------------
 * Function:    h5_get_dummy_vfd_class()
 *
 * Purpose:     Returns a disposable, generally non-functional,
 *              VFD class struct.
 *
 *              In some of the test code, we need a disposable VFD but
 *              we don't want to mess with the real VFDs and we also
 *              don't have access to the internals of the real VFDs (which
 *              use static globals and functions) to easily duplicate
 *              them (e.g.: for testing VFD ID handling).
 *
 *              This API call will return a pointer to a VFD class that
 *              can be used to construct a test VFD using H5FDregister().
 *
 * Return:      Success:    A pointer to a VFD class struct
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
H5FD_class_t *
h5_get_dummy_vfd_class(void)
{
    H5FD_class_t *vfd_class = NULL; /* Dummy VFD that will be returned */

    /* Create the class and initialize everything to zero/NULL */
    if (NULL == (vfd_class = (H5FD_class_t *)malloc(sizeof(H5FD_class_t))))
        TEST_ERROR;

    /* Copy the dummy VFD */
    memcpy(vfd_class, &H5FD_dummy_g, sizeof(H5FD_class_t));

    return vfd_class;

error:
    if (vfd_class)
        free(vfd_class);
    return NULL;
} /* h5_get_dummy_vfd_class */

/*-------------------------------------------------------------------------
 * Function:    h5_get_dummy_vol_class()
 *
 * Purpose:     Returns a disposable, generally non-functional,
 *              VOL class struct.
 *
 *              In some of the test code, we need a disposable VOL connector
 *              but we don't want to mess with the real VFDs and we also
 *              don't have access to the internals of the real VOL connectors
 *              (which use static globals and functions) to easily duplicate
 *              them (e.g.: for testing VOL connector ID handling).
 *
 *              This API call will return a pointer to a VOL class that
 *              can be used to construct a test VOL using H5VLregister_connector().
 *
 * Return:      Success:    A pointer to a VOL class struct
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
H5VL_class_t *
h5_get_dummy_vol_class(void)
{
    H5VL_class_t *vol_class = NULL; /* Dummy VOL class that will be returned */

    /* Create the class and initialize everything to zero/NULL */
    if (NULL == (vol_class = (H5VL_class_t *)calloc((size_t)1, sizeof(H5VL_class_t))))
        TEST_ERROR;

    /* Fill in the minimum parameters to make a VOL connector class that
     * can be registered.
     */
    vol_class->version = H5VL_VERSION;
    vol_class->name    = "dummy";

    return vol_class;

error:
    if (vol_class)
        free(vol_class);
    return NULL;
} /* h5_get_dummy_vol_class */

/*-------------------------------------------------------------------------
 * Function:    h5_get_version_string
 *
 * Purpose:     Get the string that corresponds to the libvery version bound.
 *
 * Return:      The string
 *
 *-------------------------------------------------------------------------
 */
H5_ATTR_PURE const char *
h5_get_version_string(H5F_libver_t libver)
{
    return (LIBVER_NAMES[libver]);
} /* end of h5_get_version_string */

/*-------------------------------------------------------------------------
 * Function:    h5_compare_file_bytes()
 *
 * Purpose:     Helper function to compare two files byte-for-byte
 *
 * Return:      Success:  0, if files are identical
 *              Failure: -1, if files differ
 *
 *-------------------------------------------------------------------------
 */
int
h5_compare_file_bytes(char *f1name, char *f2name)
{
    FILE   *f1ptr     = NULL; /* two file pointers */
    FILE   *f2ptr     = NULL;
    HDoff_t f1size    = 0; /* size of the files */
    HDoff_t f2size    = 0;
    char    f1char    = 0; /* one char from each file */
    char    f2char    = 0;
    HDoff_t ii        = 0;
    int     ret_value = 0; /* for error handling */

    /* Open files for reading */
    f1ptr = fopen(f1name, "rb");
    if (f1ptr == NULL) {
        fprintf(stderr, "Unable to fopen() %s\n", f1name);
        ret_value = -1;
        goto done;
    }
    f2ptr = fopen(f2name, "rb");
    if (f2ptr == NULL) {
        fprintf(stderr, "Unable to fopen() %s\n", f2name);
        ret_value = -1;
        goto done;
    }

    /* Get the file sizes and verify that they equal */
    HDfseek(f1ptr, 0, SEEK_END);
    f1size = HDftell(f1ptr);

    HDfseek(f2ptr, 0, SEEK_END);
    f2size = HDftell(f2ptr);

    if (f1size != f2size) {
        fprintf(stderr, "Files differ in size, %" PRIuHSIZE " vs. %" PRIuHSIZE "\n", (hsize_t)f1size,
                (hsize_t)f2size);
        ret_value = -1;
        goto done;
    }

    /* Compare each byte and fail if a difference is found */
    HDrewind(f1ptr);
    HDrewind(f2ptr);
    for (ii = 0; ii < f1size; ii++) {
        if (fread(&f1char, 1, 1, f1ptr) != 1) {
            ret_value = -1;
            goto done;
        }
        if (fread(&f2char, 1, 1, f2ptr) != 1) {
            ret_value = -1;
            goto done;
        }
        if (f1char != f2char) {
            fprintf(stderr, "Mismatch @ 0x%" PRIXHSIZE ": 0x%X != 0x%X\n", (hsize_t)ii, f1char, f2char);
            ret_value = -1;
            goto done;
        }
    }

done:
    if (f1ptr)
        fclose(f1ptr);
    if (f2ptr)
        fclose(f2ptr);
    return ret_value;
} /* end h5_compare_file_bytes() */

/*-------------------------------------------------------------------------
 * Function:    H5_get_srcdir_filename
 *
 * Purpose:     Append the test file name to the srcdir path and return the whole string
 *
 * Return:      The string or NULL (errors or not enough space)
 *
 *-------------------------------------------------------------------------
 */
const char *
H5_get_srcdir_filename(const char *filename)
{
    const char *srcdir = H5_get_srcdir();

    /* Check for error */
    if (NULL == srcdir)
        return NULL;

    /* Build path to test file. We're checking the length so suppress
     * the gcc format-truncation warning.
     */
    if ((strlen(srcdir) + strlen("testfiles/") + strlen(filename) + 1) < sizeof(srcdir_testpath)) {
        H5_GCC_DIAG_OFF("format-truncation")
        snprintf(srcdir_testpath, sizeof(srcdir_testpath), "%stestfiles/%s", srcdir, filename);
        H5_GCC_DIAG_ON("format-truncation")
        return srcdir_testpath;
    }

    /* If not enough space, just return NULL */
    return NULL;
} /* end H5_get_srcdir_filename() */

/*-------------------------------------------------------------------------
 * Function:    H5_get_srcdir
 *
 * Purpose:     Just return the srcdir path
 *
 * Return:      The string
 *
 *-------------------------------------------------------------------------
 */
const char *
H5_get_srcdir(void)
{
    const char *srcdir = getenv("srcdir");

    /* Check for using the srcdir from configure time */
    if (NULL == srcdir)
        srcdir = config_srcdir;

    /* Build path to all test files */
    if ((strlen(srcdir) + 2) < sizeof(srcdir_path)) {
        snprintf(srcdir_path, sizeof(srcdir_path), "%s/", srcdir);
        return (srcdir_path);
    } /* end if */
    else
        return (NULL);
} /* end H5_get_srcdir() */

/*-------------------------------------------------------------------------
 * Function:    h5_duplicate_file_by_bytes
 *
 * Purpose:     Duplicate a file byte-for-byte at filename/path 'orig'
 *              to a new (or replaced) file at 'dest'.
 *
 * Return:      Success:  0, completed successfully
 *              Failure: -1
 *
 *-------------------------------------------------------------------------
 */
int
h5_duplicate_file_by_bytes(const char *orig, const char *dest)
{
    FILE   *orig_ptr  = NULL;
    FILE   *dest_ptr  = NULL;
    hsize_t fsize     = 0;
    hsize_t read_size = 0;
    hsize_t max_buf   = 0;
    void   *dup_buf   = NULL;
    int     ret_value = 0;

    max_buf = 4096 * sizeof(char);

    orig_ptr = fopen(orig, "rb");
    if (NULL == orig_ptr) {
        ret_value = -1;
        goto done;
    }

    HDfseek(orig_ptr, 0, SEEK_END);
    fsize = (hsize_t)HDftell(orig_ptr);
    HDrewind(orig_ptr);

    dest_ptr = fopen(dest, "wb");
    if (NULL == dest_ptr) {
        ret_value = -1;
        goto done;
    }

    read_size = MIN(fsize, max_buf);
    dup_buf   = malloc(read_size);
    if (NULL == dup_buf) {
        ret_value = -1;
        goto done;
    }

    while (read_size > 0) {
        if (fread(dup_buf, read_size, 1, orig_ptr) != 1) {
            ret_value = -1;
            goto done;
        }
        fwrite(dup_buf, read_size, 1, dest_ptr);
        fsize -= read_size;
        read_size = MIN(fsize, max_buf);
    }

done:
    if (orig_ptr != NULL)
        fclose(orig_ptr);
    if (dest_ptr != NULL)
        fclose(dest_ptr);
    if (dup_buf != NULL)
        free(dup_buf);
    return ret_value;
} /* end h5_duplicate_file_by_bytes() */

/*-------------------------------------------------------------------------
 * Function:    h5_check_if_file_locking_enabled
 *
 * Purpose:     Checks if file locking is enabled on this file system.
 *
 * Return:      SUCCEED/FAIL
 *              are_enabled will be false if file locking is disabled on
 *              the file system of if there were errors.
 *
 *-------------------------------------------------------------------------
 */
herr_t
h5_check_if_file_locking_enabled(bool *is_enabled)
{
    const char *filename = "locking_test_file";
    int         pmode    = O_RDWR | O_CREAT | O_TRUNC;
    int         fd       = -1;

    *is_enabled = true;

    if ((fd = HDopen(filename, pmode, H5_POSIX_CREATE_MODE_RW)) < 0)
        goto error;

    /* Test HDflock() to see if it works */
    if (HDflock(fd, LOCK_EX | LOCK_NB) < 0) {
        if (ENOSYS == errno) {
            /* When errno is set to ENOSYS, the file system does not support
             * locking, so ignore it. This is most frequently used on
             * Lustre. If we also want to check for disabled NFS locks
             * we'll need to check for ENOLCK, too. That isn't done by
             * default here since that could also represent an actual
             * error condition.
             */
            errno       = 0;
            *is_enabled = false;
        }
        else
            goto error;
    }
    if (HDflock(fd, LOCK_UN) < 0)
        goto error;

    if (HDclose(fd) < 0)
        goto error;
    if (HDremove(filename) < 0)
        goto error;

    return SUCCEED;

error:
    *is_enabled = false;
    if (fd > -1) {
        HDclose(fd);
        HDremove(filename);
    }
    return FAIL;
} /* end h5_check_if_file_locking_enabled() */

/*-------------------------------------------------------------------------
 * Function:    h5_using_native_vol
 *
 * Purpose:     Checks if the VOL connector being used is (or the VOL
 *              connector stack being used resolves to) the native VOL
 *              connector. Either or both of fapl_id and obj_id may be
 *              provided, but checking of obj_id takes precedence.
 *              H5I_INVALID_HID should be specified for the parameter that
 *              is not provided.
 *
 *              obj_id must be the ID of an HDF5 object that is accessed
 *              with the VOL connector to check. If obj_id is provided, the
 *              entire VOL connector stack is checked to see if it resolves
 *              to the native VOL connector. If only fapl_id is provided,
 *              only the top-most VOL connector set on fapl_id is checked
 *              against the native VOL connector.
 *
 *              The HDF5_VOL_CONNECTOR environment variable is not checked
 *              here, as that only overrides the setting for the default
 *              File Access Property List, which may not be the File Access
 *              Property List used for accessing obj_id. There is also
 *              complexity in determining whether the connector stack
 *              resolves to the native VOL connector when the only
 *              information available is a string.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
h5_using_native_vol(hid_t fapl_id, hid_t obj_id, bool *is_native_vol)
{
    hbool_t is_native = false;
    hid_t   native_id = H5I_INVALID_HID;
    hid_t   vol_id    = H5I_INVALID_HID;
    herr_t  ret_value = SUCCEED;

    assert((fapl_id >= 0) || (obj_id >= 0));
    assert(is_native_vol);

    if (fapl_id == H5P_DEFAULT)
        fapl_id = H5P_FILE_ACCESS_DEFAULT;

    if (obj_id >= 0) {
        if (H5VLobject_is_native(obj_id, &is_native) < 0) {
            ret_value = FAIL;
            goto done;
        }
    }
    else {
        if (true != H5VLis_connector_registered_by_value(H5VL_NATIVE_VALUE)) {
            ret_value = FAIL;
            goto done;
        }

        if ((native_id = H5VLget_connector_id_by_value(H5VL_NATIVE_VALUE)) < 0) {
            ret_value = FAIL;
            goto done;
        }

        if (H5Pget_vol_id(fapl_id, &vol_id) < 0) {
            ret_value = FAIL;
            goto done;
        }

        if (vol_id == native_id)
            is_native = true;
    }

    *is_native_vol = is_native;

done:
    if (vol_id != H5I_INVALID_HID)
        H5VLclose(vol_id);
    if (native_id != H5I_INVALID_HID)
        H5VLclose(native_id);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    h5_using_default_driver
 *
 * Purpose:     Checks if the specified VFD name matches the library's
 *              default VFD. If `drv_name` is NULL, the HDF5_DRIVER
 *              environment is checked instead (if it is set).
 *
 * Return:      true/false
 *
 *-------------------------------------------------------------------------
 */
bool
h5_using_default_driver(const char *drv_name)
{
    bool ret_val = true;

    assert(H5_DEFAULT_VFD == H5FD_SEC2);

    if (!drv_name)
        drv_name = getenv(HDF5_DRIVER);

    if (drv_name)
        return (!strcmp(drv_name, "sec2") || !strcmp(drv_name, "nomatch"));

    return ret_val;
}

/*-------------------------------------------------------------------------
 * Function:    h5_using_parallel_driver
 *
 * Purpose:     Checks if the current VFD set on the given FAPL is a
 *              parallel-enabled VFD (The MPI I/O VFD, for example).
 *
 *              This is mostly useful for avoiding tests that use features
 *              which are not currently supported for parallel HDF5, such
 *              as writing of VL or region reference datatypes.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
h5_using_parallel_driver(hid_t fapl_id, bool *driver_is_parallel)
{
    unsigned long feat_flags = 0;
    hid_t         driver_id  = H5I_INVALID_HID;
    herr_t        ret_value  = SUCCEED;

    assert(fapl_id >= 0);
    assert(driver_is_parallel);

    if (fapl_id == H5P_DEFAULT)
        fapl_id = H5P_FILE_ACCESS_DEFAULT;

    if ((driver_id = H5Pget_driver(fapl_id)) < 0)
        return FAIL;

    if (H5FDdriver_query(driver_id, &feat_flags) < 0)
        return FAIL;

    *driver_is_parallel = (feat_flags & H5FD_FEAT_HAS_MPI);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    h5_driver_is_default_vfd_compatible
 *
 * Purpose:     Checks if the current VFD set on the given FAPL creates a
 *              file that is compatible with the default VFD. Some examples
 *              are the core and MPI I/O drivers. Some counterexamples are
 *              the multi and family drivers, which split the HDF5 file
 *              into several different files.
 *
 *              This routine is helpful for skipping tests that use
 *              pre-generated files. VFDs that create files which aren't
 *              compatible with the default VFD will generally not be able
 *              to open these pre-generated files and those particular
 *              tests will fail.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
h5_driver_is_default_vfd_compatible(hid_t fapl_id, bool *default_vfd_compatible)
{
    unsigned long feat_flags = 0;
    hid_t         driver_id  = H5I_INVALID_HID;
    herr_t        ret_value  = SUCCEED;

    assert(fapl_id >= 0);
    assert(default_vfd_compatible);

    if (fapl_id == H5P_DEFAULT)
        fapl_id = H5P_FILE_ACCESS_DEFAULT;

    if ((driver_id = H5Pget_driver(fapl_id)) < 0)
        return FAIL;

    if (H5FDdriver_query(driver_id, &feat_flags) < 0)
        return FAIL;

    *default_vfd_compatible = (feat_flags & H5FD_FEAT_DEFAULT_VFD_COMPATIBLE);

    return ret_value;
} /* end h5_driver_is_default_vfd_compatible() */

/*-------------------------------------------------------------------------
 * Function:    h5_driver_uses_multiple_files
 *
 * Purpose:     Checks if the specified VFD name matches a driver that
 *              stores data using multiple files.
 *
 *              The following flags can be used to control what types of
 *              drivers are checked for by this routine:
 *
 *              H5_EXCLUDE_MULTIPART_DRIVERS - This flag excludes any
 *                drivers which store data using multiple files which,
 *                together, make up a single logical file. These are
 *                drivers like the split, multi and family drivers.
 *
 *              H5_EXCLUDE_NON_MULTIPART_DRIVERS - This flag excludes any
 *                drivers which store data using multiple files which are
 *                separate logical files. The splitter driver is an example
 *                of this type of driver.
 *
 *              Eventually, this should become a VFD feature flag so this
 *              check is less fragile.
 *
 * Return:      true/false
 *
 *-------------------------------------------------------------------------
 */
bool
h5_driver_uses_multiple_files(const char *drv_name, unsigned flags)
{
    bool ret_val = false;

    if (!drv_name)
        drv_name = getenv(HDF5_DRIVER);

    if (drv_name) {
        if ((flags & H5_EXCLUDE_MULTIPART_DRIVERS) == 0) {
            if (!strcmp(drv_name, "split") || !strcmp(drv_name, "multi") || !strcmp(drv_name, "family") ||
                !strcmp(drv_name, H5FD_SUBFILING_NAME))
                return true;
        }

        if ((flags & H5_EXCLUDE_NON_MULTIPART_DRIVERS) == 0) {
            if (!strcmp(drv_name, "splitter"))
                return true;
        }
    }

    return ret_val;
}
