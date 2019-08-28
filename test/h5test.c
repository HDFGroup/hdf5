/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, November 19, 1998
 *
 * Purpose:  Provides support functions for most of the hdf5 tests cases.
 *
 */

#undef NDEBUG      /*override -DNDEBUG      */

#include "h5test.h"
#include "H5srcdir.h"

/* Necessary for h5_verify_cached_stabs() */
#define H5G_FRIEND        /*suppress error about including H5Gpkg      */
#define H5G_TESTING
#include "H5Gpkg.h"

#ifdef H5_HAVE_WIN32_API
#include <process.h>
#endif  /* H5_HAVE_WIN32_API */

/*
 * Define these environment variables or constants to influence functions in
 * this test support library.  The environment variable is used in preference
 * to the cpp constant.  If neither is defined then use some default value.
 *
 * HDF5_DRIVER:    This string describes what low level file driver to
 *      use for HDF5 file access.  The first word in the
 *      value is the name of the driver and subsequent data
 *      is interpreted according to the driver.  See
 *      h5_get_vfd_fapl() for details.
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
char  *paraprefix = NULL;  /* for command line option para-prefix */
#ifdef H5_HAVE_PARALLEL
MPI_Info    h5_io_info_g=MPI_INFO_NULL;/* MPI INFO object for IO */
#endif

#define READ_BUF_SIZE           65536

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

/* Length of multi-file VFD filename buffers */
#define H5TEST_MULTI_FILENAME_LEN       1024

/* Temporary file for sending signal messages */
#define TMP_SIGNAL_FILE "tmp_signal_file"

/* The # of seconds to wait for the message file--used by h5_wait_message() */
#define MESSAGE_TIMEOUT         300             /* Timeout in seconds */

/*  The strings that correspond to library version bounds H5F_libver_t in H5Fpublic.h */
/*  This is used by h5_get_version_string() */
const char *LIBVER_NAMES[] = {
    "earliest", /* H5F_LIBVER_EARLIEST = 0  */
    "v18",      /* H5F_LIBVER_V18 = 1       */
    "latest",   /* H5F_LIBVER_V110 = 2      */
    NULL
};

/* Previous error reporting function */
static H5E_auto2_t err_func = NULL;

static herr_t h5_errors(hid_t estack, void *client_data);
static char *h5_fixname_real(const char *base_name, hid_t fapl, const char *suffix,
                              char *fullname, size_t size, hbool_t nest_printf);

/*-------------------------------------------------------------------------
 * Function:  h5_errors
 *
 * Purpose:  Displays the error stack after printing "*FAILED*".
 *
 * Return:  Success:  0
 *
 *    Failure:  -1
 *
 * Programmer:  Robb Matzke
 *    Wednesday, March  4, 1998
 *
 * Modifications:
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
 * Programmer:  Neil Fortner
 *              June 1, 2015
 *
 *-------------------------------------------------------------------------
 */
void
h5_clean_files(const char *base_name[], hid_t fapl)
{
    int i;

    for(i = 0; base_name[i]; i++) {
        h5_delete_test_file(base_name[i], fapl);
    }

    /* Close the FAPL used to access the file */
    H5Pclose(fapl);

    return;
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
 * Programmer:  Dana Robinson
 *              February 2016
 *
 *-------------------------------------------------------------------------
 */
/* Disable warning for "format not a string literal" here -QAK */
/*
 *      This pragma only needs to surround the snprintf() calls with
 *      sub_filename in the code below, but early (4.4.7, at least) gcc only
 *      allows diagnostic pragmas to be toggled outside of functions.
 */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat-nonliteral"
void
h5_delete_test_file(const char *base_name, hid_t fapl)
{
    char filename[1024];        /* VFD-dependent filename to delete     */
    char sub_filename[2048];    /* sub-files in multi & family VFDs     */
    hid_t driver = -1;          /* VFD ID                               */

    /* Get the VFD-dependent filename */
    if(NULL == h5_fixname(base_name, fapl, filename, sizeof(filename)))
        return;

    driver = H5Pget_driver(fapl);

    if(driver == H5FD_FAMILY) {
        int j;
        for(j = 0; /*void*/; j++) {
            HDsnprintf(sub_filename, sizeof(sub_filename), filename, j);

            /* If we can't access the file, it probably doesn't exist
             * and we are done deleting the sub-files.
             */
            if(HDaccess(sub_filename, F_OK) < 0)
                break;

            HDremove(sub_filename);
        } /* end for */
    } else if(driver == H5FD_CORE) {
        hbool_t backing;        /* Whether the core file has backing store */

        H5Pget_fapl_core(fapl, NULL, &backing);

        /* If the file was stored to disk with bacing store, remove it */
        if(backing)
            HDremove(filename);
    } else if (driver == H5FD_MULTI) {
        H5FD_mem_t mt;

        HDassert(HDstrlen(multi_letters) == H5FD_MEM_NTYPES);

        for(mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t,mt)) {
            HDsnprintf(sub_filename, sizeof(sub_filename), "%s-%c.h5", filename, multi_letters[mt]);
            HDremove(sub_filename);
        }
    } else {
        HDremove(filename);
    } /* end driver selection tree */

    return;
} /* end h5_delete_test_file() */
#pragma GCC diagnostic pop


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
 * Programmer:  Dana Robinson
 *              February 2016
 *
 *-------------------------------------------------------------------------
 */
void
h5_delete_all_test_files(const char *base_name[], hid_t fapl)
{
    int i;                      /* iterator                             */

    for(i = 0; base_name[i]; i++) {
        h5_delete_test_file(base_name[i], fapl);
    } /* end for */

    return;
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
 * Programmer:  Albert Cheng
 *              May 28, 1998
 *
 *-------------------------------------------------------------------------
 */
int
h5_cleanup(const char *base_name[], hid_t fapl)
{
    int    retval = 0;

    if(GetTestCleanup()) {
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
 * Programmer:  Dana Robinson
 *              February 2016
 *
 *-------------------------------------------------------------------------
 */
void
h5_test_shutdown(void)
{

    /* Restore the original error reporting routine */
    h5_restore_err();

    return;
} /* end h5_test_shutdown() */


/*-------------------------------------------------------------------------
 * Function:    h5_restore_err
 *
 * Purpose:     Restore the default error handler.
 *
 * Return:      N/A
 *
 * Programmer:  Quincey Koziol
 *              Sept 10, 2015
 *
 *-------------------------------------------------------------------------
 */
void
h5_restore_err(void)
{
    /* Restore the original error reporting routine */
    HDassert(err_func != NULL);
    H5Eset_auto2(H5E_DEFAULT, err_func, NULL);
    err_func = NULL;
}


/*-------------------------------------------------------------------------
 * Function:  h5_reset
 *
 * Purpose:  Reset the library by closing it.
 *
 * Return:  void
 *
 * Programmer:  Robb Matzke
 *              Friday, November 20, 1998
 *
 *-------------------------------------------------------------------------
 */
void
h5_reset(void)
{
    HDfflush(stdout);
    HDfflush(stderr);
    H5close();

    /* Save current error stack reporting routine and redirect to our local one */
    HDassert(err_func == NULL);
    H5Eget_auto2(H5E_DEFAULT, &err_func, NULL);
    H5Eset_auto2(H5E_DEFAULT, h5_errors, NULL);

/*
 * I commented this chunk of code out because it's not clear what diagnostics
 *      were being output and under what circumstances, and creating this file
 *      is throwing off debugging some of the tests.  I can't see any _direct_
 *      harm in keeping this section of code, but I can't see any _direct_
 *      benefit right now either.  If we figure out under which circumstances
 *      diagnostics are being output, we should enable this behavior based on
 *      appropriate configure flags/macros.  QAK - 2007/12/20
 */
#ifdef OLD_WAY
{
    char  filename[1024];

    /*
     * Cause the library to emit some diagnostics early so they don't
     * interfere with other formatted output.
     */
    HDsprintf(filename, "/tmp/h5emit-%05d.h5", HDgetpid());
    H5E_BEGIN_TRY {
        hid_t file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT,
                 H5P_DEFAULT);
        hid_t grp = H5Gcreate2(file, "emit", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        H5Gclose(grp);
        H5Fclose(file);
        HDunlink(filename);
    } H5E_END_TRY;
}
#endif /* OLD_WAY */
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
 * Programmer:  Dana Robinson
 *              February 2016
 *
 *-------------------------------------------------------------------------
 */
void
h5_test_init(void)
{
    HDfflush(stdout);
    HDfflush(stderr);
    H5close();

    /* Save current error stack reporting routine and redirect to our local one */
    HDassert(err_func == NULL);
    H5Eget_auto2(H5E_DEFAULT, &err_func, NULL);
    H5Eset_auto2(H5E_DEFAULT, h5_errors, NULL);

    return;
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
 * Programmer:  Robb Matzke
 *              Thursday, November 19, 1998
 *
 *-------------------------------------------------------------------------
 */
char *
h5_fixname(const char *base_name, hid_t fapl, char *fullname, size_t size)
{
    return (h5_fixname_real(base_name, fapl, ".h5", fullname, size, FALSE));
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
    return (h5_fixname_real(base_name, fapl, NULL, fullname, size, FALSE));
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
 * Programmer:  Neil Fortner
 *              Wednesday, July 15, 2015
 *
 *-------------------------------------------------------------------------
 */
char *
h5_fixname_printf(const char *base_name, hid_t fapl, char *fullname, size_t size)
{
    return (h5_fixname_real(base_name, fapl, ".h5", fullname, size, TRUE));
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
 * Programmer:  Robb Matzke
 *              Thursday, November 19, 1998
 *
 *-------------------------------------------------------------------------
 */
static char *
h5_fixname_real(const char *base_name, hid_t fapl, const char *_suffix,
                char *fullname, size_t size, hbool_t nest_printf)
{
    const char     *prefix = NULL;
    const char     *env = NULL;    /* HDF5_DRIVER environment variable     */
    char           *ptr, last = '\0';
    const char     *suffix = _suffix;
    size_t          i, j;
    hid_t           driver = -1;
    int             isppdriver = 0;  /* if the driver is MPI parallel */

    if (!base_name || !fullname || size < 1)
        return NULL;

    HDmemset(fullname, 0, size);

    /* figure out the suffix */
    if(H5P_DEFAULT != fapl) {
        if((driver = H5Pget_driver(fapl)) < 0)
            return NULL;

        if(suffix) {
            if(H5FD_FAMILY == driver) {
                suffix = nest_printf ? "%%05d.h5" : "%05d.h5";
            }
            else if (H5FD_MULTI == driver) {

                /* Get the environment variable, if it exists, in case
                 * we are using the split driver since both of those
                 * use the multi VFD under the hood.
                 */
                env = HDgetenv("HDF5_DRIVER");
#ifdef HDF5_DRIVER
                /* Use the environment variable, then the compile-time constant */
                if(!env)
                    env = HDF5_DRIVER;
#endif
                if(env && !HDstrcmp(env, "split")) {
                    /* split VFD */
                    suffix = NULL;
                }
                else {
                    /* multi VFD */
                    suffix = NULL;
                }
            }
        }
    }

    /* Must first check fapl is not H5P_DEFAULT (-1) because H5FD_XXX
     * could be of value -1 if it is not defined.
     */
    isppdriver = H5P_DEFAULT != fapl && (H5FD_MPIO == driver);

    /* Check HDF5_NOCLEANUP environment setting.
     * (The #ifdef is needed to prevent compile failure in case MPI is not
     * configured.)
     */
    if(isppdriver) {
#ifdef H5_HAVE_PARALLEL
        if(getenv_all(MPI_COMM_WORLD, 0, "HDF5_NOCLEANUP"))
            SetTestNoCleanup();
#endif  /* H5_HAVE_PARALLEL */
    } else {
        if(HDgetenv("HDF5_NOCLEANUP"))
            SetTestNoCleanup();
    }

    /* Check what prefix to use for test files. Process HDF5_PARAPREFIX and
     * HDF5_PREFIX.
     * Use different ones depending on parallel or serial driver used.
     * (The #ifdef is needed to prevent compile failure in case MPI is not
     * configured.)
     */
    if(isppdriver) {
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
                HDprintf("*** Hint ***\n"
                       "You can use environment variable HDF5_PARAPREFIX to "
                       "run parallel test files in a\n"
                       "different directory or to add file type prefix. e.g.,\n"
                       "   HDF5_PARAPREFIX=pfs:/PFS/user/me\n"
                       "   export HDF5_PARAPREFIX\n"
                       "*** End of Hint ***\n");

            explained = TRUE;
#ifdef HDF5_PARAPREFIX
            prefix = HDF5_PARAPREFIX;
#endif  /* HDF5_PARAPREFIX */
        }
#endif  /* H5_HAVE_PARALLEL */
    } else {
        /*
         * For serial:
         *      First use the environment variable, then try the constant
         */
        prefix = HDgetenv("HDF5_PREFIX");

#ifdef HDF5_PREFIX
        if (!prefix)
            prefix = HDF5_PREFIX;
#endif  /* HDF5_PREFIX */
    }

    /* Prepend the prefix value to the base name */
    if (prefix && *prefix) {
        if (isppdriver) {
            /* This is a parallel system */
            char *subdir;

            if (!HDstrcmp(prefix, HDF5_PARAPREFIX)) {
                /*
                 * If the prefix specifies the HDF5_PARAPREFIX directory, then
                 * default to using the "/tmp/$USER" or "/tmp/$LOGIN"
                 * directory instead.
                 */
                char *user, *login;

                user = HDgetenv("USER");
                login = HDgetenv("LOGIN");
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
                HDstrncpy(fullname, prefix, size);
                fullname[size -1] = '\0';
            }

            if (HDstrlen(fullname) + HDstrlen(base_name) + 1 < size) {
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
                        HDstrcpy(fullname, prefix);

                HDstrcat(fullname, "/");
                HDstrcat(fullname, base_name);
            } else {
                /* Buffer is too small */
                return NULL;
            }
        } else {
            if (HDsnprintf(fullname, size, "%s/%s", prefix, base_name) == (int)size)
                /* Buffer is too small */
                return NULL;
        }
    } else if (HDstrlen(base_name) >= size) {
        /* Buffer is too small */
        return NULL;
    } else {
        HDstrcpy(fullname, base_name);
    }

    /* Append a suffix */
    if (suffix) {
        if (HDstrlen(fullname) + HDstrlen(suffix) >= size)
            return NULL;

        HDstrcat(fullname, suffix);
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
 * Programmer:  Albert Cheng; Jun  1, 2006
 *
 *-------------------------------------------------------------------------
 */
H5_ATTR_PURE const char *
h5_rmprefix(const char *filename)
{
    const char *ret_ptr;

    if ((ret_ptr = HDstrstr(filename, ":")) == NULL)
        ret_ptr = filename;
    else
        ret_ptr++;

    return(ret_ptr);
}


/*-------------------------------------------------------------------------
 * Function:    h5_fileaccess
 *
 * Purpose:     Returns a file access template which is the default template
 *              but with a file driver set
 *              according to a constant or environment variable HDF5_DRIVER
 *
 * Return:      Success:    A file access property list
 *              Failure:    H5I_INVALID_HID
 *
 * Programmer:  Robb Matzke
 *              Thursday, November 19, 1998
 *
 *-------------------------------------------------------------------------
 */
hid_t
h5_fileaccess(void)
{
    hid_t       fapl_id = H5I_INVALID_HID;

    if((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;

    /* Attempt to set up a file driver first */
    if(h5_get_vfd_fapl(fapl_id) < 0)
        goto error;

    return fapl_id;

error:
    if(fapl_id != H5I_INVALID_HID)
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
 * Programmer:  Robb Matzke
 *              Thursday, November 19, 1998
 *
 *-------------------------------------------------------------------------
 */
hid_t
h5_fileaccess_flags(unsigned flags)
{
    hid_t       fapl_id = H5I_INVALID_HID;

    if((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;

    /* Attempt to set up a file driver first */
    if((flags & H5_FILEACCESS_VFD) && h5_get_vfd_fapl(fapl_id) < 0)
        goto error;

    return fapl_id;

error:
    if(fapl_id != H5I_INVALID_HID)
        H5Pclose(fapl_id);
    return H5I_INVALID_HID;
} /* end h5_fileaccess_flags() */


/*-------------------------------------------------------------------------
 * Function:    h5_get_vfd_fapl
 *
 * Purpose:     Sets the file driver for a FAPL according to the value specified
 *              in the constant or environment variable "HDF5_DRIVER".
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 * Programmer:  Dana Robinson
 *              February 2016
 *
 *-------------------------------------------------------------------------
 */
herr_t
h5_get_vfd_fapl(hid_t fapl)
{
    const char  *env = NULL;    /* HDF5_DRIVER environment variable     */
    const char  *tok = NULL;    /* strtok pointer                       */
    char        *lasts = NULL;  /* Context pointer for strtok_r() call */
    char        buf[1024];      /* buffer for tokenizing HDF5_DRIVER    */

    /* Get the environment variable, if it exists */
    env = HDgetenv("HDF5_DRIVER");
#ifdef HDF5_DRIVER
    /* Use the environment variable, then the compile-time constant */
    if(!env)
        env = HDF5_DRIVER;
#endif

    /* If the environment variable was not set, just return
     * without modifying the FAPL.
     */
    if(!env || !*env)
        goto done;

    /* Get the first 'word' of the environment variable.
     * If it's nothing (environment variable was whitespace)
     * just return the default fapl.
     */
    HDstrncpy(buf, env, sizeof(buf));
    buf[sizeof(buf) - 1] = '\0';
    if(NULL == (tok = HDstrtok_r(buf, " \t\n\r", &lasts)))
        goto done;

    if(!HDstrcmp(tok, "sec2")) {
        /* POSIX (section 2) read() and write() system calls */
        if(H5Pset_fapl_sec2(fapl) < 0)
            goto error;
    } else if(!HDstrcmp(tok, "stdio")) {
        /* Standard C fread() and fwrite() system calls */
        if(H5Pset_fapl_stdio(fapl) < 0)
            goto error;
    } else if(!HDstrcmp(tok, "core")) {
        /* In-memory driver settings (backing store on, 1 MB increment) */
        if(H5Pset_fapl_core(fapl, (size_t)H5_MB, TRUE) < 0)
            goto error;
    } else if(!HDstrcmp(tok, "core_paged")) {
        /* In-memory driver with write tracking and paging on */
        if(H5Pset_fapl_core(fapl, (size_t)H5_MB, TRUE) < 0)
            goto error;
        if(H5Pset_core_write_tracking(fapl, TRUE, (size_t)4096) < 0)
            goto error;
     } else if(!HDstrcmp(tok, "split")) {
        /* Split meta data and raw data each using default driver */
        if(H5Pset_fapl_split(fapl, "-m.h5", H5P_DEFAULT, "-r.h5", H5P_DEFAULT) < 0)
            goto error;
    } else if(!HDstrcmp(tok, "multi")) {
        /* Multi-file driver, general case of the split driver */
        H5FD_mem_t  memb_map[H5FD_MEM_NTYPES];
        hid_t       memb_fapl[H5FD_MEM_NTYPES];
        const char  *memb_name[H5FD_MEM_NTYPES];
        char        *sv[H5FD_MEM_NTYPES];
        haddr_t     memb_addr[H5FD_MEM_NTYPES];
        H5FD_mem_t  mt;

        HDmemset(memb_map, 0, sizeof(memb_map));
        HDmemset(memb_fapl, 0, sizeof(memb_fapl));
        HDmemset(memb_name, 0, sizeof(memb_name));
        HDmemset(memb_addr, 0, sizeof(memb_addr));

        HDassert(HDstrlen(multi_letters) == H5FD_MEM_NTYPES);
        for(mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, mt)) {
            memb_fapl[mt] = H5P_DEFAULT;
            sv[mt] = (char *)HDmalloc(H5TEST_MULTI_FILENAME_LEN);
            HDassert(sv[mt]);
            HDsprintf(sv[mt], "%%s-%c.h5", multi_letters[mt]);
            memb_name[mt] = sv[mt];
            memb_addr[mt] = (haddr_t)MAX(mt - 1, 0) * (HADDR_MAX / 10);
        } /* end for */

        if(H5Pset_fapl_multi(fapl, memb_map, memb_fapl, memb_name, memb_addr, FALSE) < 0)
            goto error;

        for(mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, mt))
            HDfree(sv[mt]);
    } else if(!HDstrcmp(tok, "family")) {
        /* Family of files, each 1MB and using the default driver */
        hsize_t fam_size = 100 * 1024 * 1024;   /* 100 MB */

        /* Was a family size specified in the environment variable? */
        if((tok = HDstrtok_r(NULL, " \t\n\r", &lasts)))
            fam_size = (hsize_t)(HDstrtod(tok, NULL) * 1024 * 1024);
        if(H5Pset_fapl_family(fapl, fam_size, H5P_DEFAULT) < 0)
            goto error;
    } else if(!HDstrcmp(tok, "log")) {
        /* Log file access */
        unsigned log_flags = H5FD_LOG_LOC_IO | H5FD_LOG_ALLOC;

        /* Were special log file flags specified in the environment variable? */
        if((tok = HDstrtok_r(NULL, " \t\n\r", &lasts)))
            log_flags = (unsigned)HDstrtol(tok, NULL, 0);

        if(H5Pset_fapl_log(fapl, NULL, log_flags, (size_t)0) < 0)
            goto error;
#ifdef H5_HAVE_DIRECT
    } else if(!HDstrcmp(tok, "direct")) {
        /* Linux direct read() and write() system calls.  Set memory boundary,
         * file block size, and copy buffer size to the default values.
         */
        if(H5Pset_fapl_direct(fapl, 1024, 4096, 8 * 4096) < 0)
            goto error;
#endif
    } else {
        /* Unknown driver */
        goto error;
    } /* end if */

done:
    return 0;

error:
    return -1;
} /* end h5_get_vfd_fapl() */


/*-------------------------------------------------------------------------
 * Function:  h5_no_hwconv
 *
 * Purpose:  Turn off hardware data type conversions.
 *
 * Return:  void
 *
 * Programmer:  Robb Matzke
 *              Friday, November 20, 1998
 *
 * Modifications:
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
 * Programmer:  Albert Cheng
 *              2002/04/22
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
h5_show_hostname(void)
{
    char  hostname[80];
#ifdef H5_HAVE_WIN32_API
    WSADATA wsaData;
    int err;
#endif

    /* try show the process or thread id in multiple processes cases*/
#ifdef H5_HAVE_PARALLEL
    {
        int mpi_rank, mpi_initialized, mpi_finalized;

        MPI_Initialized(&mpi_initialized);
        MPI_Finalized(&mpi_finalized);

        if(mpi_initialized && !mpi_finalized) {
            MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
            HDprintf("MPI-process %d.", mpi_rank);
        }
        else
            HDprintf("thread 0.");
    }
#elif defined(H5_HAVE_THREADSAFE)
    HDprintf("thread %lu.", HDpthread_self_ulong());
#else
    HDprintf("thread 0.");
#endif
#ifdef H5_HAVE_WIN32_API

    err = WSAStartup( MAKEWORD(2,2), &wsaData );
    if ( err != 0 ) {
        /* could not find a usable WinSock DLL */
        return;
    }

    /* Confirm that the WinSock DLL supports 2.2.*/
    /* Note that if the DLL supports versions greater    */
    /* than 2.2 in addition to 2.2, it will still return */
    /* 2.2 in wVersion since that is the version we      */
    /* requested.                                        */

    if ( LOBYTE( wsaData.wVersion ) != 2 ||
         HIBYTE( wsaData.wVersion ) != 2 ) {
        /* could not find a usable WinSock DLL */
        WSACleanup( );
        return;
    }

#endif
#ifdef H5_HAVE_GETHOSTNAME
    if (HDgethostname(hostname, (size_t)80) < 0)
        HDprintf(" gethostname failed\n");
    else
        HDprintf(" hostname=%s\n", hostname);
#else
    HDprintf(" gethostname not supported\n");
#endif
#ifdef H5_HAVE_WIN32_API
    WSACleanup();
#endif
}


#ifdef H5_HAVE_PARALLEL
/*
 * Function:    h5_set_info_object
 * Purpose:     Process environment variables setting to set up MPI Info
 *              object.
 * Return:      0 if all is fine; otherwise non-zero.
 * Programmer:  Albert Cheng, 2002/05/21.
 * Modifications:
 *          Bill Wendling, 2002/05/31
 *          Modified so that the HDF5_MPI_INFO environment variable can
 *          be a semicolon separated list of "key=value" pairings. Most
 *          of the code is to remove any whitespaces which might be
 *          surrounding the "key=value" pairs.
 */
int
h5_set_info_object(void)
{
    char  *envp;      /* environment pointer */
    int    ret_value=0;

    /* handle any MPI INFO hints via $HDF5_MPI_INFO */
    if ((envp = HDgetenv("HDF5_MPI_INFO")) != NULL){
        char *next, *valp;

        valp = envp = next = HDstrdup(envp);

        if (!valp) return 0;

        /* create an INFO object if not created yet */
        if (h5_io_info_g == MPI_INFO_NULL)
            MPI_Info_create(&h5_io_info_g);

        do {
            size_t len;
            char *key_val, *endp, *namep;

            if (*valp == ';')
                valp++;

            /* copy key/value pair into temporary buffer */
            len = strcspn(valp, ";");
            next = &valp[len];
            key_val = (char *)HDcalloc(1, len + 1);

            /* increment the next pointer past the terminating semicolon */
            if (*next == ';')
                ++next;

            namep = HDstrncpy(key_val, valp, len);

            /* pass up any beginning whitespaces */
            while (*namep && (*namep == ' ' || *namep == '\t'))
                namep++;

            if (!*namep) continue; /* was all white space, so move to next k/v pair */

            /* eat up any ending white spaces */
            endp = &namep[HDstrlen(namep) - 1];

            while (endp && (*endp == ' ' || *endp == '\t'))
                *endp-- = '\0';

            /* find the '=' */
            valp = HDstrchr(namep, '=');

            if (valp != NULL) {     /* it's a valid key/value pairing */
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
                    HDprintf("MPI_Info_set failed\n");
                    ret_value = -1;
                }
            }

            valp = next;
            HDfree(key_val);
        } while (next && *next);

        HDfree(envp);
    }

    return ret_value;
}


/*
 * Function:    h5_dump_info_object
 * Purpose:     Display content of an MPI Info object
 * Return:      void
 * Programmer:  Albert Cheng 2002/05/21
 * Modifications:
 */
void
h5_dump_info_object(MPI_Info info)
{
    char  key[MPI_MAX_INFO_KEY+1];
    char  value[MPI_MAX_INFO_VAL+1];
    int    flag;
    int    i, nkeys;

    HDprintf("Dumping MPI Info Object(%d) (up to %d bytes per item):\n", (int)info,
  MPI_MAX_INFO_VAL);
    if (info==MPI_INFO_NULL){
  HDprintf("object is MPI_INFO_NULL\n");
    }
    else {
  MPI_Info_get_nkeys(info, &nkeys);
  HDprintf("object has %d items\n", nkeys);
  for (i=0; i<nkeys; i++){
      MPI_Info_get_nthkey(info, i, key);
      MPI_Info_get(info, key, MPI_MAX_INFO_VAL, value, &flag);
      HDprintf("%s=%s\n", key, value);
  }

    }
}
#endif  /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 * Function:  h5_get_file_size
 *
 * Purpose:  Get the current size of a file (in bytes)
 *
 * Return:  Success:  Size of file in bytes
 *    Failure:  -1
 *
 * Programmer:  Quincey Koziol
 *              Saturday, March 22, 2003
 *
 *-------------------------------------------------------------------------
 */
/* Disable warning for "format not a string literal" here -QAK */
/*
 *      This pragma only needs to surround the snprintf() calls with
 *      temp in the code below, but early (4.4.7, at least) gcc only
 *      allows diagnostic pragmas to be toggled outside of functions.
 */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat-nonliteral"
h5_stat_size_t
h5_get_file_size(const char *filename, hid_t fapl)
{
    char temp[2048];    /* Temporary buffer for file names */
    h5_stat_t  sb;     /* Structure for querying file info */
    int j = 0;

    if(fapl == H5P_DEFAULT) {
        /* Get the file's statistics */
        if(0 == HDstat(filename, &sb))
            return((h5_stat_size_t)sb.st_size);
    } /* end if */
    else {
        hid_t  driver;         /* VFD used for file */

        /* Get the driver used when creating the file */
        if((driver = H5Pget_driver(fapl)) < 0)
            return(-1);

        /* Check for simple cases */
        if(driver == H5FD_SEC2 || driver == H5FD_STDIO || driver == H5FD_CORE ||
#ifdef H5_HAVE_WINDOWS
                driver == H5FD_WINDOWS ||
#endif /* H5_HAVE_WINDOWS */
#ifdef H5_HAVE_DIRECT
                driver == H5FD_DIRECT ||
#endif /* H5_HAVE_DIRECT */
                driver == H5FD_LOG) {
            /* Get the file's statistics */
            if(0 == HDstat(filename, &sb))
                return((h5_stat_size_t)sb.st_size);
        } /* end if */
        else if(driver == H5FD_MULTI) {
            H5FD_mem_t mt;
            h5_stat_size_t tot_size = 0;

            HDassert(HDstrlen(multi_letters) == H5FD_MEM_NTYPES);
            for(mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, mt)) {
                /* Create the filename to query */
                HDsnprintf(temp, sizeof temp, "%s-%c.h5", filename, multi_letters[mt]);

                /* Check for existence of file */
                if(0 == HDaccess(temp, F_OK)) {
                    /* Get the file's statistics */
                    if(0 != HDstat(temp, &sb))
                        return(-1);

                    /* Add to total size */
                    tot_size += (h5_stat_size_t)sb.st_size;
                } /* end if */
            } /* end for */

            /* Return total size */
            return(tot_size);
        } /* end if */
#ifdef H5_HAVE_PARALLEL
        else if(driver == H5FD_MPIO) {
            MPI_File fh;         /* MPI file handle used to open the file and verify its size */
            int mpi_ret;
            MPI_Offset file_size;

            mpi_ret = MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_RDONLY, MPI_INFO_NULL, &fh);
            if (mpi_ret != MPI_SUCCESS) return -1;
            mpi_ret = MPI_File_get_size(fh, &file_size);
            if (mpi_ret != MPI_SUCCESS) return -1;
            mpi_ret = MPI_File_close(&fh);
            if (mpi_ret != MPI_SUCCESS) return -1;

            return file_size;
        }
#endif /* H5_HAVE_PARALLEL */
        else if(driver == H5FD_FAMILY) {
            h5_stat_size_t tot_size = 0;

            /* Try all filenames possible, until we find one that's missing */
            for(j = 0; /*void*/; j++) {
                /* Create the filename to query */
                HDsnprintf(temp, sizeof temp, filename, j);

                /* Check for existence of file */
                if(HDaccess(temp, F_OK) < 0)
                    break;

                /* Get the file's statistics */
                if(0 != HDstat(temp, &sb))
                    return(-1);

                /* Add to total size */
                tot_size += (h5_stat_size_t)sb.st_size;
            } /* end for */

            /* Return total size */
            return(tot_size);
        } /* end if */
        else {
            HDassert(0 && "Unknown VFD!");
        } /* end else */
    } /* end else */

    return(-1);
} /* end get_file_size() */
#pragma GCC diagnostic pop

/*
 * This routine is designed to provide equivalent functionality to 'printf'
 * and allow easy replacement for environments which don't have stdin/stdout
 * available. (i.e. Windows & the Mac)
 */
int
print_func(const char *format, ...)
{
  va_list arglist;
  int ret_value;

  HDva_start(arglist, format);
  ret_value = HDvprintf(format, arglist);
  HDva_end(arglist);
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
 * Programmer:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int h5_szip_can_encode(void )
{
    unsigned int filter_config_flags;

    H5Zget_filter_info(H5Z_FILTER_SZIP, &filter_config_flags);
    if ((filter_config_flags &
            (H5Z_FILTER_CONFIG_ENCODE_ENABLED|H5Z_FILTER_CONFIG_DECODE_ENABLED)) == 0) {
        /* filter present but neither encode nor decode is supported (???) */
        return -1;
    } else if ((filter_config_flags &
            (H5Z_FILTER_CONFIG_ENCODE_ENABLED|H5Z_FILTER_CONFIG_DECODE_ENABLED)) ==
            H5Z_FILTER_CONFIG_DECODE_ENABLED) {
        /* decoder only: read but not write */
        return 0;
    } else if ((filter_config_flags &
            (H5Z_FILTER_CONFIG_ENCODE_ENABLED|H5Z_FILTER_CONFIG_DECODE_ENABLED)) ==
            H5Z_FILTER_CONFIG_ENCODE_ENABLED) {
        /* encoder only: write but not read (???) */
        return -1;
    } else if ((filter_config_flags &
            (H5Z_FILTER_CONFIG_ENCODE_ENABLED|H5Z_FILTER_CONFIG_DECODE_ENABLED)) ==
            (H5Z_FILTER_CONFIG_ENCODE_ENABLED|H5Z_FILTER_CONFIG_DECODE_ENABLED)) {
        return 1;
    }
   return(-1);
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
 *     NULL if the varialbe doesn't exist in task 'root's environment.
 *
 * Programmer:  Leon Arber
 *              4/4/05
 *
 * Modifications:
 *    Use original getenv if MPI is not initialized. This happens
 *    one uses the PHDF5 library to build a serial nature code.
 *    Albert 2006/04/07
 *
 *-------------------------------------------------------------------------
 */
char *
getenv_all(MPI_Comm comm, int root, const char* name)
{
    int mpi_size, mpi_rank, mpi_initialized, mpi_finalized;
    int len;
    static char* env = NULL;

    HDassert(name);

    MPI_Initialized(&mpi_initialized);
    MPI_Finalized(&mpi_finalized);

    if(mpi_initialized && !mpi_finalized) {
        MPI_Comm_rank(comm, &mpi_rank);
        MPI_Comm_size(comm, &mpi_size);
        HDassert(root < mpi_size);

        /* The root task does the getenv call
         * and sends the result to the other tasks */
        if(mpi_rank == root) {
            env = HDgetenv(name);
            if(env) {
                len = (int)HDstrlen(env);
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
            if(len >= 0) {
                if(env == NULL)
                    env = (char*) HDmalloc((size_t)len+1);
                else if(HDstrlen(env) < (size_t)len)
                    env = (char*) HDrealloc(env, (size_t)len+1);

                MPI_Bcast(env, len, MPI_CHAR, root, comm);
                env[len] = '\0';
            }
            else {
                if(env)
                    HDfree(env);
                env = NULL;
            }
        }
#ifndef NDEBUG
        MPI_Barrier(comm);
#endif
    }
    else {
        /* use original getenv */
        if(env)
            HDfree(env);
        env = HDgetenv(name);
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
 * Programmer:  Larry Knox
 *              Monday, October 13, 2009
 *
 *-------------------------------------------------------------------------
 */
int
h5_make_local_copy(const char *origfilename, const char *local_copy_name)
{
    int fd_old = (-1), fd_new = (-1);   /* File descriptors for copying data */
    ssize_t nread;                      /* Number of bytes read in */
    void  *buf = NULL;                  /* Buffer for copying data */
    const char *filename = H5_get_srcdir_filename(origfilename);       /* Get the test file name to copy */

    /* Allocate copy buffer */
    if(NULL == (buf = HDcalloc((size_t)1, (size_t)READ_BUF_SIZE)))
        goto error;

    /* Copy old file into temporary file */
    if((fd_old = HDopen(filename, O_RDONLY)) < 0)
        goto error;
    if((fd_new = HDopen(local_copy_name, O_RDWR|O_CREAT|O_TRUNC, H5_POSIX_CREATE_MODE_RW)) < 0)
        goto error;

    /* Copy data */
    while((nread = HDread(fd_old, buf, (size_t)READ_BUF_SIZE)) > 0)
        if(HDwrite(fd_new, buf, (size_t)nread) < 0)
            goto error;

    /* Close files */
    if(HDclose(fd_old) < 0)
        goto error;
    if(HDclose(fd_new) < 0)
        goto error;

    /* Release memory */
    HDfree(buf);

    return 0;

error:
    /* ignore return values since we're already noted the problem */
    if(fd_old > 0)
        HDclose(fd_old);
    if(fd_new > 0)
        HDclose(fd_new);
    HDfree(buf);
    return -1;
} /* end h5_make_local_copy() */


/*-------------------------------------------------------------------------
 * Function:    h5_verify_cached_stabs_cb
 *
 * Purpose:     Callback function for h5_verify_cached_stabs.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Neil Fortner
 *              Tuesday, April 12, 2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t
h5_verify_cached_stabs_cb(hid_t oid, const char H5_ATTR_UNUSED *name,
    const H5O_info_t *oinfo, void H5_ATTR_UNUSED *udata)
{
    if(oinfo->type == H5O_TYPE_GROUP)
        return H5G__verify_cached_stabs_test(oid);
    else
        return SUCCEED;
} /* end h5_verify_cached_stabs_cb() */


/*-------------------------------------------------------------------------
 * Function:    h5_verify_cached_stabs
 *
 * Purpose:     Verify that all groups in every file in base_name have
 *              their symbol table information cached (if present, and if
 *              the parent group also uses a symbol table).  Does not
 *              check that the root group's symbol table information is
 *              cached in the superblock.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              Tuesday, April 12, 2011
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
h5_verify_cached_stabs(const char *base_name[], hid_t fapl)
{
    hid_t       file = -1;
    char        filename[1024];
    int         i = 0;

    while(base_name[i]) {
        if (h5_fixname(base_name[i], fapl, filename, sizeof(filename)) == NULL)
            continue;

        H5E_BEGIN_TRY {
            file = H5Fopen(filename, H5F_ACC_RDONLY, fapl);
        } H5E_END_TRY
        if(file < 0) {
            i++;
            continue;
        } /* end if */

        if(H5Ovisit2(file, H5_INDEX_NAME, H5_ITER_NATIVE,
                h5_verify_cached_stabs_cb, NULL, H5O_INFO_BASIC) < 0)
            goto error;

        if(H5Fclose(file) < 0)
            goto error;
        file = -1;

        i++;
    } /* end while */

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;

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
 * Programmer:  Mike McGreevy
 *              August 18, 2010
 *
 *-------------------------------------------------------------------------
 */
void
h5_send_message(const char *send, const char *arg1, const char *arg2)
{
    FILE *signalfile = NULL;

    /* Create signal file (which will send signal to some other process) */
    signalfile = HDfopen(TMP_SIGNAL_FILE, "w+");

    /* Write messages to signal file, if provided */
    if(arg2 != NULL) {
        HDassert(arg1);
        HDfprintf(signalfile, "%s\n%s\n", arg1, arg2);
    } /* end if */
    else if(arg1 != NULL) {
        HDassert(arg2 == NULL);
        HDfprintf(signalfile, "%s\n", arg1);
    } /* end if */
    else {
        HDassert(arg1 == NULL);
        HDassert(arg2 == NULL);
    }/* end else */

    HDfclose(signalfile);

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
 *              This functon will then immediately remove the file (i.e.,
 *              to indicate that it has been received and can be reused),
 *              and then exits, allowing the calling function to continue.
 *
 * Return:      void
 *
 * Programmer:  Mike McGreevy
 *              August 18, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
h5_wait_message(const char *waitfor)
{
    FILE *returnfile;
    time_t t0,t1;

    /* Start timer. If this function runs for too long (i.e.,
        expected signal is never received), it will
        return failure */
    HDtime(&t0);

    /* Wait for return signal from some other process */
    while ((returnfile = HDfopen(waitfor, "r")) == NULL) {

        /* make note of current time. */
        HDtime(&t1);

        /* If we've been waiting for a signal for too long, then
            it was likely never sent and we should fail rather
            than loop infinitely */
        if(HDdifftime(t1, t0) > MESSAGE_TIMEOUT) {
            HDfprintf(stdout, "Error communicating between processes. Make sure test script is running.\n");
            TEST_ERROR;
        } /* end if */
    } /* end while */

    HDfclose(returnfile);
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
static H5FD_t *dummy_vfd_open(const char H5_ATTR_UNUSED *name, unsigned H5_ATTR_UNUSED flags, hid_t H5_ATTR_UNUSED fapl_id, haddr_t H5_ATTR_UNUSED maxaddr) { return NULL; }

static herr_t dummy_vfd_close(H5FD_t *_file);
static herr_t dummy_vfd_close(H5FD_t H5_ATTR_UNUSED *_file) { return FAIL; }

static haddr_t dummy_vfd_get_eoa(const H5FD_t *file, H5FD_mem_t type);
static haddr_t dummy_vfd_get_eoa(const H5FD_t H5_ATTR_UNUSED *file, H5FD_mem_t H5_ATTR_UNUSED type) { return HADDR_UNDEF; }

static herr_t dummy_vfd_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t addr);
static herr_t dummy_vfd_set_eoa(H5FD_t H5_ATTR_UNUSED *_file, H5FD_mem_t H5_ATTR_UNUSED type, haddr_t H5_ATTR_UNUSED addr) { return FAIL; }

static haddr_t dummy_vfd_get_eof(const H5FD_t *file, H5FD_mem_t type);
static haddr_t dummy_vfd_get_eof(const H5FD_t H5_ATTR_UNUSED *file, H5FD_mem_t H5_ATTR_UNUSED type) { return HADDR_UNDEF; }

static herr_t dummy_vfd_read(H5FD_t *_file, H5FD_mem_t type, hid_t fapl_id, haddr_t addr, size_t size, void *buf);
static herr_t dummy_vfd_read(H5FD_t H5_ATTR_UNUSED *_file, H5FD_mem_t H5_ATTR_UNUSED type, hid_t H5_ATTR_UNUSED fapl_id, haddr_t H5_ATTR_UNUSED addr, size_t H5_ATTR_UNUSED size, void H5_ATTR_UNUSED *buf) { return FAIL; }

static herr_t dummy_vfd_write(H5FD_t *_file, H5FD_mem_t type, hid_t fapl_id, haddr_t addr, size_t size, const void *buf);
static herr_t dummy_vfd_write(H5FD_t H5_ATTR_UNUSED *_file, H5FD_mem_t H5_ATTR_UNUSED type, hid_t H5_ATTR_UNUSED fapl_id, haddr_t H5_ATTR_UNUSED addr, size_t H5_ATTR_UNUSED size, const void H5_ATTR_UNUSED *buf) { return FAIL; }

/* Dummy VFD with the minimum parameters to make a VFD that can be registered */
static const H5FD_class_t H5FD_dummy_g = {
    "dummy",                    /* name         */
    1,                          /* maxaddr      */
    H5F_CLOSE_WEAK,             /* fc_degree    */
    NULL,                       /* terminate    */
    NULL,                       /* sb_size      */
    NULL,                       /* sb_encode    */
    NULL,                       /* sb_decode    */
    0,                          /* fapl_size    */
    NULL,                       /* fapl_get     */
    NULL,                       /* fapl_copy    */
    NULL,                       /* fapl_free    */
    0,                          /* dxpl_size    */
    NULL,                       /* dxpl_copy    */
    NULL,                       /* dxpl_free    */
    dummy_vfd_open,             /* open         */
    dummy_vfd_close,            /* close        */
    NULL,                       /* cmp          */
    NULL,                       /* query        */
    NULL,                       /* get_type_map */
    NULL,                       /* alloc        */
    NULL,                       /* free         */
    dummy_vfd_get_eoa,          /* get_eoa      */
    dummy_vfd_set_eoa,          /* set_eoa      */
    dummy_vfd_get_eof,          /* get_eof      */
    NULL,                       /* get_handle   */
    dummy_vfd_read,             /* read         */
    dummy_vfd_write,            /* write        */
    NULL,                       /* flush        */
    NULL,                       /* truncate     */
    NULL,                       /* lock         */
    NULL,                       /* unlock       */
    H5FD_FLMAP_DICHOTOMY    /* fl_map       */
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
    H5FD_class_t *vfd_class = NULL;     /* Dummy VFD that will be returned */

    /* Create the class and initialize everything to zero/NULL */
    if(NULL == (vfd_class = (H5FD_class_t *)HDmalloc(sizeof(H5FD_class_t))))
        TEST_ERROR;

    /* Copy the dummy VFD */
    HDmemcpy(vfd_class, &H5FD_dummy_g, sizeof(H5FD_class_t));

    return vfd_class;

error:
    if(vfd_class)
        HDfree(vfd_class);
    return NULL;
} /* h5_get_dummy_vfd_class */

/*-------------------------------------------------------------------------
 * Function:    h5_get_version_string
 *
 * Purpose:     Get the string that corresponds to the libvery version bound.
 *
 * Return:      The string
 *
 *-------------------------------------------------------------------------
 */
char *
h5_get_version_string(H5F_libver_t libver)
{
    return(LIBVER_NAMES[libver]);
} /* end of h5_get_version_string */
