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
   FILE
   testhdf5.c - HDF5 testing framework main file.

   REMARKS
   General test wrapper for HDF5 base library test programs

   DESIGN
   Each test function should be implemented as function having no
   parameters and returning void (i.e. no return value).  They should be put
   into the list of AddTest() calls in main() below.  Functions which depend
   on other functionality should be placed below the AddTest() call for the
   base functionality testing.
   Each test module should include testhdf5.h and define a unique set of
   names for test files they create.

   BUGS/LIMITATIONS


 */

/* ANY new test needs to have a prototype in testhdf5.h */
#include "testhdf5.h"

int nerrors = 0;

char *paraprefix = NULL; /* for command line option para-prefix */

/* Length of multi-file VFD filename buffers */
#define H5TEST_MULTI_FILENAME_LEN 1024

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

    HDva_start(arglist, format);
    ret_value = HDvprintf(format, arglist);
    HDva_end(arglist);
    return ret_value;
}

/*
 * This routine is designed to provide equivalent functionality to 'printf'
 * and also increment the error count for the testing framework.
 */
int
TestErrPrintf(const char *format, ...)
{
    va_list arglist;
    int     ret_value;

    /* Increment the error count */
    nerrors++;

    /* Print the requested information */
    HDva_start(arglist, format);
    ret_value = HDvprintf(format, arglist);
    HDva_end(arglist);

    /* Return the length of the string produced (like printf() does) */
    return ret_value;
}

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
getenv_all(MPI_Comm comm, int root, const char *name)
{
    int          mpi_size, mpi_rank, mpi_initialized, mpi_finalized;
    int          len;
    static char *env = NULL;

    HDassert(name);

    MPI_Initialized(&mpi_initialized);
    MPI_Finalized(&mpi_finalized);

    if (mpi_initialized && !mpi_finalized) {
        MPI_Comm_rank(comm, &mpi_rank);
        MPI_Comm_size(comm, &mpi_size);
        HDassert(root < mpi_size);

        /* The root task does the getenv call
         * and sends the result to the other tasks */
        if (mpi_rank == root) {
            env = HDgetenv(name);
            if (env) {
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
            if (len >= 0) {
                if (env == NULL)
                    env = (char *)HDmalloc((size_t)len + 1);
                else if (HDstrlen(env) < (size_t)len)
                    env = (char *)HDrealloc(env, (size_t)len + 1);

                MPI_Bcast(env, len, MPI_CHAR, root, comm);
                env[len] = '\0';
            }
            else {
                if (env)
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
        if (env)
            HDfree(env);
        env = HDgetenv(name);
    } /* end if */

    return env;
}

#endif

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
 * Programmer:  Robb Matzke
 *              Thursday, November 19, 1998
 *
 *-------------------------------------------------------------------------
 */
hid_t
h5_fileaccess(void)
{
    hid_t fapl_id = H5I_INVALID_HID;

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;

    /* Finally, check for libver bounds */
    if (h5_get_libver_fapl(fapl_id) < 0)
        goto error;

    return fapl_id;

error:
    if (fapl_id != H5I_INVALID_HID)
        H5Pclose(fapl_id);
    return H5I_INVALID_HID;
} /* end h5_fileaccess() */

/*-------------------------------------------------------------------------
 * Function:    h5_get_libver_fapl
 *
 * Purpose:     Sets the library version bounds for a FAPL according to the
 *              value in the constant or environment variable "HDF5_LIBVER_BOUNDS".
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 * Programmer:  Quincey Koziol
 *              November 2018
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
    env = HDgetenv("HDF5_LIBVER_BOUNDS");
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
    HDstrncpy(buf, env, sizeof(buf));
    buf[sizeof(buf) - 1] = '\0';
    if (NULL == (tok = HDstrtok_r(buf, " \t\n\r", &lasts)))
        goto done;

    if (!HDstrcmp(tok, "latest")) {
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

#ifndef HDF5_PARAPREFIX
#define HDF5_PARAPREFIX ""
#endif
static char *
h5_fixname_real(const char *base_name, hid_t fapl, const char *_suffix, char *fullname, size_t size,
                hbool_t nest_printf, hbool_t subst_for_superblock)
{
    const char *prefix         = NULL;
    const char *driver_env_var = NULL; /* HDF5_DRIVER environment variable     */
    char       *ptr, last = '\0';
    const char *suffix = _suffix;
    size_t      i, j;
    hid_t       driver     = -1;
    int         isppdriver = 0; /* if the driver is MPI parallel */

    if (!base_name || !fullname || size < 1)
        return NULL;

    HDmemset(fullname, 0, size);

    /* Determine if driver is set by environment variable. If it is,
     * only generate a suffix if fixing the filename for the superblock
     * file. */
    driver_env_var = HDgetenv(HDF5_DRIVER);
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
                else
                    suffix = nest_printf ? "-%%06d.h5" : "-%06d.h5";
            }
            else if (H5FD_MULTI == driver) {

                /* Check the HDF5_DRIVER environment variable in case
                 * we are using the split driver since both of those
                 * use the multi VFD under the hood.
                 */
                if (driver_env_var && !HDstrcmp(driver_env_var, "split")) {
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

    /* Must first check fapl is not H5P_DEFAULT (-1) because H5FD_XXX
     * could be of value -1 if it is not defined.
     */
    isppdriver = ((H5P_DEFAULT != fapl) || driver_env_var) && (H5FD_MPIO == driver);
#if 0
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
        if (HDgetenv(HDF5_NOCLEANUP))
            SetTestNoCleanup();
    }
#endif
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
#endif /* HDF5_PARAPREFIX */
        }
#endif /* H5_HAVE_PARALLEL */
    }
    else {
        /*
         * For serial:
         *      First use the environment variable, then try the constant
         */
        prefix = HDgetenv("HDF5_PREFIX");

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

            if (!HDstrcmp(prefix, HDF5_PARAPREFIX)) {
                /*
                 * If the prefix specifies the HDF5_PARAPREFIX directory, then
                 * default to using the "/tmp/$USER" or "/tmp/$LOGIN"
                 * directory instead.
                 */
                char *user, *login;

                user   = HDgetenv("USER");
                login  = HDgetenv("LOGIN");
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
                fullname[size - 1] = '\0';
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
            }
            else {
                /* Buffer is too small */
                return NULL;
            }
        }
        else {
            if (HDsnprintf(fullname, size, "%s/%s", prefix, base_name) == (int)size)
                /* Buffer is too small */
                return NULL;
        }
    }
    else if (HDstrlen(base_name) >= size) {
        /* Buffer is too small */
        return NULL;
    }
    else {
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

char *
h5_fixname(const char *base_name, hid_t fapl, char *fullname, size_t size)
{
    return (h5_fixname_real(base_name, fapl, ".h5", fullname, size, FALSE, FALSE));
}

char *
h5_fixname_superblock(const char *base_name, hid_t fapl_id, char *fullname, size_t size)
{
    return (h5_fixname_real(base_name, fapl_id, ".h5", fullname, size, FALSE, TRUE));
}

hbool_t
h5_using_default_driver(const char *drv_name)
{
    hbool_t ret_val = TRUE;

    HDassert(H5_DEFAULT_VFD == H5FD_SEC2);

    if (!drv_name)
        drv_name = HDgetenv(HDF5_DRIVER);

    if (drv_name)
        return (!HDstrcmp(drv_name, "sec2") || !HDstrcmp(drv_name, "nomatch"));

    return ret_val;
}

herr_t
h5_driver_is_default_vfd_compatible(hid_t fapl_id, hbool_t *default_vfd_compatible)
{
    unsigned long feat_flags = 0;
    hid_t         driver_id  = H5I_INVALID_HID;
    herr_t        ret_value  = SUCCEED;

    HDassert(fapl_id >= 0);
    HDassert(default_vfd_compatible);

    if (fapl_id == H5P_DEFAULT)
        fapl_id = H5P_FILE_ACCESS_DEFAULT;

    if ((driver_id = H5Pget_driver(fapl_id)) < 0)
        return FAIL;

    if (H5FDdriver_query(driver_id, &feat_flags) < 0)
        return FAIL;

    *default_vfd_compatible = (feat_flags & H5FD_FEAT_DEFAULT_VFD_COMPATIBLE);

    return ret_value;
} /* end h5_driver_is_default_vfd_compatible() */

int
main(int argc, char *argv[])
{
#if defined(H5_PARALLEL_TEST)
    MPI_Init(&argc, &argv);
#else
    (void)argc;
    (void)argv;
#endif

    HDprintf("===================================\n");
    HDprintf("HDF5 TESTS START\n");
    HDprintf("===================================\n");

    /* Initialize testing framework */
    /* TestInit(argv[0], NULL, NULL); */

    /* Tests are generally arranged from least to most complexity... */
    /* AddTest("config", test_configure, cleanup_configure, "Configure definitions", NULL); */
    HDprintf("** CONFIGURE DEFINITIONS **\n");
    test_configure();
    HDprintf("\n");

    /* AddTest("metadata", test_metadata, cleanup_metadata, "Encoding/decoding metadata", NULL); */

    /* AddTest("checksum", test_checksum, cleanup_checksum, "Checksum algorithm", NULL); */
    HDprintf("** CHECKSUM ALGORITHM **\n");
    test_checksum();
    HDprintf("\n");

    /* AddTest("tst", test_tst, NULL,  "Ternary Search Trees", NULL); */

    /* AddTest("heap", test_heap, NULL,  "Memory Heaps", NULL); */

    /* AddTest("skiplist", test_skiplist, NULL,  "Skip Lists", NULL); */

    /* AddTest("refstr", test_refstr, NULL,  "Reference Counted Strings", NULL); */

    /* AddTest("file", test_file, cleanup_file, "Low-Level File I/O", NULL); */
    HDprintf("** LOW-LEVEL FILE I/O **\n");
    test_file();
    HDprintf("\n");

    /* AddTest("objects", test_h5o, cleanup_h5o, "Generic Object Functions", NULL); */
    HDprintf("** GENERIC OBJECT FUNCTIONS **\n");
    test_h5o();
    HDprintf("\n");

    /* AddTest("h5s",  test_h5s,  cleanup_h5s,  "Dataspaces", NULL); */
    HDprintf("** DATASPACES **\n");
    test_h5s();
    HDprintf("\n");

    /* AddTest("coords",  test_coords,  cleanup_coords,  "Dataspace coordinates", NULL); */
    HDprintf("** DATASPACE COORDINATES **\n");
    test_coords();
    HDprintf("\n");

    /* AddTest("sohm", test_sohm, cleanup_sohm,  "Shared Object Header Messages", NULL); */

    /* AddTest("attr", test_attr, cleanup_attr,  "Attributes", NULL); */
    HDprintf("** ATTRIBUTES **\n");
    test_attr();
    HDprintf("\n");

    /* AddTest("select", test_select, cleanup_select,  "Selections", NULL); */
    HDprintf("** SELECTIONS **\n");
    test_select();
    HDprintf("\n");

    /* AddTest("time", test_time, cleanup_time,  "Time Datatypes", NULL); */
    HDprintf("** TIME DATATYPES**\n");
    test_time();
    HDprintf("\n");

    /* AddTest("ref_deprec", test_reference_deprec, cleanup_reference_deprec,  "Deprecated References", NULL);
     */

    /* AddTest("ref", test_reference, cleanup_reference,  "References", NULL); */
    HDprintf("** REFERENCES **\n");
    test_reference();
    HDprintf("\n");

    /* AddTest("vltypes", test_vltypes, cleanup_vltypes,  "Variable-Length Datatypes", NULL); */
    HDprintf("** VARIABLE-LENGTH DATATYPES **\n");
    test_vltypes();
    HDprintf("\n");

    /* AddTest("vlstrings", test_vlstrings, cleanup_vlstrings,  "Variable-Length Strings", NULL); */
    HDprintf("** VARIABLE-LENGTH STRINGS **\n");
    test_vlstrings();
    HDprintf("\n");

    /* AddTest("iterate", test_iterate, cleanup_iterate,  "Group & Attribute Iteration", NULL); */
    HDprintf("** GROUP & ATTRIBUTE ITERATION **\n");
    test_iterate();
    HDprintf("\n");

    /* AddTest("array", test_array, cleanup_array,  "Array Datatypes", NULL); */
    HDprintf("** ARRAY DATATYPES **\n");
    test_array();
    HDprintf("\n");

    /* AddTest("genprop", test_genprop, cleanup_genprop,  "Generic Properties", NULL); */
    HDprintf("** GENERIC PROPERTIES **\n");
    test_genprop();
    HDprintf("\n");

    /* AddTest("unicode", test_unicode, cleanup_unicode,  "UTF-8 Encoding", NULL); */
    HDprintf("** UTF-8 ENCODING **\n");
    test_unicode();
    HDprintf("\n");

    /* AddTest("id", test_ids, NULL,  "User-Created Identifiers", NULL); */
    HDprintf("** USER-CREATED IDENTIFIERS **\n");
    test_ids();
    HDprintf("\n");

    /* AddTest("misc", test_misc, cleanup_misc,  "Miscellaneous", NULL); */
    HDprintf("** MISCELLANEOUS **\n");
    test_misc();
    HDprintf("\n");

    /* Display testing information */
    /* TestInfo(argv[0]); */

    /* Parse command line arguments */
    /* TestParseCmdLine(argc,argv); */

    /* Perform requested testing */
    /* PerformTests(); */

    /* Display test summary, if requested */
    /* if (GetTestSummary())
        TestSummary(); */

    /* Clean up test files, if allowed */
    if (/* GetTestCleanup() && */ !getenv("HDF5_NOCLEANUP")) {
        /* TestCleanup(); */

        HDprintf("TEST CLEANUP\n");

        H5E_BEGIN_TRY
        cleanup_configure();
        cleanup_checksum();
        cleanup_file();
        cleanup_h5o();
        cleanup_h5s();
        cleanup_coords();
        cleanup_attr();
        cleanup_select();
        cleanup_time();
        cleanup_reference();
        cleanup_vltypes();
        cleanup_vlstrings();
        cleanup_iterate();
        cleanup_array();
        cleanup_genprop();
        cleanup_unicode();
        cleanup_misc();
        H5E_END_TRY;

        HDprintf("\n");
    }

    /* Release test infrastructure */
    /* TestShutdown(); */

    /* Exit failure if errors encountered; else exit success. */
    /* No need to print anything since PerformTests() already does. */
    if (nerrors /* GetTestNumErrs() */ > 0) {
        HDprintf("** HDF5 tests failed with %d errors **\n", nerrors);
        HDexit(EXIT_FAILURE);
    }
    else {
        HDprintf("** HDF5 tests ran successfully **\n");
        HDexit(EXIT_SUCCESS);
    }
} /* end main() */
