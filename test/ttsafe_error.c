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

/********************************************************************
 *
 * Testing thread safety. Deliberate per-thread errors to test error stack
 * -----------------------------------------------------------------------
 *
 * Create 16 multiple threads to create datasets with the same name. The
 * library should respond with 15 equivalent error stack printouts (one for
 * each bad thread). The final hdf5 file should be a valid file with one
 * entry.
 *
 * Temporary files generated:
 *
 *     ttsafe_error.h5
 *
 * Created: Apr 28 2000
 * Programmer: Chee Wai LEE
 *
 ********************************************************************/
#include "ttsafe.h"

#ifdef H5_HAVE_THREADSAFE

#define NUM_THREAD              16
#define FILENAME                "ttsafe_error.h5"

/* Having a common dataset name is an error */
#define DATASETNAME             "commonname"
#define EXPECTED_ERROR_DEPTH	7
#define WRITE_NUMBER            37

/* Typedefs */
typedef struct err_num_struct {
    hid_t maj_num;
    hid_t min_num;
} err_num_t;

/* Global variables */
hid_t               error_file_g    = H5I_INVALID_HID;
int                 error_flag_g    = 0;
int                 error_count_g   = 0;
err_num_t           expected_g[EXPECTED_ERROR_DEPTH];
H5TS_mutex_simple_t error_mutex_g;

/* Prototypes */
static herr_t error_callback(hid_t , void *);
static herr_t walk_error_callback(unsigned, const H5E_error2_t *, void *);
static void *tts_error_thread(void *);


void
tts_error(void)
{
    hid_t           dataset = H5I_INVALID_HID;
    H5TS_thread_t   threads[NUM_THREAD];
    H5TS_attr_t     attribute;
    int             value, i;
    herr_t          status;

    /* Must initialize these at runtime */
    expected_g[0].maj_num = H5E_DATASET;
    expected_g[0].min_num = H5E_CANTINIT;

    expected_g[1].maj_num = H5E_DATASET;
    expected_g[1].min_num = H5E_CANTINIT;

    expected_g[2].maj_num = H5E_LINK;
    expected_g[2].min_num = H5E_CANTINIT;

    expected_g[3].maj_num = H5E_LINK;
    expected_g[3].min_num = H5E_CANTINSERT;

    expected_g[4].maj_num = H5E_SYM;
    expected_g[4].min_num = H5E_NOTFOUND;

    expected_g[5].maj_num = H5E_SYM;
    expected_g[5].min_num = H5E_CALLBACK;

    expected_g[6].maj_num = H5E_LINK;
    expected_g[6].min_num = H5E_EXISTS;

    /* set up mutex for global count of errors */
    H5TS_mutex_init(&error_mutex_g);

    /* make thread scheduling global */
    H5TS_attr_init(&attribute);

#ifdef H5_HAVE_SYSTEM_SCOPE_THREADS
    /* set thread scope to system */
    H5TS_attr_setscope(&attribute, H5TS_SCOPE_SYSTEM);
#endif /* H5_HAVE_SYSTEM_SCOPE_THREADS */

    /* Create a hdf5 file using H5F_ACC_TRUNC access, default file
     * creation plist and default file access plist
     */
    error_file_g = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(error_file_g, H5I_INVALID_HID, "H5Fcreate");

    for (i = 0; i < NUM_THREAD; i++)
        threads[i] = H5TS_create_thread(tts_error_thread, &attribute, NULL);

    for (i = 0; i < NUM_THREAD; i++)
        H5TS_wait_for_thread(threads[i]);

    if (error_flag_g) {
        TestErrPrintf("At least one thread reported a value that was different from the exected value\n");
        HDprintf("(Update this test if the error stack changed!)\n");
    }

    if (error_count_g != NUM_THREAD - 1)
        TestErrPrintf("Error: %d threads failed instead of %d\n", error_count_g, NUM_THREAD-1);

    dataset = H5Dopen2(error_file_g, DATASETNAME, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dopen2");

    status = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &value);
    CHECK(status, FAIL, "H5Dread");

    if (value != WRITE_NUMBER)
        TestErrPrintf("Error: Successful thread wrote value %d instead of %d\n", value, WRITE_NUMBER);

    status = H5Dclose(dataset);
    CHECK(status, FAIL, "H5Dclose");
    status = H5Fclose(error_file_g);
    CHECK(status, FAIL, "H5Fclose");

    H5TS_attr_destroy(&attribute);
} /* end tts_error() */

static void *
tts_error_thread(void H5_ATTR_UNUSED *arg)
{
    hid_t   dataspace   = H5I_INVALID_HID;
    hid_t   datatype    = H5I_INVALID_HID;
    hid_t   dataset     = H5I_INVALID_HID;
    hsize_t dimsf[1]; /* dataset dimensions */
    H5E_auto2_t old_error_cb;
    void *old_error_client_data;
    int value;
    herr_t status;

    /* preserve previous error stack handler */
    status = H5Eget_auto2(H5E_DEFAULT, &old_error_cb, &old_error_client_data);
    CHECK(status, FAIL, "H5Eget_auto2");

    /* set each thread's error stack handler */
    status = H5Eset_auto2(H5E_DEFAULT, error_callback, NULL);
    CHECK(status, FAIL, "H5Eset_auto2");

    /* define dataspace for dataset */
    dimsf[0] = 1;
    dataspace = H5Screate_simple(1, dimsf, NULL);
    CHECK(dataspace, H5I_INVALID_HID, "H5Screate_simple");

    /* define datatype for the data using native little endian integers */
    datatype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(datatype, H5I_INVALID_HID, "H5Tcopy");
    status = H5Tset_order(datatype, H5T_ORDER_LE);
    CHECK(status, FAIL, "H5Tset_order");

    /* create a new dataset within the file */
    dataset = H5Dcreate2(error_file_g, DATASETNAME, datatype, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    /* Most of these will fail, so don't check the error here */
    if (dataset >= 0) {
        value = WRITE_NUMBER;
        status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &value);
        CHECK(status, FAIL, "H5Dwrite");
        status = H5Dclose(dataset);
        CHECK(status, FAIL, "H5Dclose");
    }

    status = H5Tclose(datatype);
    CHECK(status, FAIL, "H5Tclose");
    status = H5Sclose(dataspace);
    CHECK(status, FAIL, "H5Sclose");

    /* turn our error stack handler off */
    status = H5Eset_auto2(H5E_DEFAULT, old_error_cb, old_error_client_data);
    CHECK(status, FAIL, "H5Eset_auto2");

    return NULL;
} /* end tts_error_thread() */

static herr_t
error_callback(hid_t H5_ATTR_UNUSED estack_id, void *client_data)
{
    H5TS_mutex_lock_simple(&error_mutex_g);
    error_count_g++;
    H5TS_mutex_unlock_simple(&error_mutex_g);
    return H5Ewalk2(H5E_DEFAULT, H5E_WALK_DOWNWARD, walk_error_callback, client_data);
}

static herr_t
walk_error_callback(unsigned n, const H5E_error2_t *err_desc, void H5_ATTR_UNUSED *client_data)
{
    hid_t   maj_num = H5I_INVALID_HID;
    hid_t   min_num = H5I_INVALID_HID;

    if (err_desc) {
        maj_num = err_desc->maj_num;
        min_num = err_desc->min_num;

        if (n <= EXPECTED_ERROR_DEPTH && maj_num == expected_g[n].maj_num && min_num == expected_g[n].min_num)
            return SUCCEED;
    }

    error_flag_g = -1;
    return SUCCEED;
}

void
cleanup_error(void)
{
    HDunlink(FILENAME);
}

#endif /*H5_HAVE_THREADSAFE*/

