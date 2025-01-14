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

/********************************************************************
 *
 * Testing thread safety in dataset creation in the HDF5 library
 * -------------------------------------------------------------
 *
 * Set of tests to run multiple threads so that each creates a different
 * dataset. This is likely to cause race-conditions if run in a non
 * threadsafe environment.
 *
 * Temporary files generated:
 *   ttsafe_dcreate.h5
 *
 ********************************************************************/
#include "ttsafe.h"

#ifdef H5_HAVE_THREADSAFE_API

#define FILENAME   "ttsafe_dcreate.h5"
#define NUM_THREAD 16

H5TS_THREAD_RETURN_TYPE tts_dcreate_creator(void *);

typedef struct thr_info {
    int         id;
    hid_t       file;
    const char *dsetname;
} thr_info;

/*
 * Set individual dataset names (rather than generated the names
 * automatically)
 */
const char *dsetname[NUM_THREAD] = {"zero",   "one",      "two",      "three",  "four", "five",
                                    "six",    "seven",    "eight",    "nine",   "ten",  "eleven",
                                    "twelve", "thirteen", "fourteen", "fifteen"};

thr_info thread_out[NUM_THREAD];

/*
 **********************************************************************
 * Thread safe test - multiple dataset creation
 **********************************************************************
 */
void
tts_dcreate(void H5_ATTR_UNUSED *params)
{
    /* thread definitions */
    H5TS_thread_t threads[NUM_THREAD];

    /* HDF5 data definitions */
    hid_t  file    = H5I_INVALID_HID;
    hid_t  dataset = H5I_INVALID_HID;
    int    datavalue, i;
    herr_t status;

    /*
     * Create a hdf5 file using H5F_ACC_TRUNC access, default file
     * creation plist and default file access plist
     */
    file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, H5I_INVALID_HID, "H5Fcreate");

    /* simultaneously create a large number of datasets within the file */
    for (i = 0; i < NUM_THREAD; i++) {
        thread_out[i].id       = i;
        thread_out[i].file     = file;
        thread_out[i].dsetname = dsetname[i];
        if (H5TS_thread_create(&threads[i], tts_dcreate_creator, &thread_out[i]) < 0)
            TestErrPrintf("thread # %d did not start", i);
    }

    for (i = 0; i < NUM_THREAD; i++)
        if (H5TS_thread_join(threads[i], NULL) < 0)
            TestErrPrintf("thread %d failed to join", i);

    /* compare data to see if it is written correctly */

    for (i = 0; i < NUM_THREAD; i++) {
        if ((dataset = H5Dopen2(file, dsetname[i], H5P_DEFAULT)) < 0) {
            TestErrPrintf("Dataset name not found - test failed\n");
            H5Fclose(file);
            return;
        }
        else {
            status = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &datavalue);
            CHECK(status, FAIL, "H5Dread");

            if (datavalue != i) {
                TestErrPrintf("Wrong value read %d for dataset name %s - test failed\n", datavalue,
                              dsetname[i]);
                status = H5Dclose(dataset);
                CHECK(status, FAIL, "H5Dclose");
                status = H5Fclose(file);
                CHECK(status, FAIL, "H5Fclose");
                return;
            }

            status = H5Dclose(dataset);
            CHECK(status, FAIL, "H5Dclose");
        }
    }

    /* close remaining resources */
    status = H5Fclose(file);
    CHECK(status, FAIL, "H5Fclose");
} /* end tts_dcreate() */

H5TS_THREAD_RETURN_TYPE
tts_dcreate_creator(void *_thread_data)
{
    hid_t           dataspace = H5I_INVALID_HID;
    hid_t           dataset   = H5I_INVALID_HID;
    herr_t          status;
    hsize_t         dimsf[1]; /* dataset dimensions */
    struct thr_info thread_data;

    memcpy(&thread_data, _thread_data, sizeof(struct thr_info));

    /* define dataspace for dataset */
    dimsf[0]  = 1;
    dataspace = H5Screate_simple(1, dimsf, NULL);
    CHECK(dataspace, H5I_INVALID_HID, "H5Screate_simple");

    /* create a new dataset within the file */
    dataset = H5Dcreate2(thread_data.file, thread_data.dsetname, H5T_NATIVE_INT, dataspace, H5P_DEFAULT,
                         H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* initialize data for dataset and write value to dataset */
    status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &thread_data.id);
    CHECK(status, FAIL, "H5Dwrite");

    /* close dataset and dataspace resources */
    status = H5Dclose(dataset);
    CHECK(status, FAIL, "H5Dclose");
    status = H5Sclose(dataspace);
    CHECK(status, FAIL, "H5Sclose");

    return (H5TS_thread_ret_t)0;
} /* end tts_dcreate_creator() */

void
cleanup_dcreate(void H5_ATTR_UNUSED *params)
{
    if (GetTestCleanup()) {
        HDunlink(FILENAME);
    }
}
#endif /* H5_HAVE_THREADSAFE_API */
