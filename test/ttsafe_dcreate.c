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
 * Created: Apr 28 2000
 * Programmer: Chee Wai LEE
 *
 ********************************************************************/
#include "ttsafe.h"

#ifdef H5_HAVE_THREADSAFE

#define FILENAME		"ttsafe_dcreate.h5"
#define NUM_THREAD		16

void *tts_dcreate_creator(void *);

typedef struct thread_info {
	int id;
	hid_t file;
	const char *dsetname;
} thread_info;

/*
 * Set individual dataset names (rather than generated the names
 * automatically)
 */
const char *dsetname[NUM_THREAD]={
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "ten",
    "eleven",
    "twelve",
    "thirteen",
    "fourteen",
    "fifteen"
};

thread_info thread_out[NUM_THREAD];

/*
 **********************************************************************
 * Thread safe test - multiple dataset creation
 **********************************************************************
 */
void
tts_dcreate(void)
{
    /* thread definitions */
    H5TS_thread_t threads[NUM_THREAD];

    /* HDF5 data definitions */
    hid_t   file        = H5I_INVALID_HID;
    hid_t   dataset     = H5I_INVALID_HID;
    int datavalue, i;
    H5TS_attr_t attribute;
    herr_t status;

    /* set pthread attribute to perform global scheduling */
    H5TS_attr_init(&attribute);

    /* set thread scope to system */
#ifdef H5_HAVE_SYSTEM_SCOPE_THREADS
    H5TS_attr_setscope(&attribute, H5TS_SCOPE_SYSTEM);
#endif /* H5_HAVE_SYSTEM_SCOPE_THREADS */

    /*
     * Create a hdf5 file using H5F_ACC_TRUNC access, default file
     * creation plist and default file access plist
     */
    file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, H5I_INVALID_HID, "H5Fcreate");

    /* simultaneously create a large number of datasets within the file */
    for(i = 0; i < NUM_THREAD; i++) {
        thread_out[i].id = i;
        thread_out[i].file = file;
        thread_out[i].dsetname = dsetname[i];
        threads[i] = H5TS_create_thread(tts_dcreate_creator, NULL, &thread_out[i]);
    }

    for(i = 0;i < NUM_THREAD; i++)
        H5TS_wait_for_thread(threads[i]);

    /* compare data to see if it is written correctly */

    for(i = 0; i < NUM_THREAD; i++) {
        if((dataset = H5Dopen2(file, dsetname[i], H5P_DEFAULT)) < 0) {
            TestErrPrintf("Dataset name not found - test failed\n");
            H5Fclose(file);
            return;
        } else {
            status = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &datavalue);
            CHECK(status, FAIL, "H5Dread");

            if(datavalue != i) {
                TestErrPrintf("Wrong value read %d for dataset name %s - test failed\n",
                            datavalue, dsetname[i]);
                status = H5Dclose(dataset);
                CHECK(status, FAIL, "H5Dclose");
                status = H5Fclose(file);
                CHECK(status, FAIL, "H5Fclose");
                return;
            }

            status= H5Dclose(dataset);
            CHECK(status, FAIL, "H5Dclose");
        }
    }

    /* close remaining resources */
    status = H5Fclose(file);
    CHECK(status, FAIL, "H5Fclose");

    /* Destroy the thread attribute */
    H5TS_attr_destroy(&attribute);
} /* end tts_dcreate() */

void *
tts_dcreate_creator(void *_thread_data)
{
    hid_t   dataspace   = H5I_INVALID_HID;
    hid_t   dataset     = H5I_INVALID_HID;
    herr_t  status;
    hsize_t dimsf[1]; /* dataset dimensions */
    struct thread_info thread_data;

    memcpy(&thread_data, _thread_data, sizeof(struct thread_info));

    /* define dataspace for dataset */
    dimsf[0] = 1;
    dataspace = H5Screate_simple(1, dimsf, NULL);
    CHECK(dataspace, H5I_INVALID_HID, "H5Screate_simple");

    /* create a new dataset within the file */
    dataset = H5Dcreate2(thread_data.file, thread_data.dsetname,
                        H5T_NATIVE_INT, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* initialize data for dataset and write value to dataset */
    status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
             H5P_DEFAULT, &thread_data.id);
    CHECK(status, FAIL, "H5Dwrite");

    /* close dataset and dataspace resources */
    status = H5Dclose(dataset);
    CHECK(status, FAIL, "H5Dclose");
    status = H5Sclose(dataspace);
    CHECK(status, FAIL, "H5Sclose");

    return NULL;
} /* end tts_dcreate_creator() */

void
cleanup_dcreate(void)
{
    HDunlink(FILENAME);
}
#endif /*H5_HAVE_THREADSAFE*/

