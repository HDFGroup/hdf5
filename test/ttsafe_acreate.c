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
 * Testing for thread safety in H5A (dataset attribute) library
 * operations. -- Threaded program --
 * ------------------------------------------------------------------
 *
 * Plan: Attempt to break H5Acreate2 by making many simultaneous create
 *       calls.
 *
 * Claim: N calls to H5Acreate2 should create N attributes for a dataset
 *        if threadsafe. If some unprotected shared data exists for the
 *        dataset (eg, a count of the number of attributes in the
 *        dataset), there is a small chance that consecutive reads occur
 *        before a write to that shared variable.
 *
 ********************************************************************/

#include "ttsafe.h"

#ifdef H5_HAVE_THREADSAFE_API

#define FILENAME    "ttsafe_acreate.h5"
#define DATASETNAME "IntData"
#define NUM_THREADS 16

H5TS_THREAD_RETURN_TYPE tts_acreate_thread(void *);

typedef struct acreate_data_struct {
    hid_t dataset;
    hid_t datatype;
    hid_t dataspace;
    int   current_index;
} ttsafe_name_data_t;

void
tts_acreate(void H5_ATTR_UNUSED *params)
{
    /* Thread declarations */
    H5TS_thread_t threads[NUM_THREADS];

    /* HDF5 data declarations */
    hid_t   file      = H5I_INVALID_HID;
    hid_t   dataset   = H5I_INVALID_HID;
    hid_t   dataspace = H5I_INVALID_HID;
    hid_t   datatype  = H5I_INVALID_HID;
    hid_t   attribute = H5I_INVALID_HID;
    hsize_t dimsf[1]; /* dataset dimensions */

    /* data declarations */
    int    data; /* data to write */
    int    buffer, i;
    herr_t status;

    ttsafe_name_data_t *attrib_data[NUM_THREADS];

    char *attribute_name = NULL;

    memset(attrib_data, 0, sizeof(attrib_data));

    /*
     * Create an HDF5 file using H5F_ACC_TRUNC access, default file
     * creation plist and default file access plist
     */
    file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, H5I_INVALID_HID, "H5Fcreate");

    /* create a simple dataspace for the dataset */
    dimsf[0]  = 1;
    dataspace = H5Screate_simple(1, dimsf, NULL);
    CHECK(dataspace, H5I_INVALID_HID, "H5Screate_simple");

    /* define datatype for the data using native little endian integers */
    datatype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(datatype, H5I_INVALID_HID, "H5Tcopy");
    status = H5Tset_order(datatype, H5T_ORDER_LE);
    CHECK(status, FAIL, "H5Tset_order");

    /* create a new dataset within the file */
    dataset = H5Dcreate2(file, DATASETNAME, datatype, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* initialize data for dataset and write value to dataset */
    data   = NUM_THREADS;
    status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &data);
    CHECK(status, FAIL, "H5Dwrite");

    /*
     * Simultaneously create a large number of attributes to be associated
     * with the dataset
     */
    for (i = 0; i < NUM_THREADS; i++) {
        attrib_data[i]                = (ttsafe_name_data_t *)malloc(sizeof(ttsafe_name_data_t));
        attrib_data[i]->dataset       = dataset;
        attrib_data[i]->datatype      = datatype;
        attrib_data[i]->dataspace     = dataspace;
        attrib_data[i]->current_index = i;
        if (H5TS_thread_create(&threads[i], tts_acreate_thread, attrib_data[i]) < 0)
            TestErrPrintf("thread # %d did not start", i);
    }

    for (i = 0; i < NUM_THREADS; i++)
        if (H5TS_thread_join(threads[i], NULL) < 0)
            TestErrPrintf("thread %d failed to join", i);

    /* verify the correctness of the test */
    for (i = 0; i < NUM_THREADS; i++) {
        attribute_name = gen_name(i);

        attribute = H5Aopen(dataset, attribute_name, H5P_DEFAULT);
        CHECK(attribute, H5I_INVALID_HID, "H5Aopen");

        if (attribute < 0)
            TestErrPrintf("unable to open appropriate attribute.  Test failed!\n");
        else {
            status = H5Aread(attribute, H5T_NATIVE_INT, &buffer);
            CHECK(status, FAIL, "H5Aread");
            VERIFY(buffer, i, "data values don't match");

            status = H5Aclose(attribute);
            CHECK(status, FAIL, "H5Aclose");
        }

        free(attribute_name);
    }

    /* close remaining resources */
    status = H5Sclose(dataspace);
    CHECK(status, FAIL, "H5Sclose");
    status = H5Tclose(datatype);
    CHECK(status, FAIL, "H5Sclose");
    status = H5Dclose(dataset);
    CHECK(status, FAIL, "H5Dclose");
    status = H5Fclose(file);
    CHECK(status, FAIL, "H5Fclose");

    for (i = 0; i < NUM_THREADS; i++)
        if (attrib_data[i])
            free(attrib_data[i]);
} /* end tts_acreate() */

H5TS_THREAD_RETURN_TYPE
tts_acreate_thread(void *client_data)
{
    hid_t  attribute = H5I_INVALID_HID;
    char  *attribute_name;
    int   *attribute_data; /* data for attributes */
    herr_t status;

    ttsafe_name_data_t *attrib_data;

    attrib_data = (ttsafe_name_data_t *)client_data;

    /* Create attribute */
    attribute_name = gen_name(attrib_data->current_index);
    attribute      = H5Acreate2(attrib_data->dataset, attribute_name, attrib_data->datatype,
                                attrib_data->dataspace, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attribute, H5I_INVALID_HID, "H5Acreate2");

    /* Write data to the attribute */
    attribute_data  = (int *)malloc(sizeof(int));
    *attribute_data = attrib_data->current_index;
    status          = H5Awrite(attribute, H5T_NATIVE_INT, attribute_data);
    CHECK(status, FAIL, "H5Awrite");
    status = H5Aclose(attribute);
    CHECK(status, FAIL, "H5Aclose");

    free(attribute_data);
    free(attribute_name);
    return (H5TS_thread_ret_t)0;
} /* end tts_acreate_thread() */

void
cleanup_acreate(void H5_ATTR_UNUSED *params)
{
    if (GetTestCleanup()) {
        HDunlink(FILENAME);
    }
}

#endif /* H5_HAVE_THREADSAFE_API */
