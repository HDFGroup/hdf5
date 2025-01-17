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
 * Testing thread safety. Thread Cancellation safety
 * -------------------------------------------------
 *
 * The main thread spawns a child to perform a series of dataset writes
 * to a hdf5 file. The main thread and child thread synchronizes within
 * a callback function called during a H5Diterate call after which the
 * main thread attempts to cancel the child thread.
 *
 * The cancellation should only work after the child thread has safely
 * left the H5Diterate call.
 *
 * Temporary files generated:
 *   ttsafe_cancel.h5
 *
 ********************************************************************/
#include "ttsafe.h"

#ifdef H5_HAVE_THREADSAFE_API
#ifdef H5_HAVE_PTHREAD_H

#define FILENAME    "ttsafe_cancel.h5"
#define DATASETNAME "commonname"

void  *tts_cancel_thread(void *);
herr_t tts_cancel_callback(void *, hid_t, unsigned, const hsize_t *, void *);
void   cancellation_cleanup(void *);

hid_t cancel_file;
typedef struct cleanup_struct {
    hid_t dataset;
    hid_t datatype;
    hid_t dataspace;
} cancel_cleanup_t;

/* Used by tts_cancel_thread.
 * Global because the thread gets cancelled and can't clean up its allocations */
cancel_cleanup_t cleanup_structure = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID};

pthread_t             childthread;
static H5TS_barrier_t barrier;

void
tts_cancel(void H5_ATTR_UNUSED *params)
{
    hid_t dataset;
    int   buffer;
    int   ret;

    /* Initialize barrier */
    ret = H5TS_barrier_init(&barrier, 2);
    CHECK_I(ret, "H5TS_barrier_init");

    /*
     * Create a hdf5 file using H5F_ACC_TRUNC access, default file
     * creation plist and default file access plist
     */
    cancel_file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    assert(cancel_file >= 0);
    ret = pthread_create(&childthread, NULL, tts_cancel_thread, NULL);
    assert(ret == 0);
    ret = H5TS_barrier_wait(&barrier);
    assert(ret == 0);
    ret = pthread_cancel(childthread);
    assert(ret == 0);

    dataset = H5Dopen2(cancel_file, DATASETNAME, H5P_DEFAULT);
    assert(dataset >= 0);
    ret = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &buffer);
    assert(ret >= 0);

    if (buffer != 11)
        TestErrPrintf("operation unsuccessful with value at %d instead of 11\n", buffer);

    ret = H5Dclose(dataset);
    assert(ret >= 0);
    ret = H5Fclose(cancel_file);
    assert(ret >= 0);

    ret = H5TS_barrier_destroy(&barrier);
    CHECK_I(ret, "H5TS_barrier_destroy");
} /* end tts_cancel() */

void *
tts_cancel_thread(void H5_ATTR_UNUSED *arg)
{
    hid_t   dataspace = H5I_INVALID_HID;
    hid_t   datatype  = H5I_INVALID_HID;
    hid_t   dataset   = H5I_INVALID_HID;
    int     datavalue;
    int     buffer;
    hsize_t dimsf[1]; /* dataset dimensions */
    herr_t  status;

    /* define dataspace for dataset */
    dimsf[0]  = 1;
    dataspace = H5Screate_simple(1, dimsf, NULL);
    CHECK(dataspace, H5I_INVALID_HID, "H5Screate_simple");

    /* define datatype for the data using native little endian integers */
    datatype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(datatype, H5I_INVALID_HID, "H5Tcopy");
    status = H5Tset_order(datatype, H5T_ORDER_LE);
    CHECK(status, FAIL, "H5Tset_order");

    /* create a new dataset within the file */
    dataset =
        H5Dcreate2(cancel_file, DATASETNAME, datatype, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* If thread is cancelled, make cleanup call */
    cleanup_structure.dataset   = dataset;
    cleanup_structure.datatype  = datatype;
    cleanup_structure.dataspace = dataspace;
    pthread_cleanup_push(cancellation_cleanup, &cleanup_structure);

    datavalue = 1;
    status    = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &datavalue);
    CHECK(status, FAIL, "H5Dwrite");

    status = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &buffer);
    CHECK(status, FAIL, "H5Dread");
    status = H5Diterate(&buffer, H5T_NATIVE_INT, dataspace, tts_cancel_callback, &dataset);
    CHECK(status, FAIL, "H5Diterate");

    HDsleep(3);

    datavalue = 100;
    status    = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &datavalue);
    CHECK(status, FAIL, "H5Dwrite");

    status = H5Dclose(dataset);
    CHECK(status, FAIL, "H5Dclose");
    status = H5Tclose(datatype);
    CHECK(status, FAIL, "H5Tclose");
    status = H5Sclose(dataspace);
    CHECK(status, FAIL, "H5Sclose");

    /*
     * Required by pthreads. The argument 0 pops the stack but does not
     * execute the cleanup routine.
     */
    pthread_cleanup_pop(0);

    return NULL;
} /* end tts_cancel_thread() */

herr_t
tts_cancel_callback(void *elem, hid_t H5_ATTR_UNUSED type_id, unsigned H5_ATTR_UNUSED ndim,
                    const hsize_t H5_ATTR_UNUSED *point, void *operator_data)
{
    hid_t  dataset = *(hid_t *)operator_data;
    int    value   = *(int *)elem;
    herr_t status;

    status = H5TS_barrier_wait(&barrier);
    CHECK_I(status, "H5TS_barrier_wait");

    HDsleep(3);

    if (value != 1) {
        TestErrPrintf("Error! Element value should be 1 and not %d\n", value);
        return FAIL;
    }

    value += 10;
    status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &value);
    CHECK(status, FAIL, "H5Dwrite");

    return SUCCEED;
} /* end tts_cancel_callback() */

/*
 * Need to perform the dataset, datatype and dataspace close that was never
 * performed because of thread cancellation
 */
void
cancellation_cleanup(void *arg)
{
    cancel_cleanup_t *_cleanup_structure = (cancel_cleanup_t *)arg;
    herr_t            status;

    status = H5Dclose(_cleanup_structure->dataset);
    CHECK(status, FAIL, "H5Dclose");
    status = H5Tclose(_cleanup_structure->datatype);
    CHECK(status, FAIL, "H5Tclose");
    status = H5Sclose(_cleanup_structure->dataspace);
    CHECK(status, FAIL, "H5Sclose");
} /* end cancellation_cleanup() */

void
cleanup_cancel(void H5_ATTR_UNUSED *params)
{
    if (GetTestCleanup()) {
        HDunlink(FILENAME);
    }
}

#endif /* H5_HAVE_PTHREAD_H */
#endif /* H5_HAVE_THREADSAFE_API */
