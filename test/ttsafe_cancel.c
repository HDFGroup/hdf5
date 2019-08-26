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
 * Testing thread safety. Thread Cancellation safety
 * -------------------------------------------------
 *
 * The main thread spawns a child to perform a series of dataset writes
 * to a hdf5 file. The main thread and child thread synchronizes within
 * a callback function called during a H5Diterate call afterwhich the
 * main thread attempts to cancel the child thread.
 *
 * The cancellation should only work after the child thread has safely
 * left the H5Diterate call.
 *
 * Temporary files generated:
 *   ttsafe_cancel.h5
 *
 * Created: May 15 2000
 * Programmer: Chee Wai LEE
 *
 ********************************************************************/
#include "ttsafe.h"

#ifdef H5_HAVE_THREADSAFE
#ifndef H5_HAVE_WIN_THREADS

#define FILENAME	"ttsafe_cancel.h5"
#define DATASETNAME	"commonname"

void *tts_cancel_thread(void *);
void tts_cancel_barrier(void);
herr_t tts_cancel_callback(void *, hid_t, unsigned , const hsize_t *, void *);
void cancellation_cleanup(void *);

hid_t cancel_file;
typedef struct cleanup_struct {
	hid_t dataset;
	hid_t datatype;
	hid_t dataspace;
} cancel_cleanup_t;

pthread_t childthread;
pthread_mutex_t mutex;
pthread_cond_t cond;

void
tts_cancel(void)
{
    pthread_attr_t attribute;
    hid_t dataset;
    int buffer;
    int ret;

    /* make thread scheduling global */
    ret=pthread_attr_init(&attribute);
    assert(ret==0);
#ifdef H5_HAVE_SYSTEM_SCOPE_THREADS
    ret=pthread_attr_setscope(&attribute, PTHREAD_SCOPE_SYSTEM);
    assert(ret==0);
#endif /* H5_HAVE_SYSTEM_SCOPE_THREADS */


    /* Initialize mutex & condition variables */
    ret=pthread_mutex_init(&mutex,NULL);
    assert(ret==0);
    ret=pthread_cond_init(&cond,NULL);
    assert(ret==0);

    /*
     * Create a hdf5 file using H5F_ACC_TRUNC access, default file
     * creation plist and default file access plist
     */
    cancel_file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    assert(cancel_file>=0);
    ret=pthread_create(&childthread, &attribute, tts_cancel_thread, NULL);
    assert(ret==0);
    tts_cancel_barrier();
    ret=pthread_cancel(childthread);
    assert(ret==0);

    dataset = H5Dopen2(cancel_file, DATASETNAME, H5P_DEFAULT);
    assert(dataset>=0);
    ret=H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &buffer);
    assert(ret>=0);

    if (buffer != 11)
        TestErrPrintf("operation unsuccessful with value at %d instead of 11\n", buffer);

    ret=H5Dclose(dataset);
    assert(ret>=0);
    ret=H5Fclose(cancel_file);
    assert(ret>=0);

    /* Destroy the thread attribute */
    ret=pthread_attr_destroy(&attribute);
    assert(ret==0);
} /* end tts_cancel() */

void *
tts_cancel_thread(void H5_ATTR_UNUSED *arg)
{
    hid_t   dataspace   = H5I_INVALID_HID;
    hid_t   datatype    = H5I_INVALID_HID;
    hid_t   dataset     = H5I_INVALID_HID;
    int datavalue;
    int buffer;
    hsize_t dimsf[1];	/* dataset dimensions */
    cancel_cleanup_t *cleanup_structure;
    herr_t status;

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
    dataset = H5Dcreate2(cancel_file, DATASETNAME, datatype, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* If thread is cancelled, make cleanup call */
    cleanup_structure = (cancel_cleanup_t*)HDmalloc(sizeof(cancel_cleanup_t));
    cleanup_structure->dataset = dataset;
    cleanup_structure->datatype = datatype;
    cleanup_structure->dataspace = dataspace;
    pthread_cleanup_push(cancellation_cleanup, cleanup_structure);


    datavalue = 1;
    status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &datavalue);
    CHECK(status, FAIL, "H5Dwrite");

    status = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &buffer);
    CHECK(status, FAIL, "H5Dread");
    status = H5Diterate(&buffer, H5T_NATIVE_INT, dataspace, tts_cancel_callback, &dataset);
    CHECK(status, FAIL, "H5Diterate");

    HDsleep(3);

    datavalue = 100;
    status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &datavalue);
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
    hid_t dataset = *(hid_t *)operator_data;
    int value = *(int *)elem;
    herr_t status;

    tts_cancel_barrier();
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
    cancel_cleanup_t *cleanup_structure = (cancel_cleanup_t *)arg;
    herr_t status;

    status = H5Dclose(cleanup_structure->dataset);
    CHECK(status, FAIL, "H5Dclose");
    status = H5Tclose(cleanup_structure->datatype);
    CHECK(status, FAIL, "H5Tclose");
    status = H5Sclose(cleanup_structure->dataspace);
    CHECK(status, FAIL, "H5Sclose");

    /* retained for debugging */
    /*  print_func("cancellation noted, cleaning up ... \n"); */
} /* end cancellation_cleanup() */

/*
 * Artificial (and specific to this test) barrier to keep track of whether
 * both the main and child threads have reached a point in the program.
 */
void
tts_cancel_barrier(void)
{
    static int count = 2;
    int status;

    status = pthread_mutex_lock(&mutex);
    VERIFY(status, 0, "pthread_mutex_lock");

    if (count != 1) {
        count--;
        status = pthread_cond_wait(&cond, &mutex);
        VERIFY(status, 0, "pthread_cond_wait");
    }
    else {
        status = pthread_cond_signal(&cond);
        VERIFY(status, 0, "pthread_cond_signal");
    }

    status = pthread_mutex_unlock(&mutex);
    VERIFY(status, 0, "pthread_mutex_unlock");
} /* end tts_cancel_barrier() */

void cleanup_cancel(void)
{
    HDunlink(FILENAME);
}

#endif /*H5_HAVE_WIN_THREADS*/
#endif /*H5_HAVE_THREADSAFE*/

