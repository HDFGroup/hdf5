/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
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
 * HDF5 APIs exercised in thread:
 * H5Screate_simple, H5Tcopy, H5Tset_order, H5Dcreate, H5Dclose,
 * H5Dwrite, H5Dread, H5Diterate, H5Tclose, H5Sclose.
 *
 * Created: May 15 2000
 * Programmer: Chee Wai LEE
 *
 * Modification History
 * --------------------
 *
 *	19 May 2000, Bill Wendling
 *	Changed so that it creates its own HDF5 file and removes it at cleanup
 *	time.
 *
 ********************************************************************/
#include "ttsafe.h"

#ifdef H5_HAVE_THREADSAFE

#define FILENAME	"ttsafe_cancel.h5"
#define DATASETNAME	"commonname"

void *tts_cancel_thread(void *);
void tts_cancel_barrier(void);
herr_t tts_cancel_callback(void *, hid_t, unsigned, const hsize_t *, void *);
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

void tts_cancel(void)
{
    pthread_attr_t attribute;
    hid_t dataset;
    int buffer;

    /* make thread scheduling global */
    pthread_attr_init(&attribute);
#ifdef H5_HAVE_SYSTEM_SCOPE_THREADS
    pthread_attr_setscope(&attribute, PTHREAD_SCOPE_SYSTEM);
#endif /* H5_HAVE_SYSTEM_SCOPE_THREADS */

    /*
     * Create a hdf5 file using H5F_ACC_TRUNC access, default file
     * creation plist and default file access plist
     */
    cancel_file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    pthread_create(&childthread, &attribute, tts_cancel_thread, NULL);
    tts_cancel_barrier();
    pthread_cancel(childthread);

    dataset = H5Dopen(cancel_file, DATASETNAME);
    H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &buffer);

    if (buffer != 11)
        TestErrPrintf("operation unsuccessful with value at %d instead of 11\n", buffer);

    H5Dclose(dataset);
    H5Fclose(cancel_file);

    /* Destroy the thread attribute */
    pthread_attr_destroy(&attribute);
}

void *tts_cancel_thread(void UNUSED *arg)
{
    int datavalue;
    int *buffer;
    hid_t dataspace, datatype, dataset;
    hsize_t dimsf[1];	/* dataset dimensions */
    cancel_cleanup_t *cleanup_structure;

    /* define dataspace for dataset */
    dimsf[0] = 1;
    dataspace = H5Screate_simple(1,dimsf,NULL);

    /* define datatype for the data using native little endian integers */
    datatype = H5Tcopy(H5T_NATIVE_INT);
    H5Tset_order(datatype, H5T_ORDER_LE);

    /* create a new dataset within the file */
    dataset = H5Dcreate(cancel_file, DATASETNAME, datatype, dataspace, H5P_DEFAULT);

    /* If thread is cancelled, make cleanup call */
    cleanup_structure = (cancel_cleanup_t*)malloc(sizeof(cancel_cleanup_t));
    cleanup_structure->dataset = dataset;
    cleanup_structure->datatype = datatype;
    cleanup_structure->dataspace = dataspace;
    pthread_cleanup_push(cancellation_cleanup, cleanup_structure);

    datavalue = 1;
    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &datavalue);

    buffer = malloc(sizeof(int));
    H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buffer);
    H5Diterate(buffer, H5T_NATIVE_INT, dataspace, tts_cancel_callback, &dataset);

    sleep(3);

    datavalue = 100;
    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &datavalue);
    H5Dclose(dataset);
    H5Tclose(datatype);
    H5Sclose(dataspace);

    /*
     * Required by pthreads. The argument 0 pops the stack but does not
     * execute the cleanup routine.
     */
    pthread_cleanup_pop(0);
    return NULL;
}

herr_t tts_cancel_callback(void *elem, hid_t UNUSED type_id, unsigned UNUSED ndim,
			   const hsize_t UNUSED *point, void *operator_data)
{
    int value = *(int *)elem;
    hid_t dataset = *(hid_t *)operator_data;

    tts_cancel_barrier();
    sleep(3);

    if (value != 1) {
        TestErrPrintf("Error! Element value should be 1 and not %d\n", value);
        return -1;
    }

    value += 10;
    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &value);
    return 0;
}

/*
 * Need to perform the dataset, datatype and dataspace close that was never
 * performed because of thread cancellation
 */
void cancellation_cleanup(void *arg)
{
    cancel_cleanup_t *cleanup_structure = (cancel_cleanup_t *)arg;
    H5Dclose(cleanup_structure->dataset);
    H5Tclose(cleanup_structure->datatype);
    H5Sclose(cleanup_structure->dataspace);

    /* retained for debugging */
    /*  print_func("cancellation noted, cleaning up ... \n"); */
}

/*
 * Artificial (and specific to this test) barrier to keep track of whether
 * both the main and child threads have reached a point in the program.
 */
void tts_cancel_barrier(void)
{
    static int count = 2;

    pthread_mutex_lock(&mutex);

    if (count != 1) {
        count--;
        pthread_cond_wait(&cond, &mutex);
    } else {
        pthread_cond_signal(&cond);
    }

    pthread_mutex_unlock(&mutex);
}

void cleanup_cancel(void)
{
	HDunlink(FILENAME);
}

#endif /*H5_HAVE_THREADSAFE*/
