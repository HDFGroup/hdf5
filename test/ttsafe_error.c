/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
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
 * HDF5 APIs exercised in thread:
 *
 *     H5Screate_simple, H5Tcopy, H5Tset_order, H5Dcreate, H5Dclose,
 *     H5Tclose, H5Sclose.
 *
 * Created: Apr 28 2000
 * Programmer: Chee Wai LEE
 *
 * Modification History
 * --------------------
 *
 * 	19 May 2000, Bill Wendling
 * 	Modified so that it creates a unique HDF5 file and removes it on
 * 	cleanup.
 *
 ********************************************************************/
#include "ttsafe.h"

#ifdef H5_HAVE_THREADSAFE

#define NUM_THREAD              16
#define FILENAME                "ttsafe_error.h5"

/* Having a common dataset name is an error */
#define DATASETNAME		"commonname"
#define EXPECTED_ERROR_DEPTH	8
#define WRITE_NUMBER		37

static herr_t error_callback(void *);
static herr_t walk_error_callback(unsigned, const H5E_error_t *, void *);
static void *tts_error_thread(void *);

/* Global variables */
hid_t error_file;
int ret;

typedef struct err_num_struct {
    hid_t maj_num;
    hid_t min_num;
} err_num_t;

err_num_t expected[8];

int error_flag = 0;
int error_count = 0;
pthread_mutex_t error_mutex;

void tts_error(void)
{
    pthread_t threads[NUM_THREAD];
    pthread_attr_t attribute;
    hid_t dataset;
    int value, i;

    /* Must initialize these at runtime */
    expected[0].maj_num = H5E_DATASET;
    expected[0].min_num = H5E_CANTINIT;

    expected[1].maj_num = H5E_DATASET;
    expected[1].min_num = H5E_CANTINIT;

    expected[2].maj_num = H5E_SYM;
    expected[2].min_num = H5E_EXISTS;

    expected[3].maj_num = H5E_SYM;
    expected[3].min_num = H5E_CANTINSERT;

    expected[4].maj_num = H5E_SYM;
    expected[4].min_num = H5E_CANTINSERT;

    expected[5].maj_num = H5E_BTREE;
    expected[5].min_num = H5E_CANTINIT;

    expected[6].maj_num = H5E_BTREE;
    expected[6].min_num = H5E_CANTINSERT;

    expected[7].maj_num = H5E_SYM;
    expected[7].min_num = H5E_CANTINSERT;

    /* set up mutex for global count of errors */
    ret=pthread_mutex_init(&error_mutex, NULL);
	assert(ret==0);

    /* make thread scheduling global */
    ret=pthread_attr_init(&attribute);
	assert(ret==0);

#ifdef H5_HAVE_SYSTEM_SCOPE_THREADS
    ret=pthread_attr_setscope(&attribute, PTHREAD_SCOPE_SYSTEM);
	assert(ret==0);
#endif /* H5_HAVE_SYSTEM_SCOPE_THREADS */

    /*
     * Create a hdf5 file using H5F_ACC_TRUNC access, default file
     * creation plist and default file access plist
     */
    error_file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	assert(error_file>=0);

    for (i = 0; i < NUM_THREAD; i++){
        ret=pthread_create(&threads[i], &attribute, tts_error_thread, NULL);
		assert(ret==0);
	}

    for (i = 0; i < NUM_THREAD; i++){
        ret=pthread_join(threads[i],NULL);
		assert(ret==0);
	}

    if (error_flag)
        TestErrPrintf("Threads reporting different error values!\n");

    if (error_count != NUM_THREAD - 1)
        TestErrPrintf("Error: %d threads failed instead of %d\n", error_count, NUM_THREAD-1);

    dataset = H5Dopen(error_file, DATASETNAME);
	assert(dataset>=0);

    ret=H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &value);
	assert(ret>=0);

    if (value != WRITE_NUMBER)
        TestErrPrintf("Error: Successful thread wrote value %d instead of %d\n", value, WRITE_NUMBER);

    ret=H5Dclose(dataset);
	assert(ret>=0);
    ret=H5Fclose(error_file);
	assert(ret>=0);

    /* Destroy the thread attribute */
    ret=pthread_attr_destroy(&attribute);
	assert(ret==0);
}

static
void *tts_error_thread(void UNUSED *arg)
{
    hid_t dataspace, datatype, dataset;
    hsize_t dimsf[1]; /* dataset dimensions */
    H5E_auto_stack_t old_error_cb;
    void *old_error_client_data;
    int value;

    /* preserve previous error stack handler */
    H5Eget_auto_stack(H5E_DEFAULT, &old_error_cb, &old_error_client_data);

    /* set each thread's error stack handler */
    H5Eset_auto(error_callback, NULL);

    /* define dataspace for dataset */
    dimsf[0] = 1;
    dataspace = H5Screate_simple(1,dimsf,NULL);
    assert(dataspace>=0);

    /* define datatype for the data using native little endian integers */
    datatype = H5Tcopy(H5T_NATIVE_INT);
	assert(datatype>=0);
    H5Tset_order(datatype, H5T_ORDER_LE);

    /* create a new dataset within the file */
    dataset = H5Dcreate(error_file, DATASETNAME, datatype, dataspace, H5P_DEFAULT);
	
    if (dataset >= 0) {   /* not an error */
        value = WRITE_NUMBER;
        H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &value);
        H5Dclose(dataset);
    }

    ret=H5Tclose(datatype);
	assert(ret>=0);
    ret=H5Sclose(dataspace);
	assert(ret>=0);

    /* turn our error stack handler off */
    H5Eset_auto_stack(H5E_DEFAULT, old_error_cb, old_error_client_data);

    return NULL;
}

static
herr_t error_callback(void *client_data)
{
    ret=pthread_mutex_lock(&error_mutex);
	assert(ret==0);
    error_count++;
    ret=pthread_mutex_unlock(&error_mutex);
	assert(ret==0);
    return H5Ewalk(H5E_WALK_DOWNWARD, walk_error_callback, client_data);
}

static
herr_t walk_error_callback(unsigned n, const H5E_error_t *err_desc, void UNUSED *client_data)
{
    hid_t maj_num, min_num;

    if (err_desc) {
        maj_num = err_desc->maj_num;
        min_num = err_desc->min_num;

        if (n < EXPECTED_ERROR_DEPTH && maj_num == expected[n].maj_num &&
                min_num == expected[n].min_num)
            return SUCCEED;
    }

    error_flag = -1;
    return SUCCEED;
}

void cleanup_error(void)
{
    HDunlink(FILENAME);
}

#endif /*H5_HAVE_THREADSAFE*/
