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
 * HDF5 APIs exercised in thread:
 * H5Screate_simple, H5Tcopy, H5Tset_order, H5Dcreate, H5Dwrite, H5Dclose,
 * H5Tclose, H5Sclose.
 *
 * Created: Apr 28 2000
 * Programmer: Chee Wai LEE
 *
 * Modification History
 * --------------------
 *
 *	19 May 2000, Bill Wendling
 *	Changed so that it creates its own HDF5 file and removes it at
 *	cleanup time. Added num_errs flag.
 *
 ********************************************************************/
#include "ttsafe.h"

#ifndef H5_HAVE_THREADSAFE
static int dummy;	/* just to create a non-empty object file */
#else

#define FILENAME		"ttsafe_dcreate.h5"
#define DATASETNAME_LENGTH	10
#define NUM_THREAD		16

/* Global variables */
extern int num_errs;
extern int Verbosity;

void *tts_dcreate_creator(void *);

typedef struct thread_info {
	int id;
	hid_t file;
	const char *dsetname;
} thread_info; 

/*
 **********************************************************************
 * Thread safe test - multiple dataset creation
 **********************************************************************
 */
void tts_dcreate(void)
{
	/* Pthread definitions */
	pthread_t threads[NUM_THREAD];

	/* HDF5 data definitions */
	hid_t file, dataset, datatype;
	int datavalue, i;
	thread_info *thread_out;
	const char *dsetname[NUM_THREAD];
	pthread_attr_t attribute;

	/* set pthread attribute to perform global scheduling */
	pthread_attr_init(&attribute);
	pthread_attr_setscope(&attribute, PTHREAD_SCOPE_SYSTEM);

	/*
	 * Set individual dataset names (rather than generated the names
	 * automatically)
	 */

	for (i = 0; i < NUM_THREAD; i++)
		dsetname[i] = malloc(sizeof(char) * DATASETNAME_LENGTH);

	dsetname[0] = "zero";
	dsetname[1] = "one";
	dsetname[2] = "two";
	dsetname[3] = "three";
	dsetname[4] = "four";
	dsetname[5] = "five";
	dsetname[6] = "six";
	dsetname[7] = "seven";
	dsetname[8] = "eight";
	dsetname[9] = "nine";
	dsetname[10] = "ten";
	dsetname[11] = "eleven";
	dsetname[12] = "twelve";
	dsetname[13] = "thirteen";
	dsetname[14] = "fourteen";
	dsetname[15] = "fifteen";

	/*
	 * Create a hdf5 file using H5F_ACC_TRUNC access, default file
	 * creation plist and default file access plist
	 */
	file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

	/* simultaneously create a large number of datasets within the file */
	for (i = 0; i < NUM_THREAD; i++) {
		thread_out = malloc(sizeof(thread_info));
		thread_out->id = i;
		thread_out->file = file;
		thread_out->dsetname = dsetname[i];
		pthread_create(&threads[i], NULL, tts_dcreate_creator, thread_out);
	}

	for (i = 0;i < NUM_THREAD; i++)
		pthread_join(threads[i], NULL);

	/* compare data to see if it is written correctly */

	/* define datatype for the data using native little endian integers */
	datatype = H5Tcopy(H5T_NATIVE_INT);

	for (i = 0; i < NUM_THREAD; i++) {
		if ((dataset = H5Dopen(file,dsetname[i])) < 0) {
			fprintf(stderr, "Dataset name not found - test failed\n");
			H5Fclose(file);
			num_errs++;
			return;
		} else {
			H5Dread(dataset, datatype, H5S_ALL, H5S_ALL, H5P_DEFAULT, &datavalue);

			if (datavalue != i) {
				fprintf(stderr,
					"Wrong value read %d for dataset name %s - test failed\n",
					datavalue, dsetname[i]);
				H5Dclose(dataset);
				H5Fclose(file);
				num_errs++;
				return;
			}

			H5Dclose(dataset);
		}
	}

	/* close remaining resources */
	H5Fclose(file);
}

void *tts_dcreate_creator(void *thread_data)
{
	hid_t   dataspace, datatype, dataset;
	hsize_t dimsf[1]; /* dataset dimensions */
	struct thread_info {
		int id;
		hid_t file;
		char *dsetname;
	} thread_in;

	thread_in.dsetname = malloc(sizeof(char) * DATASETNAME_LENGTH);
	thread_in = *((struct thread_info *)thread_data);

	/* define dataspace for dataset */
	dimsf[0] = 1;
	dataspace = H5Screate_simple(1,dimsf,NULL);

	/* define datatype for the data using native little endian integers */
	datatype = H5Tcopy(H5T_NATIVE_INT);
	H5Tset_order(datatype, H5T_ORDER_LE);

	/* create a new dataset within the file */
	dataset = H5Dcreate(thread_in.file, thread_in.dsetname,
			    datatype, dataspace, H5P_DEFAULT);

	/* initialize data for dataset and write value to dataset */
	H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		 H5P_DEFAULT, &thread_in.id);

	/* close dataset, datatype and dataspace resources */
	H5Dclose(dataset);
	H5Tclose(datatype);
	H5Sclose(dataspace);
	return NULL;
}

void cleanup_dcreate(void)
{
	H5close();
	HDunlink(FILENAME);
}
#endif /*H5_HAVE_THREADSAFE*/
