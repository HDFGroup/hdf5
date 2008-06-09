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

/*
 * This example shows how to use the Journaling feature. It also simulates a
 * crash without closing the file first.
 * The patch mode reopens the file, defines the dataset again without writing
 * any data, then close the file.  This is a temporary fix till the object header
 * coding is working.
 */

#include "hdf5.h"
#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>

#define H5FILE_NAME        	"JournalEG.h5"
#define H5JournalFILE_NAME 	H5FILE_NAME".jnl"
#define DATASETNAME "IntArray"
#define NX     500                      /* dataset dimensions */
#define NY     600
#define CHUNKX 10			/* chunk dimensions */
#define CHUNKY 10
#define RANK   2

int
main (int ac, char **av)
{
    hid_t       file, dataset, fapl_id;         /* file and dataset handles */
    hid_t       dataspace;   			/* handles */
    hsize_t     dimsf[2]={NX, NY};              /* dataset dimensions */
    hsize_t     chunk[2]={CHUNKX, CHUNKY};	/* chunk dimensions */
    hid_t       plist;
    herr_t      status;
    int         data[NX][NY];          /* data to write */
    int         i, j;
    pid_t	mypid;
    int		pmode=0;		/* patch mode, default no. */


    if (ac > 1){
	av++;
        if (strcmp("-p", *av) == 0){
	    pmode++;
	    printf("Patch mode on\n");
	}
	else{
	    fprintf(stderr, "Unknown option (%s)\n", *av);
	    return 1 ;
	}
    }

    if (pmode){
	/* just reopen the file to patch it. */
	file = H5Fopen(H5FILE_NAME, H5F_ACC_RDWR, H5P_DEFAULT);
    }else{
	/* ========================== */
	/* Enable journaling          */
	/* ========================== */

	/* create fapl */
	fapl_id = H5Pcreate(H5P_FILE_ACCESS);
	if (H5Pset_journal(fapl_id, H5JournalFILE_NAME) < 0){
	    fprintf(stderr, "H5Pset_journal on data file failed\n");
	    H5Pclose(fapl_id);
	    return(-1);
	}
	/* Delete the journal file since journal code does not allow existed journal file. */
	remove(H5JournalFILE_NAME);

	/*
	 * Create a new file using H5F_ACC_TRUNC access,
	 * default file creation properties, and default file
	 * access properties.
	 */
	file = H5Fcreate(H5FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);

	/* close handle not needed any more. */
	H5Pclose(fapl_id);
    }

    /*
     * create the data space for fixed size dataset.
     */
    dataspace = H5Screate_simple(RANK, dimsf, NULL);

    /* Create dataset creation property list */
    plist = H5Pcreate(H5P_DATASET_CREATE);
    
    /* Enable chunking */
    H5Pset_layout(plist, H5D_CHUNKED);
    H5Pset_chunk(plist, 2, chunk);

    /*
     * Create a new dataset of native int within the file using defined dataspace
     * and chunked storage type.
     */
    dataset = H5Dcreate2(file, DATASETNAME, H5T_NATIVE_INT, dataspace,
			H5P_DEFAULT, plist, H5P_DEFAULT);

    /* write actual data and simulate crash only if it is not patch mode. */
    if (!pmode){
	/* Initialize data buffers */
	for(j = 0; j < NX; j++)
	    for(i = 0; i < NY; i++)
		data[j][i] = i + j;

	/*
	 * Write the data to the dataset using default transfer properties.
	 */
	status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);


	/* simulate a crash ending of the application */
	mypid = getpid();
	kill(mypid, SIGTERM);	/* Terminate myself */
    }

    /* Close resources */
    H5Sclose(dataspace);
    H5Dclose(dataset);
    H5Pclose(plist);
    H5Fclose(file);
}
