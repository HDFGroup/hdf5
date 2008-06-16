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
#define NX     20                      /* dataset initial dimensions */
#define NY     20
#define CHUNKX 10			/* chunk dimensions */
#define CHUNKY 10
#define RANK   2

int writedata(hid_t dataset, int begin, int end);

int
main (int ac, char **av)
{
    hid_t       file, dataset;         /* file and dataset handles */
    hid_t       dataspace;   			/* handles */
    hsize_t 	maxdims[RANK] = {H5S_UNLIMITED, NY}; /* Dataset dimensions */
						/* with unlimited rows. */
    hsize_t     dimsf[RANK]={NX, NY};           /* initial dataset dimensions */
    hsize_t     chunk[RANK]={CHUNKX, CHUNKY};	/* chunk dimensions */
    hid_t       dsetpl;			/* Dataset property list */
    hid_t       faccpl;			/* File access property list */
    herr_t      status;
    pid_t	mypid;
    int		pmode=0;		/* patch mode, default no. */
    int		rmode=0;		/* Reopen mod, default no. */


    /* Parse different options:
     * Default: Create a new file and new dataset.
     * rmode: Reopen an existing file with Journaling.
     * pmode: Patch mode (implies rmode but no data write nor crash).
     *
     * How to use this:
     * ./enable_journaling	# create JournalEG.h5 file
     * ./enable_journaling -r	# reopen JournalEG.h5 with Journaling on and
     *			        # add more rows, then crash.
     * ./h5recover -j JournalEG.h5.jnl JournalEG.h5	# to recover the file.
     * ./enable_journaling -p	# patch it with metadata of the added rows.
     * Then JournalEG.h5 should have all the expected written rows and data.
     * 
     */
    while (ac > 1){
	ac--;
	av++;
        if (strcmp("-p", *av) == 0){
	    pmode++;
	    rmode++;
	    printf("Patch mode on\n");
	}else
        if (strcmp("-r", *av) == 0){
	    rmode++;
	    printf("Reopen mode\n");
	}else{
	    fprintf(stderr, "Unknown option (%s)\n", *av);
	    return 1 ;
	}
    }

    if (!rmode){
	/*===================================================
	 * Default:
	 * Create a new file, create a new dataset of unlimited dimension,
	 * initialize size to NX*NY, write data, close file.
	 *===================================================*/

	/*
	 * Create a new file using H5F_ACC_TRUNC access,
	 * default file creation properties, and default file
	 * access properties.
	 */
	file = H5Fcreate(H5FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

	/*
	 * create the data space for fixed size dataset.
	 * Initial dimension is NX*NY.
	 */
	dataspace = H5Screate_simple(RANK, dimsf, maxdims);

	/* Create dataset creation property list */
	dsetpl = H5Pcreate(H5P_DATASET_CREATE);
	
	/* Enable chunking needed for unlimited dimesion */
	H5Pset_layout(dsetpl, H5D_CHUNKED);
	H5Pset_chunk(dsetpl, 2, chunk);

	/*
	 * Create a new dataset of native int within the file using defined dataspace
	 * and chunked storage type.
	 */
	dataset = H5Dcreate2(file, DATASETNAME, H5T_NATIVE_INT, dataspace,
			    H5P_DEFAULT, dsetpl, H5P_DEFAULT);

	writedata(dataset, 0, NX-1);

	H5Sclose(dataspace);
	H5Dclose(dataset);
	H5Pclose(dsetpl);
	H5Fclose(file);
    }else{
	/*===================================================
	 * rmode:
	 *    Reopen a previous file with Journaling on, extend the dataset
	 *    to 2NX rows, write data, crash.
	 * pmode:
	 *    Patch mode (similar to rmode but no data write nor crash).
	 *    Reopen a restored file, extend the dataset to 2NX rows,
	 *    do not write data, close file.
	 *===================================================*/

	if (pmode){
	    /* just reopen the file to patch it. */
	    file = H5Fopen(H5FILE_NAME, H5F_ACC_RDWR, H5P_DEFAULT);
	}else{
	    /* reopen the file with Journaling on. */
	    /* Setup file access property list with journaling */
	    faccpl = H5Pcreate(H5P_FILE_ACCESS);
	    if (H5Pset_journal(faccpl, H5JournalFILE_NAME) < 0){
		fprintf(stderr, "H5Pset_journal on data file failed\n");
		H5Pclose(faccpl);
		return(-1);
	    }
	    /* Delete the journal file since journal code does not allow */
	    /* existed journal file. */
	    remove(H5JournalFILE_NAME);
	    /* open the file with journaling property list */
	    file = H5Fopen(H5FILE_NAME, H5F_ACC_RDWR, faccpl);

	    /* close handle not needed any more. */
	    H5Pclose(faccpl);
	}

	dataset=H5Dopen2(file, DATASETNAME, H5P_DEFAULT);
	/* extend the dataset to 2NX rows. */
	dimsf[0] = 2*NX;
	dimsf[1] = NY;
	H5Dset_extent(dataset, dimsf);

	if (!pmode){
	    /* write data to new rows and crash */
	    writedata(dataset, NX, 2*NX-1);
	    /* simulate a crash ending of the application */
	    mypid = getpid();
	    kill(mypid, SIGTERM);	/* Terminate myself */
	}else{
	    H5Dclose(dataset);
	    H5Fclose(file);
	}
    }
}


/* writedata():
 * write rows of data to dataset starting from begin to end rows inclusive.
 */
int
writedata(hid_t dataset, int begin, int end)
{
    int         data[NX][NY];          /* data to write */
    int         nrows, i, j;
    hid_t	dataspace;
    hsize_t	start[RANK], count[RANK];
    herr_t	retcode;


    if ((end < begin) || (begin < 0)){
	fprintf(stderr, "writedata: bad arguments (begin=%d, end=%d)\n", begin, end);
	return(-1);
    }
    nrows = end-begin+1;
    if (nrows>NX){
	fprintf(stderr, "writedata: too many rows (begin=%d, end=%d, NX=%d)\n",
	    begin, end, NX);
	return(-1);
    }

    dataspace = H5Dget_space(dataset);
    start[0]=begin;
    start[1]=0;
    count[0]=nrows;
    count[1]=NY;
    if ((retcode = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, start,
		    NULL, count, NULL)) <0){
	fprintf(stderr, "H5Sselect_hyperslab failed\n");
	return(-1);
    };

    /* Initialize data buffer */
    for(i = 0; i < nrows; i++)
	for(j = 0; j < NY; j++)
	    data[i][j] = (i+begin)*NY + j;

    /*
     * Write the data to the dataset using default transfer properties.
     */
    retcode = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, dataspace, H5P_DEFAULT, data);
    return(retcode);

}
