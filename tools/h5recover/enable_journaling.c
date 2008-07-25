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
 */

#include "hdf5.h"
#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>

#define H5FILE_NAME        	"JournalEG.h5"
#define H5JournalFILE_NAME 	H5FILE_NAME".jnl"
#define H5recovertoolname 	"h5recover"
#define H5dumptoolname 		"h5dump"
#define ProgName	 	"enable_journaling"	/* program name */
#define DATASETNAME "IntArray"
#define NX     10                      /* dataset initial dimensions */
#define NY     100
#define CHUNKX 2			/* chunk dimensions */
#define CHUNKY 10
#define RANK   2

/* Global variables */

/* protocols */
int writedata(hid_t dataset, int begin, int end);
void helppage(void);

/* Display the online help page */
void
helppage(void)
{
    printf(
	"Usage:\n"
	"%s -[c|r|p]\n"
	"\t-c\tCreate a new file (%s)\n"
	"\t-r\tReopen the file with Journaling (%s) for crash test\n"
	ProgName, H5FILE_NAME, H5JournalFILE_NAME
    );
    printf("To try this program, run:\n");
    printf("\t%s -c\n", ProgName);
    printf("\t%s %s\n", H5dumptoolname, H5FILE_NAME);
    printf("\t%s -r\n", ProgName);
    printf("\t%s %s (This should fail)\n", H5dumptoolname, H5FILE_NAME);
    printf("\t%s -j %s %s\n", H5recovertoolname, H5JournalFILE_NAME, H5FILE_NAME);
    printf("\t%s -p\n", ProgName);
    printf("\t%s %s (This should show more data)\n", H5dumptoolname, H5FILE_NAME);
}

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
    pid_t	mypid;
    int		cmode=0;		/* Create mode, overrides the others. */
    int		rmode=0;		/* Reopen mod, default no. */
    int		zmode=0;		/* Turn off all caching, default cache on. */


    /* Parse different options:
     * Default: Create a new file and new dataset.
     * rmode: Reopen an existing file with Journaling.
     *
     * How to use this:
     * ./enable_journaling	# create JournalEG.h5 file
     * ./enable_journaling -r	# reopen JournalEG.h5 with Journaling on and
     *			        # add more rows, then crash.
     * ./h5recover -j JournalEG.h5.jnl JournalEG.h5	# to recover the file.
     * Then JournalEG.h5 should have all the expected written rows and data.
     * 
     */
    if (ac<=1){
	helppage();
	return 1;
    }
    while (ac > 1){
	ac--;
	av++;
        if (strcmp("-c", *av) == 0){
	    cmode++;
	    printf("Create mode\n");
	}else if (strcmp("-r", *av) == 0){
	    rmode++;
	    printf("Reopen mode\n");
	}else if (strcmp("-z", *av) == 0){
	    zmode++;
	    printf("zmode on => all caching off\n");
	}else{
	    fprintf(stderr, "Unknown option (%s)\n", *av);
	    helppage();
	    return 1 ;
	}
    }

    if (cmode){
	/*===================================================
	 * Default:
	 * Create a new file with latest lib version, create a new dataset
	 * of unlimited dimension, initialize size to NX*NY, write data,
         * close file.
         * Needs the latest lib version in order to allow Journaling later.
	 *===================================================*/
	/*
	 * Create a new file using H5F_ACC_TRUNC access,
	 * default file creation properties, and latest lib version file
	 * access properties.
	 */
	faccpl = H5Pcreate(H5P_FILE_ACCESS);
	if (H5Pset_libver_bounds(faccpl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0){
	    fprintf(stderr, "H5Pset_libver_bounds on data file failed\n");
	    H5Pclose(faccpl);
	    return(-1);
	}
	file = H5Fcreate(H5FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, faccpl);
	H5Pclose(faccpl);

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
	H5Pclose(dsetpl);
	H5Dclose(dataset);
	H5Fclose(file);
	return(0);
    }
    if (rmode){
	/*===================================================
	 * rmode:
	 *    Reopen a previous file with Journaling on, extend the dataset
	 *    to 4NX rows, write data, crash.
	 *    Need to turn off H5Pset_sieve_buf_size( hid_t fapl_id, hsize_t size  ) so that raw data will be flushed immediately.
	 *===================================================*/

	/* reopen the file with Journaling on. */
	/* Setup file access property list with journaling */
	faccpl = H5Pcreate(H5P_FILE_ACCESS);
#if 1
	if (H5Pset_journal(faccpl, H5JournalFILE_NAME) < 0){
	    fprintf(stderr, "H5Pset_journal on data file failed\n");
	    H5Pclose(faccpl);
	    return(-1);
	}
#endif
	if (zmode){
	    printf("turning cache off\n");
	    /* Turn off data sieving to get raw data flushed to file */
	    /* immediately.                                          */
	    if (H5Pset_sieve_buf_size(faccpl, 0) < 0){
		fprintf(stderr, "H5Pset_sieve_buf_size on data file failed\n");
		H5Pclose(faccpl);
		return(-1);
	    }
	    /* Turn off chunk cache to get chunk raw data flushed to file */
	    /* immediately.                                          */
	    {
	    int mdc_nelmts;
	    size_t rdcc_nelmts;
	    size_t rdcc_nbytes;
	    double rdcc_w0;

	    if(H5Pget_cache(faccpl, &mdc_nelmts, &rdcc_nelmts, &rdcc_nbytes, &rdcc_w0) < 0){
		fprintf(stderr, "H5Pget_cache on data file failed\n");
		H5Pclose(faccpl);
		return(-1);
	    }
	    mdc_nelmts = 0;
	    rdcc_nelmts = 0;
	    rdcc_nbytes = 0;
	    if(H5Pset_cache(faccpl, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0) < 0) {
		fprintf(stderr, "H5Pset_cache on data file failed\n");
		H5Pclose(faccpl);
		return(-1);
	    }
	    }
	}

	/* Delete the journal file since journal code does not allow */
	/* existed journal file. */
	remove(H5JournalFILE_NAME);
	/* open the file with journaling property list */
	file = H5Fopen(H5FILE_NAME, H5F_ACC_RDWR, faccpl);

	/* close handle not needed any more. */
	H5Pclose(faccpl);

	/* Open dataset for modifications. */
	dataset=H5Dopen2(file, DATASETNAME, H5P_DEFAULT);
	/* extend the dataset to 4NX rows. */
	dimsf[0] = 4*NX;
	dimsf[1] = NY;
	H5Dset_extent(dataset, dimsf);

	/* write data to new rows and crash */
	/* Do 3 writes to generate at least 3 transactions. */
	writedata(dataset, NX, 2*NX-1);
	writedata(dataset, 2*NX, 3*NX-1);
	writedata(dataset, 3*NX, 4*NX-1);
	/* simulate a crash ending of the aiplication */
#if 1
	fprintf(stderr, "going to crash myself\n");
	mypid = getpid();
	kill(mypid, SIGTERM);	/* Terminate myself */
#endif
	/* Will execute these only when not terminated. */
	H5Dclose(dataset);
	H5Fclose(file);
	return(0);
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
    hid_t	memspace, dataspace;
    hsize_t	dims[RANK], start[RANK], count[RANK];
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

    dims[0]=NX;
    dims[1]=NY;
    memspace = H5Screate_simple(RANK, dims, NULL);

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
    retcode = H5Dwrite(dataset, H5T_NATIVE_INT, memspace, dataspace, H5P_DEFAULT, data);
    
    H5Sclose(memspace);
    H5Sclose(dataspace);
    return(retcode);
}
