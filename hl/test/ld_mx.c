#include "h5hltest.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

/* Size of data buffer */
#define TEST_BUF_SIZE 100

/* Note: These two defines should be the same as the defines in test_ld.sh.in */
/* The message sent by extend_dset that the file is done with file open releasing the file lock */
#define WRITER_MESSAGE "LD_WRITER_MESSAGE"
/* The message sent by monitor_dset that it is done setting up */
#define READER_MESSAGE "LD_READER_MESSAGE"

/* 
 * Test variations (incremental) for one-dimensional dataset:
 * 	Varies from 10->13->12->12->1->3
 */
#define ONE_NTESTS	5
int one_tests[ONE_NTESTS] = {3, -1, 0, -11, 2};

/* 
 * Test variations (incremental) for two-dimensional dataset:
 *	Varies from {4,10}->{6,12}->{8,1}->{10,1}->
 *	       	      	    {3,3}->{2,2}->{1,2}->
 *		            {1,4}->{1,3}->{1,3}
 */
#define TWO_NTESTS  9
int two_tests[TWO_NTESTS][2] = { {2, 2}, {2, -11}, {2, 0},
                                 {-7, 2}, {-1, -1}, {-1, 0},
                                 {0, 2}, {0, -1}, {0, 0}   
			       };

/* 
 * Extend the specified dataset in the file with "monitor_dset" monitoring 
 *	the dataset on the other end:
 * 	1) Extend the dataset according to the variations: ONE_NTESTS, TWO_NTESTS
 *	2) Write to the dataset (currently, only for integer dataset)
 *	3) Flush the dataset 
 *
 *      Due to the implementation of file locking, coordination is needed in file
 *      opening for the writer/reader tests to proceed as expected:
 *	  --it will send the WRITER_MESSAGE when the file open is done:
 *	    to notify monitor_dset() to start monitoring the dataset
 *	  --it will wait for the READER_MESSAGE from monitor_dset() before extending
 *	    the dataset
 */
static int
extend_dset(const char *file, char *dname)
{
    hid_t fid;		/* file id */
    hid_t fapl;		/* file access property list */
    hid_t did; 		/* dataset id */
    hid_t dtype;	/* dataset's datatype */
    hid_t sid;		/* dataspace id */
    int i, j, k;	/* local index variable */
    int ndims;		/* number of dimensions */
    int buf[TEST_BUF_SIZE];	/* buffer for data */
    hsize_t cur_dims[2];	/* current dimension sizes */
    hsize_t ext_dims[2];	/* new dimension sizes after extension */

    /* Create a file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
	goto done;

    /* Set to use latest library format */
    if((H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST)) < 0)
        goto done;

    /* Open the file with SWMR write */
    if((fid = H5Fopen(file, H5F_ACC_RDWR|H5F_ACC_SWMR_WRITE, fapl)) < 0)
	goto done;

    /* Send message that "extend_dset" (this routine) is done with H5Fopen--the file lock is released */
    h5_send_message(WRITER_MESSAGE);

    /* Open the dataset */
    if((did = H5Dopen2(fid, dname, H5P_DEFAULT)) < 0)
	goto done;

    /* Get the dataset's data space */
    if((sid = H5Dget_space(did)) < 0)
	goto done;

    /* Get the # of dimensions for the dataset */
    if((ndims = H5Sget_simple_extent_ndims(sid)) < 0)
	goto done;

    /* Initialize data written to the dataset */
    HDmemset(buf, 0, sizeof(buf));
    for(k = 0; k < TEST_BUF_SIZE; k++)
	buf[k] = k;

    /* Wait for message from "monitor_dset" before starting to extend the dataset */
    if(h5_wait_message(READER_MESSAGE) < 0)
        goto done;

    /* Loop through different variations of extending the dataset */
    for(i = 0; i < (ndims == 1 ? ONE_NTESTS: TWO_NTESTS); i++) {
	HDsleep(2);

	/* Get the dataset's current dimension sizes */
	if(H5LDget_dset_dims(did, cur_dims) < 0)
	    goto done;

	/* Set up the new extended dimension sizes  */
	for(j = 0; j < ndims; j++)
	    ext_dims[j] = cur_dims[j] + (ndims == 1 ? (hsize_t)one_tests[i] : (hsize_t)two_tests[i][j]);
 
	/* Extend the dataset */
	if(H5Dset_extent(did, ext_dims) < 0)
	    goto done;

	/* Get the dataset's data type */
	if((dtype = H5Tget_native_type(H5Dget_type(did), H5T_DIR_DEFAULT)) < 0)
	    goto done;

	/* Write to the whole dataset after extension */
	if(H5Dwrite(did, dtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
	    goto done;

	/* Flush the data */
	if(H5Dflush(did) < 0)
	    goto done;

    } /* end for ONE_NTESTS or TWO_NTESTS */

    /* Closing */
    if(H5Tclose(dtype) < 0) goto done;
    if(H5Sclose(sid) < 0) goto done;
    if(H5Dclose(did) < 0) goto done;
    if(H5Pclose(fapl) < 0) goto done;
    if(H5Fclose(fid) < 0) goto done;

    return(0);

done:
    H5E_BEGIN_TRY
	H5Tclose(dtype);
        H5Sclose(sid);
        H5Dclose(did);
	H5Pclose(fapl);
	H5Fclose(fid);
    H5E_END_TRY

    return(-1);
} /* extend_dset() */


/*
 * Monitor the specified dataset in the file while "extend_dset" is extending
 *      and writing to the dataset on the other end:
 *
 *      1) Retrieve the dataset's current dimension sizes
 *      2) If there are changes in dimension sizes:
 *		print the dimension sizes
 *		retrieve the appended data and print them
 *
 *      Due to the implementation of file locking, coordination is needed in file
 *      opening for the writer/reader tests to proceed as expected:
 *	  --it will wait for the WRITER_MESSAGE from extend_dset() before opening 
 *	    the test file
 *	  --it will send the READER_MESSAGE when the setup is done:
 *	    to notify extend_dset() to start extending the dataset
 */
static int
monitor_dset(const char *fname, char *dname)
{
    hid_t fid;				/* file id */
    hid_t fapl;				/* file access property list */
    hid_t did;				/* dataset id */
    hid_t sid;				/* dataspace id */
    int	ndims;				/* # of dimensions in the dataspace */
    int i, u;				/* local index variable */
    hsize_t cur_dims[H5S_MAX_RANK];	/* current dimension sizes */
    hsize_t prev_dims[H5S_MAX_RANK];	/* previous dimension sizes */
    int buf[TEST_BUF_SIZE];		/* Buffer for data */
    herr_t ret_value = 0;		/* return value */

    /* Wait for message from "extend_dset" before opening the file */
    if(h5_wait_message(WRITER_MESSAGE) < 0)
        goto done;

    /* Create a file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
	goto done;

    /* Set to use latest library format */
    if((H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST)) < 0)
        goto done;

    /* Open the file with SWMR access */
    if((fid = H5Fopen(fname, H5F_ACC_SWMR_READ, fapl)) < 0) {
	ret_value = -1;
        goto done;
    }

    HDfprintf(stdout, "Monitoring dataset %s...\n", dname);

    /* Open the dataset for minitoring */
    if((did = H5Dopen2(fid, dname, H5P_DEFAULT)) < 0) {
	HDfprintf(stdout, "error in opening dataset \"%s\"\n", dname);
	ret_value = -1;
	goto done;
    }				

    /* Get the dataset's data space */
    if((sid = H5Dget_space(did)) < 0) {
	HDfprintf(stdout, "error in getting dataspace id for dataset \"%s\"\n", dname);
	ret_value = -1;
	goto done;
    }

    /* Get the dataset's dimension sizes */
    if((ndims = H5Sget_simple_extent_dims(sid, prev_dims, NULL)) < 0) {
	HDfprintf(stdout, "unable to get dimensions sizes for \"%s\"\n", dname);
	ret_value = -1;
	goto done;
    }

    /* Send message that "monitor_dset" (this routine) is done with setting up */
    h5_send_message(READER_MESSAGE);

    /* Monitor the dataset for changes */
    while(1) {

	/* Refresh the dataset */
	if(H5Drefresh(did) < 0) {
	    ret_value = -1;
	    goto done;
	}

	/* Get the dataset's current dimension sizes */
	if(H5LDget_dset_dims(did, cur_dims) < 0) {
	    HDfprintf(stdout, "unable to get dimension sizes for \"%s\"\n", dname);
	    ret_value = -1;
	    goto done;
	}

	/* Check for changes in dimension sizes */
	for(u = 0; u < ndims; u++) {
	    if(cur_dims[u] != prev_dims[u]) {
		break;
	    }
	}

	/* Printing only when there are changes */
	if(u < ndims) {
	    /* Print the current dimension sizes */
	    HDfprintf(stdout, "\n");
	    for(i = 0; i < ndims; i++)
		HDfprintf(stdout, "%d ", (int)cur_dims[i]);
	    HDfprintf(stdout, "\n");

	    /* Get data appended to the dataset and print the data */
	    HDmemset(buf, 0, sizeof(buf));
            if(H5LDget_dset_elmts(did, prev_dims, cur_dims, NULL, buf) >= 0) {

		for(i = 0; i < TEST_BUF_SIZE; i++) {
		    if(i % 10)
			HDfprintf(stdout, "%d ", buf[i]);
		    else
			HDfprintf(stdout, "\n%d ", buf[i]);
		}
		HDfprintf(stdout, "\n");
	    }

	    /* Flush the output to stdout */
	    HDfflush(stdout);
	    /* Update the dimension sizes */
	    HDmemcpy(prev_dims, cur_dims, ndims * sizeof(hsize_t));
	}
	HDsleep(1);
    } /* end for */

done:
    /* Closing */
    H5E_BEGIN_TRY
	H5Sclose(sid);
	H5Dclose(did);
	H5Pclose(fapl);
    H5E_END_TRY

    return(ret_value);
} /* monitor_dset() */

/* Usage: 
 *	ld_mx -m fname dname -- to monitor the file's dataset
 *  or 
 *	ld_mx -x fname dname --	to extend the file's dataset
 */
/* This file is a combination of ld_monitor.c and ld_extend.c which are svn deleted */
int
main(int argc, char *argv[])
{
    char *dname = NULL;	/* dataset name */
    char *fname = NULL;	/* file name */
    int opt = 0;
    unsigned int monitor = 0, extend = 0;

    if(argc != 4) {
	fprintf(stderr, "Usage: ld_mx -m fname dname for monitoring the dataset 'dname' in the file 'fname'\n");
	fprintf(stderr, "Usage: ld_mx -x fname dname for extending the dataset 'dname' in the file 'fname'\n");
	goto done;
    }
    /* Parse command line options */
    while((opt = getopt(argc, argv, "mx")) != -1) {
        switch(opt) {
            case 'm': /* monitor dataset */
		monitor = 1;
                break;
    
            case 'x': /* extend dataset */
		extend = 1;	
                break;
            default:
                printf("Invalid option encountered\n");
                break;
        }
    }

    if((extend && monitor) || (!extend && !monitor)) {
	fprintf(stderr, "Usage: ldmx -m/-x  fname dname\n");
	goto done;
    }

    if(optind != 2) {
	fprintf(stderr, "Usage: ldmx -m/-x  fname dname\n");
	goto done;
    }

    fname = HDstrdup(argv[optind++]);
    dname = HDstrdup(argv[optind++]);


    if(extend) {
	if(extend_dset(fname, dname) < 0)
	    goto done;
    } else if(monitor) {
	if(monitor_dset(fname, dname) < 0)
	    goto done;
    }

    exit(EXIT_SUCCESS);

done:
    if(dname) HDfree(dname);
    if(fname) HDfree(fname);
    exit(EXIT_FAILURE);
} /* main() */
