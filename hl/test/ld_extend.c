#include "H5HLprivate2.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <unistd.h>

/* Size of data buffer */
#define TEST_BUF_SIZE 100

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

static int extend_dset(const char *file, char *dname);

/* 
 * Extend the specified dataset in the file with ld_monitor.c monitoring 
 *	the dataset on the other end:
 *
 * 	1) Extend the dataset according to the variations: ONE_NTESTS, TWO_NTESTS
 *	2) Write to the dataset (currently, only for integer dataset)
 *	3) Flush the dataset 
 */
static int
extend_dset(const char *file, char *dname)
{
    hid_t fid;		/* file id */
    hid_t did; 		/* dataset id */
    hid_t dtype;	/* dataset's datatype */
    hid_t sid;		/* dataspace id */
    int i, j, k;	/* local index variable */
    int ndims;		/* number of dimensions */
    int buf[TEST_BUF_SIZE];	/* buffer for data */
    hsize_t cur_dims[2];	/* current dimension sizes */
    hsize_t ext_dims[2];	/* new dimension sizes after extension */

    /* Open the file */
    if((fid = H5Fopen(file, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
	goto done;

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

    /* Loop through different variations of extending the dataset */
    for(i = 0; i < (ndims == 1 ? ONE_NTESTS: TWO_NTESTS); i++) {

	sleep(2);

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
    if(H5Fclose(fid) < 0) goto done;

    return(0);

done:
    H5E_BEGIN_TRY
	H5Tclose(dtype);
        H5Sclose(sid);
        H5Dclose(did);
	H5Fclose(fid);
    H5E_END_TRY

    return(-1);
} /* extend_dset() */


/* Usage: extend_dset xx.h5 dname */
int
main(int argc, const char *argv[])
{
    char *dname = NULL;	/* dataset name */
    char *fname = NULL;	/* file name */

    if(argc != 3) {
	fprintf(stderr, "Should have file name and dataset name to be extended...\n");
	goto done;
    }

    /* Get the file and dataset names to be extended */
    fname = HDstrdup(argv[1]);
    dname = HDstrdup(argv[2]);

    /* Extend the specified dataset in the file */
    if(extend_dset(fname, dname) < 0)
	goto done;

    exit(EXIT_SUCCESS);

done:
    if(dname) HDfree(dname);
    if(fname) HDfree(fname);
    exit(EXIT_FAILURE);
} /* main() */
