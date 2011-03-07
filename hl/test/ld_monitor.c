#include "H5HLprivate2.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <unistd.h>

#define TEST_BUF_SIZE 100

/*
 * Monitor the specified dataset in the file while ld_extend.c extending
 *      and writing to the dataset on the other end:
 *
 *      1) Retrieve the dataset's current dimension sizes
 *      2) If there are changes in dimension sizes:
 *		print the dimension sizes
 *		retrieve the appended data and print them
 */
static int
monitor_dset(const char *fname, char *dname)
{
    hid_t fid;		/* dataset id */
    hid_t did;		/* dataset id */
    hid_t sid;		/* dataspace id */
    int	ndims;		/* # of dimensions in the dataspace */
    int i, u;		/* local index variable */
    hsize_t cur_dims[H5S_MAX_RANK];	/* current dimension sizes */
    hsize_t prev_dims[H5S_MAX_RANK];	/* previous dimension sizes */
    int buf[TEST_BUF_SIZE];		/* Buffer for data */
    herr_t ret_value = 0;		/* return value */

    /* Open the file with SWMR */
    if((fid = H5Fopen(fname, H5F_ACC_SWMR_READ, H5P_DEFAULT)) < 0)
        goto done;

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
	    if(cur_dims[u] != prev_dims[u])
		break;
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

	/* Sleep before next monitor */
        sleep(1);
    } /* end while */

done:
    /* Closing */
    H5E_BEGIN_TRY
	H5Sclose(sid);
	H5Dclose(did);
    H5E_END_TRY

    return(ret_value);
} /* monitor_dset() */

/* usage: monitor xx.h5 dname */
int
main(int argc, const char *argv[])
{
    char *dname = NULL;	/* dataset name */
    char *fname = NULL;	/* file name */

    if(argc != 3) {
        HDfprintf(stderr, "Should have file name and dataset name to be monitored...\n");
        goto done;
    }

    /* Get the file name and dataset name to be extended */
    fname = strdup(argv[1]);
    dname = strdup(argv[2]);

    /* only integer dataset */
    if(monitor_dset(fname, dname) < 0)
	goto done;

    exit(EXIT_SUCCESS);

done:
    if(dname) free(dname);
    if(fname) free(fname);
    exit(EXIT_FAILURE);
}
