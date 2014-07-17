#include "H5HLprivate2.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <unistd.h>
#include <termios.h>
#include <fcntl.h>
 
static void set_mode(int want_key)
{
    static struct termios old_val, new_val;

    if (!want_key) {
        HDtcsetattr(STDIN_FILENO, TCSANOW, &old_val);
        return;
    }
 
    HDtcgetattr(STDIN_FILENO, &old_val);
    new_val = old_val;
    new_val.c_lflag &= (tcflag_t)~(ICANON | ECHO);
    HDtcsetattr(STDIN_FILENO, TCSANOW, &new_val);
}
 
static int get_key(void)
{
    int c = 0;
    struct timeval tv;
    fd_set fs;

    tv.tv_sec = 0;
    tv.tv_usec = 0;
 
    FD_ZERO(&fs);
    FD_SET(STDIN_FILENO, &fs);
    HDselect(STDIN_FILENO + 1, &fs, 0, 0, &tv);
 
    if (FD_ISSET(STDIN_FILENO, &fs)) {
        c = HDgetchar();
        set_mode(0);
    }
    return c;
}

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
monitor_dset(const char *fname, const char *dname)
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

    /* Monitor for keypresses */
    set_mode(1);

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
	    HDmemcpy(prev_dims, cur_dims, (size_t)ndims * sizeof(hsize_t));
	}

	/* Sleep before next monitor */
        sleep(1);

        /* Check for keypress */
        if(get_key())
            break;
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
    if(argc != 3) {
        HDfprintf(stderr, "Should have file name and dataset name to be monitored...\n");
        goto done;
    }

    /* only integer dataset */
    if(monitor_dset(argv[1], argv[2]) < 0)
	goto done;

    exit(EXIT_SUCCESS);

done:
    exit(EXIT_FAILURE);
}

