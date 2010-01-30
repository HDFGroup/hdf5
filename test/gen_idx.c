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
 * Purpose:     This program is run to generate an HDF5 data file with datasets
 *              that use the B-tree indexing method.
 *
 *              To test compatibility, compile and run this program
 *              which will generate a file called "btree_idx_1_8.h5".
 *              Move it to the test directory in the current branch.
 *              The test: test_idx_compatible() in dsets.c will read it.
 */
#include <assert.h>
#include "hdf5.h"

const char *FILENAME[1] = {
    "btree_idx_1_8.h5"	/* file with datasets that use B-tree indexing method */
};

#define DSET		"dset"
#define DSET_FILTER	"dset_filter"

/*
 * Function: gen_idx_file
 *
 * Purpose: Create a file with datasets that use B-tree indexing:
 *   	one dataset: fixed dimension, chunked layout, w/o filters
 *     	one dataset: fixed dimension, chunked layout, w/ filters
 *
 */
static void gen_idx_file(void)
{
    hid_t	fapl;		    /* file access property id */
    hid_t	fid;	            /* file id */
    hid_t   	sid;	            /* space id */
    hid_t	dcpl;	    	    /* dataset creation property id */
    hid_t	did, did2; 	    	    /* dataset id */
    hsize_t 	dims[1] = {10};     /* dataset dimension */
    hsize_t 	c_dims[1] = {2};    /* chunk dimension */
    herr_t  	status;             /* return status */
    int		i;		    /* local index variable */
    int     	buf[10];            /* data buffer */


    /* Get a copy of the file aaccess property */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    assert(fapl >= 0);

    /* Set the "use the latest format" bounds for creating objects in the file */
    status  = H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);
    assert(status >= 0);

     /* Create dataset */
    fid = H5Fcreate(FILENAME[0], H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    assert(fid >= 0);

    /* Create data */
    for(i = 0; i < 10; i++)
	buf[i] = i;

    /* Set chunk */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    assert(dcpl >= 0);
    status = H5Pset_chunk(dcpl, 1, c_dims);
    assert(status >= 0);

    sid = H5Screate_simple(1, dims, NULL);
    assert(sid >= 0);

    /* Create a 1D dataset */
    did  = H5Dcreate2(fid, DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    assert(did >= 0);

    /* Write to the dataset */
    status = H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    assert(status >= 0);

#if defined (H5_HAVE_FILTER_DEFLATE)
    /* set deflate data */
    status = H5Pset_deflate(dcpl, 9);
    assert(status >= 0);

    /* Create and write the dataset */
    did2  = H5Dcreate2(fid, DSET_FILTER, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    assert(did2 >= 0);
    status = H5Dwrite(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    assert(status >= 0);

    /* Close the dataset */
    status = H5Dclose(did2);
    assert(status >= 0);

#endif

    /* Closing */
    status = H5Dclose(did);
    assert(status >= 0);
    status = H5Sclose(sid);
    assert(status >= 0);
    status = H5Pclose(dcpl);
    assert(status >= 0);
    status = H5Pclose(fapl);
    assert(status >= 0);
    status = H5Fclose(fid);
    assert(status >= 0);

} /* gen_idx_file() */

int main(void)
{
    gen_idx_file();

    return 0;
}
