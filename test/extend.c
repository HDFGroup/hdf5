/*
 * Copyright (C) 1998 Spizella Software
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, January 30, 1998
 *
 * Purpose:	Tests extendible datasets.
 */

/* See H5private.h for how to include headers */
#undef NDEBUG
#include <H5config.h>

#ifdef STDC_HEADERS
#   include <assert.h>
#   include <stdlib.h>
#endif

#include <hdf5.h>

#define TEST_FILE_NAME	"extend.h5"
#define NX	100		/* USE AN EVEN NUMBER!*/
#define NY	100		/* USE AN EVEN NUMBER!*/


/*-------------------------------------------------------------------------
 * Function:	cleanup
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Albert Cheng
 *              May 28, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
cleanup(void)
{
    if (!getenv ("HDF5_NOCLEANUP")) {
	remove(TEST_FILE_NAME);
    }
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Tests extendible datasets
 *
 * Return:	Success:	exit(0)
 *
 *		Failure:	exit(non-zero)
 *
 * Programmer:	Robb Matzke
 *              Friday, January 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main (void)
{
    hid_t			file, dataset, mem_space, file_space, cparms;
    herr_t			status;
    int				i, j, k, m;
    static int			buf1[NY][NX], buf2[NX/2][NY/2];
    static const hsize_t	dims[2] = {NX, NY};
    static const hsize_t 	half_dims[2] = {NX/2, NY/2};
    static const hsize_t 	chunk_dims[2] = {NX/2, NY/2};
    static hsize_t		maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    static hsize_t		size[2];
    hssize_t			offset[2];

    /* Initialize buffer and space */
    for (i=0; i<NX; i++) {
	for (j=0; j<NY; j++) {
	    buf1[i][j] = i*NY+j;
	}
    }
    mem_space = H5Screate_simple (2, dims, maxdims);
    assert (mem_space>=0);

    /* Create the file */
    file = H5Fcreate (TEST_FILE_NAME, H5F_ACC_TRUNC|H5F_ACC_DEBUG,
		      H5P_DEFAULT, H5P_DEFAULT);
    assert (file>=0);

    /* Create the dataset which is originally NX by NY */
    cparms = H5Pcreate (H5P_DATASET_CREATE);
    assert (cparms>=0);
    status = H5Pset_chunk (cparms, 2, chunk_dims);
    assert (status>=0);
    dataset = H5Dcreate (file, "dataset", H5T_NATIVE_INT, mem_space, cparms);
    assert (dataset>=0);
    H5Pclose (cparms);

    /* Write the data */
    for (i=0; i<5; i++) {
	for (j=0; j<5; j++) {

	    /* Extend the dataset */
	    offset[0] = i * NX;
	    offset[1] = j * NY;
	    size[0] = offset[0] + NX;
	    size[1] = offset[1] + NY;
	    status = H5Dextend (dataset, size);
	    assert (status>=0);

	    /* Select a hyperslab */
	    file_space = H5Dget_space (dataset);
	    assert (file_space>=0);
	    status = H5Sselect_hyperslab (file_space, H5S_SELECT_SET, offset, NULL, dims, NULL);
	    assert (status>=0);

	    /* Write to the hyperslab */
	    status = H5Dwrite (dataset, H5T_NATIVE_INT, mem_space, file_space,
			       H5P_DEFAULT, buf1);
	    assert (status>=0);
	    H5Sclose (file_space);
	}
    }
    H5Sclose (mem_space);


    /* Read the data */
    mem_space = H5Screate_simple (2, half_dims, NULL);
    file_space = H5Dget_space (dataset);
    for (i=0; i<10; i++) {
	for (j=0; j<10; j++) {

	    /* Select a hyperslab */
	    offset[0] = i * NX/2;
	    offset[1] = j * NY/2;
	    assert (file_space>=0);
	    status = H5Sselect_hyperslab (file_space, H5S_SELECT_SET, offset, NULL, half_dims, NULL);
	    assert (status>=0);
	    
	    /* Read */
	    status = H5Dread (dataset, H5T_NATIVE_INT, mem_space, file_space,
			      H5P_DEFAULT, buf2);
	    assert (status>=0);

	    /* Compare */
	    for (k=0; k<NX/2; k++) {
		for (m=0; m<NY/2; m++) {
		    assert (buf2[k][m]==buf1[(i%2)*NX/2+k][(j%2)*NY/2+m]);
		}
	    }
	}
    }
    

    H5Dclose (dataset);
    H5Fclose (file);
    printf("All extend tests passed.\n");
    cleanup();
    return 0;
}

