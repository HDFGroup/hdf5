/*
 * Copyright (C) 1998 Spizella Software
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, January 30, 1998
 *
 * Purpose:	Tests extendable datasets.
 */
#include <assert.h>
#include <hdf5.h>

#define NX	100		/* USE AN EVEN NUMBER!*/
#define NY	100		/* USE AN EVEN NUMBER!*/


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Tests extendable datasets
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
    hid_t		file, dataset, mem_space, file_space, cparms;
    herr_t		status;
    int			i, j, k, m;
    static int		buf1[NY][NX], buf2[NX/2][NY/2];
    static const size_t	dims[2] = {NX, NY};
    static const size_t half_dims[2] = {NX/2, NY/2};
    static const size_t chunk_dims[2] = {NX/2, NY/2};
    static size_t	maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    static size_t	size[2];
    int			offset[2];

    /* Initialize buffer and space */
    for (i=0; i<NX; i++) {
	for (j=0; j<NY; j++) {
	    buf1[i][j] = i*NY+j;
	}
    }
    mem_space = H5Screate_simple (2, dims, maxdims);
    assert (mem_space>=0);

    /* Create the file */
    file = H5Fcreate ("extend.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    assert (file>=0);

    /* Create the dataset which is originally NX by NY */
    cparms = H5Pcreate (H5P_DATASET_CREATE);
    assert (cparms>=0);
    status = H5Pset_chunk (cparms, 2, chunk_dims);
    assert (status>=0);
    dataset = H5Dcreate (file, "dataset", H5T_NATIVE_INT, mem_space, cparms);
    assert (dataset>=0);

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
	    status = H5Sset_hyperslab (file_space, offset, dims, NULL);
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
	    status = H5Sset_hyperslab (file_space, offset, half_dims, NULL);
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
    
    return 0;
}

    
    
