/*
 * Copyright (C) 1998 Spizella Software
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, January 30, 1998
 *
 * Purpose:	Tests extendible datasets.
 */

#include <h5test.h>

const char *FILENAME[] = {
    "extend",
    NULL
};

#define NX	100		/* USE AN EVEN NUMBER!*/
#define NY	100		/* USE AN EVEN NUMBER!*/


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
    hid_t			fapl;
    int				i, j, k, m;
    static int			buf1[NY][NX], buf2[NX/2][NY/2];
    static const hsize_t	dims[2] = {NX, NY};
    static const hsize_t 	half_dims[2] = {NX/2, NY/2};
    static const hsize_t 	chunk_dims[2] = {NX/2, NY/2};
    static hsize_t		maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    static hsize_t		size[2];
    hssize_t			offset[2];
    char			filename[1024];

    h5_reset();
    fapl = h5_fileaccess();
    TESTING("dataset extend");

    /* Initialize buffer and space */
    for (i=0; i<NX; i++) {
	for (j=0; j<NY; j++) {
	    buf1[i][j] = i*NY+j;
	}
    }
    if ((mem_space = H5Screate_simple (2, dims, maxdims))<0) goto error;

    /* Create the file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file = H5Fcreate (filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) {
	goto error;
    }
    
    /* Create the dataset which is originally NX by NY */
    if ((cparms = H5Pcreate (H5P_DATASET_CREATE))<0) goto error;
    if (H5Pset_chunk (cparms, 2, chunk_dims)<0) goto error;
    if ((dataset = H5Dcreate (file, "dataset", H5T_NATIVE_INT, mem_space,
			      cparms))<0) goto error;
    if (H5Pclose (cparms)<0) goto error;

    /* Write the data */
    for (i=0; i<5; i++) {
	for (j=0; j<5; j++) {

	    /* Extend the dataset */
	    offset[0] = i * NX;
	    offset[1] = j * NY;
	    size[0] = offset[0] + NX;
	    size[1] = offset[1] + NY;
	    if (H5Dextend (dataset, size)<0) goto error;

	    /* Select a hyperslab */
	    if ((file_space = H5Dget_space (dataset))<0) goto error;
	    if (H5Sselect_hyperslab (file_space, H5S_SELECT_SET, offset,
				     NULL, dims, NULL)<0) goto error;

	    /* Write to the hyperslab */
	    if (H5Dwrite (dataset, H5T_NATIVE_INT, mem_space, file_space,
			  H5P_DEFAULT, buf1)<0) goto error;
	    if (H5Sclose (file_space)<0) goto error;
	}
    }
    if (H5Sclose (mem_space)<0) goto error;

    /* Read the data */
    if ((mem_space = H5Screate_simple (2, half_dims, NULL))<0) goto error;
    if ((file_space = H5Dget_space (dataset))<0) goto error;
    for (i=0; i<10; i++) {
	for (j=0; j<10; j++) {

	    /* Select a hyperslab */
	    offset[0] = i * NX/2;
	    offset[1] = j * NY/2;
	    if (H5Sselect_hyperslab (file_space, H5S_SELECT_SET, offset,
				     NULL, half_dims, NULL)<0) goto error;
	    
	    /* Read */
	    if (H5Dread (dataset, H5T_NATIVE_INT, mem_space, file_space,
			 H5P_DEFAULT, buf2)<0) goto error;

	    /* Compare */
	    for (k=0; k<NX/2; k++) {
		for (m=0; m<NY/2; m++) {
		    if (buf2[k][m]!=buf1[(i%2)*NX/2+k][(j%2)*NY/2+m]) {
			FAILED();
			printf("    i=%d, j=%d, k=%d, m=%d\n", i, j, k, m);
			goto error;
		    }
		}
	    }
	}
    }
    

    if (H5Dclose (dataset)<0) goto error;
    if (H5Fclose (file)<0) goto error;
    PASSED();
    printf("All extend tests passed.\n");
    h5_cleanup(FILENAME, fapl);
    return 0;

 error:
    printf("*** One or more extend tests failed ***\n");
    return 1;
}

