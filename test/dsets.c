/*
 * Copyright (C) 1997 NCSA
 *		      All rights reserved.
 *
 * Programmer:	Robb Matzke <matzke@llnl.gov>
 *		Tuesday, December  9, 1997
 *
 * Purpose:	Tests the dataset interface (H5D)
 */
#include <assert.h>
#include <hdf5.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <H5config.h>
#ifndef HAVE_ATTRIBUTE
#   undef __attribute__
#   define __attribute__(X) /*void*/
#   define __unused__ /*void*/
#else
#   define __unused__ __attribute__((unused))
#endif

#ifndef HAVE_FUNCTION
#   undef __FUNCTION__
#   define __FUNCTION__ ""
#endif

#define AT() printf ("	 at %s:%d in %s()...\n",			    \
		     __FILE__, __LINE__, __FUNCTION__);

#define DSET_DEFAULT_NAME	"default"
#define DSET_CHUNKED_NAME	"chunked"
#define DSET_SIMPLE_IO_NAME	"simple_io"
#define DSET_TCONV_NAME		"tconv"
#define DSET_COMPRESS_NAME	"compressed"


/*-------------------------------------------------------------------------
 * Function:	display_error_cb
 *
 * Purpose:	Displays the error stack after printing "*FAILED*".
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		Wednesday, March  4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
display_error_cb (void __unused__ *client_data)
{
    puts ("*FAILED*");
    H5Eprint (stdout);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	test_create
 *
 * Purpose:	Attempts to create a dataset.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_create(hid_t file)
{
    hid_t	dataset, space, create_parms;
    hsize_t	dims[2];
    herr_t	status;
    hsize_t	csize[2];
    herr_t	(*func)(void*) = NULL;
    void	*client_data = NULL;

    printf("%-70s", "Testing create/open/close");
    fflush (stdout);

    /* Create the data space */
    dims[0] = 256;
    dims[1] = 512;
    space = H5Screate_simple(2, dims, NULL);
    assert(space>=0);

    /*
     * Create a dataset using the default dataset creation properties.	We're
     * not sure what they are, so we won't check.
     */
    dataset = H5Dcreate(file, DSET_DEFAULT_NAME, H5T_NATIVE_DOUBLE, space,
			H5P_DEFAULT);
    if (dataset<0) goto error;

    /* Close the dataset */
    if (H5Dclose(dataset) < 0) goto error;

    /*
     * Try creating a dataset that already exists.  This should fail since a
     * dataset can only be created once.  Temporarily turn off error
     * reporting.
     */
    H5Eget_auto (&func, &client_data);
    H5Eset_auto (NULL, NULL);
    dataset = H5Dcreate(file, DSET_DEFAULT_NAME, H5T_NATIVE_DOUBLE, space,
			H5P_DEFAULT);
    H5Eset_auto (func, client_data);
    if (dataset >= 0) {
	puts("*FAILED*");
	printf("   Library allowed overwrite of existing dataset.\n");
	goto error;
    }
    
    /*
     * Open the dataset we created above and then close it.  This is how
     * existing datasets are accessed.
     */
    if ((dataset = H5Dopen(file, DSET_DEFAULT_NAME))<0) goto error;
    if (H5Dclose(dataset) < 0) goto error;
    
    /*
     * Try opening a non-existent dataset. This should fail since new datasets
     * cannot be created with this function.  Temporarily turn off error
     * reporting.
     */
    H5Eget_auto (&func, &client_data);
    H5Eset_auto (NULL, NULL);
    dataset = H5Dopen(file, "does_not_exist");
    H5Eset_auto (func, client_data);
    if (dataset >= 0) {
	puts("*FAILED*");
	printf("   Opened a non-existent dataset.\n");
	goto error;
    }

    /*
     * Create a new dataset that uses chunked storage instead of the default
     * layout.
     */
    create_parms = H5Pcreate(H5P_DATASET_CREATE);
    assert(create_parms >= 0);
    csize[0] = 5;
    csize[1] = 100;
    status = H5Pset_chunk(create_parms, 2, csize);
    assert(status >= 0);

    dataset = H5Dcreate(file, DSET_CHUNKED_NAME, H5T_NATIVE_DOUBLE, space,
			create_parms);
    if (dataset < 0) goto error;

    /*
     * Close the chunked dataset.
     */
    if (H5Dclose(dataset) < 0) goto error;

    puts(" PASSED");
    return 0;

 error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_simple_io
 *
 * Purpose:	Tests simple I/O.  That is, reading and writing a complete
 *		multi-dimensional array without data type or data space
 *		conversions, without compression, and stored contiguously.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		Wednesday, December 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_simple_io(hid_t file)
{
    hid_t		dataset, space, xfer;
    herr_t		status;
    int			points[100][200], check[100][200];
    int			i, j, n;
    hsize_t		dims[2];
    void		*tconv_buf = NULL;

    printf("%-70s", "Testing simple I/O");
    fflush (stdout);

    /* Initialize the dataset */
    for (i = n = 0; i < 100; i++) {
	for (j = 0; j < 100; j++) {
	    points[i][j] = n++;
	}
    }

    /* Create the data space */
    dims[0] = 100;
    dims[1] = 200;
    space = H5Screate_simple(2, dims, NULL);
    assert(space>=0);

    /* Create a small conversion buffer to test strip mining */
    tconv_buf = malloc (1000);
    xfer = H5Pcreate (H5P_DATASET_XFER);
    assert (xfer>=0);
    status = H5Pset_buffer (xfer, 1000, tconv_buf, NULL);
    assert (status>=0);

    /* Create the dataset */
    dataset = H5Dcreate(file, DSET_SIMPLE_IO_NAME, H5T_NATIVE_INT, space,
			H5P_DEFAULT);
    assert(dataset >= 0);

    /* Write the data to the dataset */
    status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		      xfer, points);
    if (status<0) goto error;

    /* Read the dataset back */
    status = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		     xfer, check);
    if (status<0) goto error;

    /* Check that the values read are the same as the values written */
    for (i = 0; i < 100; i++) {
	for (j = 0; j < 200; j++) {
	    if (points[i][j] != check[i][j]) {
		puts("*FAILED*");
		printf("   Read different values than written.\n");
		printf("   At index %d,%d\n", i, j);
		goto error;
	    }
	}
    }

    H5Dclose(dataset);

    puts(" PASSED");
    return 0;

  error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:	test_tconv
 *
 * Purpose:	Test some simple data type conversion stuff.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January 14, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_tconv(hid_t file)
{
    char	*out=NULL, *in=NULL;
    int		i;
    hsize_t	dims[1];
    hid_t	space, dataset, type;
    herr_t	status;

    out = malloc (4*1000000);
    assert (out);
    in = malloc (4*1000000);
    assert (in);

    printf("%-70s", "Testing data type conversion");
    fflush (stdout);
    
    /* Initialize the dataset */
    for (i = 0; i < 1000000; i++) {
	out[i*4+0] = 0x11;
	out[i*4+1] = 0x22;
	out[i*4+2] = 0x33;
	out[i*4+3] = 0x44;
    }

    /* Create the data space */
    dims[0] = 1000000;
    space = H5Screate_simple (1, dims, NULL);
    assert(space >= 0);

    /* Create the data set */
    dataset = H5Dcreate(file, DSET_TCONV_NAME, H5T_NATIVE_INT32, space,
			H5P_DEFAULT);
    assert(dataset >= 0);

    /* Write the data to the dataset */
    status = H5Dwrite(dataset, H5T_NATIVE_INT32, H5S_ALL, H5S_ALL,
		      H5P_DEFAULT, out);
    assert(status >= 0);

    /* Create a new type with the opposite byte order */
    type = H5Tcopy(H5T_NATIVE_INT32);
    switch (H5Tget_order(type)) {
    case H5T_ORDER_BE:
	H5Tset_order(type, H5T_ORDER_LE);
	break;
    case H5T_ORDER_LE:
	H5Tset_order(type, H5T_ORDER_BE);
	break;
    default:
	assert("funny byte order" && 0);
	break;
    }

    /* Read data with byte order conversion */
    status = H5Dread(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, in);
    assert(status >= 0);

    /* Check */
    for (i = 0; i < 1000000; i++) {
	assert(in[4 * i + 0] == out[4 * i + 3]);
	assert(in[4 * i + 1] == out[4 * i + 2]);
	assert(in[4 * i + 2] == out[4 * i + 1]);
	assert(in[4 * i + 3] == out[4 * i + 0]);
    }

    H5Dclose(dataset);
    H5Tclose(type);

    puts(" PASSED");
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	test_compression
 *
 * Purpose:	Tests dataset compression.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 15, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_compression(hid_t file)
{
    hid_t		dataset, space, xfer, dc;
    herr_t		status;
    int			points[100][200], check[100][200];
    int			i, j, n;
    hsize_t		dims[2], chunk_size[2];
    void		*tconv_buf = NULL;

    printf("%-70s", "Testing compression");
    fflush (stdout);
    
    /* Initialize the dataset */
    for (i = n = 0; i < 100; i++) {
	for (j = 0; j < 100; j++) {
	    points[i][j] = n++;
	}
    }

    /* Create the data space */
    dims[0] = 100;
    dims[1] = 200;
    space = H5Screate_simple(2, dims, NULL);
    assert(space>=0);

    /* Create a small conversion buffer to test strip mining */
    tconv_buf = malloc (1000);
    xfer = H5Pcreate (H5P_DATASET_XFER);
    assert (xfer>=0);
    status = H5Pset_buffer (xfer, 1000, tconv_buf, NULL);
    assert (status>=0);

    /* Use chunked storage with compression */
    dc = H5Pcreate (H5P_DATASET_CREATE);
    chunk_size[0] = 2;
    chunk_size[1] = 25;
    H5Pset_chunk (dc, 2, chunk_size);
    H5Pset_deflate (dc, 6);

    /* Create the dataset */
    dataset = H5Dcreate(file, DSET_COMPRESS_NAME, H5T_NATIVE_INT, space, dc);
    assert(dataset >= 0);

    /* Write the data to the dataset */
    status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		      xfer, points);
    if (status<0) goto error;

    /* Read the dataset back */
    status = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		     xfer, check);
    if (status<0) goto error;

    /* Check that the values read are the same as the values written */
    for (i = 0; i < 100; i++) {
	for (j = 0; j < 200; j++) {
	    if (points[i][j] != check[i][j]) {
		puts("*FAILED*");
		printf("   Read different values than written.\n");
		printf("   At index %d,%d\n", i, j);
		goto error;
	    }
	}
    }

    /*
     * Write some random data to the dataset, hopefully causing chunks to be
     * reallocated as they grow.
     */
    for (i=0; i<100; i++) {
	for (j=0; j<100; j++) {
	    points[i][j] = rand ();
	}
    }
    status = H5Dwrite (dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		       xfer, points);
    if (status<0) goto error;


    /* Read the dataset back and check it */
    status = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		     xfer, check);
    if (status<0) goto error;

    /* Check that the values read are the same as the values written */
    for (i = 0; i < 100; i++) {
	for (j = 0; j < 200; j++) {
	    if (points[i][j] != check[i][j]) {
		puts("*FAILED*");
		printf("   Read different values than written.\n");
		printf("   At index %d,%d\n", i, j);
		goto error;
	    }
	}
    }

    
    

    H5Dclose(dataset);

    puts(" PASSED");
    return 0;

  error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Tests the dataset interface (H5D)
 *
 * Return:	Success:	exit(0)
 *
 *		Failure:	exit(1)
 *
 * Programmer:	Robb Matzke
 *		Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t		    file, grp;
    herr_t		    status;
    int			    nerrors = 0;

    status = H5open ();
    assert (status>=0);

    /* Automatic error reporting to standard output */
    H5Eset_auto (display_error_cb, NULL);

    unlink("dataset.h5");
    file = H5Fcreate("dataset.h5", H5F_ACC_TRUNC|H5F_ACC_DEBUG,
		     H5P_DEFAULT, H5P_DEFAULT);
    assert(file >= 0);

    /* Cause the library to emit initial messages */
    grp = H5Gcreate (file, "emit diagnostics", 0);
    H5Gclose (grp);
    

    status = test_create(file);
    nerrors += status < 0 ? 1 : 0;

    status = test_simple_io(file);
    nerrors += status < 0 ? 1 : 0;

    status = test_tconv(file);
    nerrors += status < 0 ? 1 : 0;
    
    status = test_compression(file);
    nerrors += status < 0 ? 1 : 0;

    status = H5Fclose(file);

    if (nerrors) {
	printf("***** %d DATASET TEST%s FAILED! *****\n",
	       nerrors, 1 == nerrors ? "" : "S");
	exit(1);
    }
    printf("All dataset tests passed.\n");

    status = H5close ();
    assert (status>=0);
    
    return 0;
}
