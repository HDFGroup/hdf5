/*
 * Copyright (C) 1997 NCSA
 *		      All rights reserved.
 *
 * Programmer:	Robb Matzke <matzke@llnl.gov>
 *		Tuesday, December  9, 1997
 *
 * Purpose:	Tests the dataset interface (H5D)
 */

/* See H5private.h for how to include files */
#undef NDEBUG
#include <hdf5.h>

#ifdef STDC_HEADERS
#   include <assert.h>
#   include <math.h>
#   include <stdio.h>
#   include <stdlib.h>
#   include <string.h>
#endif

#ifdef HAVE_UNISTD_H
#   include <sys/types.h>
#   include <unistd.h>
#endif

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

#define TEST_FILE_NAME		"dataset.h5"
#define DSET_DEFAULT_NAME	"default"
#define DSET_CHUNKED_NAME	"chunked"
#define DSET_SIMPLE_IO_NAME	"simple_io"
#define DSET_TCONV_NAME		"tconv"
#define DSET_COMPRESS_NAME	"compressed"
#define DSET_BOGUS_NAME		"bogus"

#define H5Z_BOGUS		305


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

    /* Add a comment to the dataset */
    status = H5Gset_comment(file, DSET_DEFAULT_NAME, "This is a dataset");
    if (status<0) goto error;

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
    H5Pclose (create_parms);

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

    H5Pclose (xfer);
    H5Dclose(dataset);
    free (tconv_buf);
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
    hid_t	space, dataset;
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
    dataset = H5Dcreate(file, DSET_TCONV_NAME, H5T_STD_I32LE, space,
			H5P_DEFAULT);
    assert(dataset >= 0);

    /* Write the data to the dataset */
    status = H5Dwrite(dataset, H5T_STD_I32LE, H5S_ALL, H5S_ALL,
		      H5P_DEFAULT, out);
    assert(status >= 0);

    /* Read data with byte order conversion */
    status = H5Dread(dataset, H5T_STD_I32BE, H5S_ALL, H5S_ALL,
		     H5P_DEFAULT, in);
    assert(status >= 0);

    /* Check */
    for (i = 0; i < 1000000; i++) {
	assert(in[4 * i + 0] == out[4 * i + 3]);
	assert(in[4 * i + 1] == out[4 * i + 2]);
	assert(in[4 * i + 2] == out[4 * i + 1]);
	assert(in[4 * i + 3] == out[4 * i + 0]);
    }

    H5Dclose(dataset);
    free (out);
    free (in);

    puts(" PASSED");
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	bogus
 *
 * Purpose:	A bogus compression method that doesn't do anything.
 *
 * Return:	Success:	Data chunk size
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Tuesday, April 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
bogus(unsigned int __unused__ flags, size_t __unused__ cd_nelmts,
      const unsigned int __unused__ cd_values[], size_t nbytes,
      size_t __unused__ *buf_size, void __unused__ **buf)
{
#if 0
    abort();
#endif
    return nbytes;
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
    const hsize_t	size[2] = {100, 200};
    const hsize_t	chunk_size[2] = {2, 25};
    const hssize_t	hs_offset[2] = {7, 30};
    const hsize_t	hs_size[2] = {4, 50};
    
    hsize_t		i, j, n;
    void		*tconv_buf = NULL;

    printf ("%-70s", "Testing compression (setup)");
    fflush (stderr);
    
    /* Create the data space */
    space = H5Screate_simple(2, size, NULL);
    assert(space>=0);

    /*
     * Create a small conversion buffer to test strip mining. We
     * might as well test all we can!
     */
    xfer = H5Pcreate (H5P_DATASET_XFER);
    assert (xfer>=0);
    tconv_buf = malloc (1000);
    status = H5Pset_buffer (xfer, 1000, tconv_buf, NULL);
    assert (status>=0);

    /* Use chunked storage with compression */
    dc = H5Pcreate (H5P_DATASET_CREATE);
    H5Pset_chunk (dc, 2, chunk_size);
    H5Pset_deflate (dc, 6);

    /* Create the dataset */
    dataset = H5Dcreate(file, DSET_COMPRESS_NAME, H5T_NATIVE_INT, space, dc);
    assert(dataset >= 0);
    puts (" PASSED");

    /*----------------------------------------------------------------------
     * STEP 1: Read uninitialized data.  It should be zero.
     *---------------------------------------------------------------------- 
     */
    printf ("%-70s", "Testing compression (uninitialized read)");
    fflush (stdout);

    status = H5Dread (dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		      xfer, check);
    if (status<0) goto error;
    
    for (i=0; i<size[0]; i++) {
	for (j=0; j<size[1]; j++) {
	    if (0!=check[i][j]) {
		puts("*FAILED*");
		printf("   Read a non-zero value.\n");
		printf("   At index %lu,%lu\n",
		       (unsigned long)i, (unsigned long)j);
		goto error;
	    }
	}
    }
    puts (" PASSED");

    /*----------------------------------------------------------------------
     * STEP 2: Test compression by setting up a chunked dataset and writing
     * to it.
     *---------------------------------------------------------------------- 
     */
    printf("%-70s", "Testing compression (write)");
    fflush (stdout);
    
    for (i=n=0; i<size[0]; i++) {
	for (j=0; j<size[1]; j++) {
	    points[i][j] = n++;
	}
    }

    status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		      xfer, points);
    if (status<0) goto error;
    puts (" PASSED");

    /*----------------------------------------------------------------------
     * STEP 3: Try to read the data we just wrote.
     *---------------------------------------------------------------------- 
     */
    printf ("%-70s", "Testing compression (read)");
    fflush (stdout);

    /* Read the dataset back */
    status = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		     xfer, check);
    if (status<0) goto error;

    /* Check that the values read are the same as the values written */
    for (i=0; i<size[0]; i++) {
	for (j=0; j<size[1]; j++) {
	    if (points[i][j] != check[i][j]) {
		puts("*FAILED*");
		printf("   Read different values than written.\n");
		printf("   At index %lu,%lu\n",
		       (unsigned long)i, (unsigned long)j);
		goto error;
	    }
	}
    }
    puts (" PASSED");

    /*----------------------------------------------------------------------
     * STEP 4: Write new data over the top of the old data.  The new data is
     * random thus not very compressible, and will cause the chunks to move
     * around as they grow.  We only change values for the left half of the
     * dataset although we rewrite the whole thing.
     *---------------------------------------------------------------------- 
     */
    printf ("%-70s", "Testing compression (modify)");
    fflush (stdout);
    
    for (i=0; i<size[0]; i++) {
	for (j=0; j<size[1]/2; j++) {
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
    for (i=0; i<size[0]; i++) {
	for (j=0; j<size[1]; j++) {
	    if (points[i][j] != check[i][j]) {
		puts("*FAILED*");
		printf("   Read different values than written.\n");
		printf("   At index %lu,%lu\n",
		       (unsigned long)i, (unsigned long)j);
		goto error;
	    }
	}
    }
    puts (" PASSED");

    /*----------------------------------------------------------------------
     * STEP 5: Close the dataset and then open it and read it again.  This
     * insures that the compression message is picked up properly from the
     * object header.
     *---------------------------------------------------------------------- 
     */
    printf ("%-70s", "Testing compression (re-open)");
    fflush (stdout);
    
    H5Dclose (dataset);
    dataset = H5Dopen (file, DSET_COMPRESS_NAME);
    status = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		     xfer, check);
    if (status<0) goto error;

    /* Check that the values read are the same as the values written */
    for (i=0; i<size[0]; i++) {
	for (j=0; j<size[1]; j++) {
	    if (points[i][j] != check[i][j]) {
		puts("*FAILED*");
		printf("   Read different values than written.\n");
		printf("   At index %lu,%lu\n",
		       (unsigned long)i, (unsigned long)j);
		goto error;
	    }
	}
    }
    puts (" PASSED");


    /*----------------------------------------------------------------------
     * STEP 6: Test partial I/O by writing to and then reading from a
     * hyperslab of the dataset.  The hyperslab does not line up on chunk
     * boundaries (we know that case already works from above tests).
     *---------------------------------------------------------------------- 
     */
    printf ("%-70s", "Testing compression (partial I/O)");
    fflush (stderr);

    for (i=0; i<hs_size[0]; i++) {
	for (j=0; j<hs_size[1]; j++) {
	    points[hs_offset[0]+i][hs_offset[1]+j] = rand ();
	}
    }
    H5Sselect_hyperslab(space, H5S_SELECT_SET, hs_offset, NULL, hs_size, NULL);

    status = H5Dwrite (dataset, H5T_NATIVE_INT, space, space, xfer, points);
    if (status<0) goto error;
    status = H5Dread (dataset, H5T_NATIVE_INT, space, space, xfer, check);
    if (status<0) goto error;
    
    /* Check that the values read are the same as the values written */
    for (i=0; i<hs_size[0]; i++) {
	for (j=0; j<hs_size[1]; j++) {
	    if (points[hs_offset[0]+i][hs_offset[1]+j] !=
		check[hs_offset[0]+i][hs_offset[1]+j]) {
		puts("*FAILED*");
		printf("   Read different values than written.\n");
		printf("   At index %lu,%lu\n",
		       (unsigned long)(hs_offset[0]+i),
		       (unsigned long)(hs_offset[1]+j));
		printf("   At original: %d\n",
		       (int)points[hs_offset[0]+i][hs_offset[1]+j]);
		printf("   At returned: %d\n",
		       (int)check[hs_offset[0]+i][hs_offset[1]+j]);
		goto error;
	    }
	}
    }
    puts (" PASSED");

    /*----------------------------------------------------------------------
     * STEP 7: Register an application-defined compression method and use it
     * to write and then read the dataset.
     *---------------------------------------------------------------------- 
     */
    printf ("%-70s", "Testing compression (app-defined method)");
    fflush (stdout);

    if (H5Zregister (H5Z_BOGUS, "bogus", bogus)<0) goto error;
    if (H5Pset_filter (dc, H5Z_BOGUS, 0, 0, NULL)<0) goto error;
    if (H5Dclose (dataset)<0) goto error;
    if (H5Sclose (space)<0) goto error;
    if ((space = H5Screate_simple (2, size, NULL))<0) goto error;
    dataset = H5Dcreate (file, DSET_BOGUS_NAME, H5T_NATIVE_INT, space, dc);
    assert (dataset>=0);

    status = H5Dwrite (dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		       xfer, points);
    if (status<0) goto error;
    status = H5Dread (dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		      xfer, check);
    if (status<0) goto error;
    
    for (i=0; i<size[0]; i++) {
	for (j=0; j<size[1]; j++) {
	    if (points[i][j] != check[i][j]) {
		puts("*FAILED*");
		printf("   Read different values than written.\n");
		printf("   At index %lu,%lu\n",
		       (unsigned long)i, (unsigned long)j);
		goto error;
	    }
	}
    }
    puts (" PASSED");
    
			 
  
    /*----------------------------------------------------------------------
     * Cleanup
     *---------------------------------------------------------------------- 
     */
    H5Pclose (xfer);
    H5Pclose (dc);
    H5Dclose(dataset);
    free (tconv_buf);
    return 0;

  error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_multiopen
 *
 * Purpose:	Tests that a bug no longer exists.  If a dataset is opened
 *		twice and one of the handles is used to extend the dataset,
 *		then the other handle should return the new size when
 *		queried.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Tuesday, June  9, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_multiopen (hid_t file)
{
    hid_t		dcpl=-1, space=-1, dset1=-1, dset2=-1;
    hsize_t		cur_size[1] = {10};
    static hsize_t	max_size[1] = {H5S_UNLIMITED};
    hsize_t		tmp_size[1];

    printf ("%-70s", "Testing multi-open with extending");

    /* Create the dataset and open it twice */
    if ((dcpl=H5Pcreate (H5P_DATASET_CREATE))<0) goto error;
    if (H5Pset_chunk (dcpl, 1, cur_size)<0) goto error;
    if ((space=H5Screate_simple (1, cur_size, max_size))<0) goto error;
    if ((dset1=H5Dcreate (file, "multiopen", H5T_NATIVE_INT, space,
			  dcpl))<0) goto error;
    if ((dset2=H5Dopen (dset1, "."))<0) goto error;
    if (H5Sclose (space)<0) goto error;

    /* Extend with the first handle */
    cur_size[0] = 20;
    if (H5Dextend (dset1, cur_size)<0) goto error;

    /* Get the size from the second handle */
    if ((space = H5Dget_space (dset2))<0) goto error;
    if (H5Sget_simple_extent_dims (space, tmp_size, NULL)<0) goto error;
    if (cur_size[0]!=tmp_size[0]) {
	puts ("*FAILED*");
	printf ("   Got %d instead of %d!\n",
		(int)tmp_size[0], (int)cur_size[0]);
	goto error;
    }
    
    if (H5Dclose (dset1)<0) goto error;
    if (H5Dclose (dset2)<0) goto error;
    if (H5Sclose (space)<0) goto error;
    if (H5Pclose (dcpl)<0) goto error;
    puts (" PASSED");
    return 0;
    
 error:
    H5E_BEGIN_TRY {
	H5Dclose (dset1);
	H5Dclose (dset2);
	H5Sclose (space);
	H5Pclose (dcpl);
    } H5E_END_TRY;
    return -1;
}


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
    hid_t		file, grp, fapl;
    herr_t		status;
    int			nerrors=0, mdc_nelmts;

    status = H5open ();
    assert (status>=0);

    /* Automatic error reporting to standard output */
    H5Eset_auto (display_error_cb, NULL);

    /* Create the file */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    assert(fapl>=0);

#if 1
    /* Turn off raw data cache */
    status = H5Pget_cache(fapl, &mdc_nelmts, NULL, NULL, NULL);
    assert(status>=0);
    status = H5Pset_cache(fapl, mdc_nelmts, 0, 0, 0.0);
    assert(status>=0);
#endif
    
    file = H5Fcreate(TEST_FILE_NAME, H5F_ACC_TRUNC|H5F_ACC_DEBUG,
		     H5P_DEFAULT, fapl);
    assert(file >= 0);
    H5Pclose(fapl);

    /* Cause the library to emit initial messages */
    grp = H5Gcreate (file, "emit diagnostics", 0);
    H5Gset_comment(grp, ".", "Causes diagnostic messages to be emitted");
    H5Gclose (grp);

    status = test_create(file);
    nerrors += status < 0 ? 1 : 0;

    status = test_simple_io(file);
    nerrors += status < 0 ? 1 : 0;

    status = test_tconv(file);
    nerrors += status < 0 ? 1 : 0;

    status = test_compression(file);
    nerrors += status < 0 ? 1 : 0;

    status = test_multiopen (file);
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
    
    cleanup();
    return 0;
}
