/*
 * Copyright (C) 1997 NCSA
 *		      All rights reserved.
 *
 * Programmer:	Robb Matzke <matzke@llnl.gov>
 *		Tuesday, December  9, 1997
 *
 * Purpose:	Tests the dataset interface (H5D)
 */

#include <h5test.h>

const char *FILENAME[] = {
    "dataset",
    NULL
};

#define DSET_DEFAULT_NAME	"default"
#define DSET_CHUNKED_NAME	"chunked"
#define DSET_SIMPLE_IO_NAME	"simple_io"
#define DSET_TCONV_NAME		"tconv"
#define DSET_COMPRESS_NAME	"compressed"
#define DSET_BOGUS_NAME		"bogus"

#define H5Z_BOGUS		305


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

    TESTING("create, open, close");

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
    H5E_BEGIN_TRY {
	dataset = H5Dcreate(file, DSET_DEFAULT_NAME, H5T_NATIVE_DOUBLE, space,
			    H5P_DEFAULT);
    } H5E_END_TRY;
    if (dataset >= 0) {
	FAILED();
	puts("    Library allowed overwrite of existing dataset.");
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
    H5E_BEGIN_TRY {
	dataset = H5Dopen(file, "does_not_exist");
    } H5E_END_TRY;
    if (dataset >= 0) {
	FAILED();
	puts("    Opened a non-existent dataset.");
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

    PASSED();
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

    TESTING("simple I/O");

    /* Initialize the dataset */
    for (i = n = 0; i < 100; i++) {
	for (j = 0; j < 100; j++) {
	    points[i][j] = n++;
	}
    }

    /* Create the data space */
    dims[0] = 100;
    dims[1] = 200;
    if ((space = H5Screate_simple(2, dims, NULL))<0) goto error;

    /* Create a small conversion buffer to test strip mining */
    tconv_buf = malloc (1000);
    xfer = H5Pcreate (H5P_DATASET_XFER);
    assert (xfer>=0);
    if ((status = H5Pset_buffer (xfer, 1000, tconv_buf, NULL))<0) goto error;

    /* Create the dataset */
    if ((dataset = H5Dcreate(file, DSET_SIMPLE_IO_NAME, H5T_NATIVE_INT, space,
			     H5P_DEFAULT))<0) goto error;

    /* Write the data to the dataset */
    if (H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer, points)<0)
	goto error;

    /* Read the dataset back */
    if (H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer, check)<0)
	goto error;

    /* Check that the values read are the same as the values written */
    for (i = 0; i < 100; i++) {
	for (j = 0; j < 200; j++) {
	    if (points[i][j] != check[i][j]) {
		FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %d,%d\n", i, j);
		goto error;
	    }
	}
    }

    H5Pclose (xfer);
    H5Dclose(dataset);
    free (tconv_buf);
    PASSED();
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

    out = malloc (4*1000000);
    assert (out);
    in = malloc (4*1000000);
    assert (in);

    TESTING("data type conversion");
    
    /* Initialize the dataset */
    for (i = 0; i < 1000000; i++) {
	out[i*4+0] = 0x11;
	out[i*4+1] = 0x22;
	out[i*4+2] = 0x33;
	out[i*4+3] = 0x44;
    }

    /* Create the data space */
    dims[0] = 1000000;
    if ((space = H5Screate_simple (1, dims, NULL))<0) goto error;

    /* Create the data set */
    if ((dataset = H5Dcreate(file, DSET_TCONV_NAME, H5T_STD_I32LE, space,
			     H5P_DEFAULT))<0) goto error;

    /* Write the data to the dataset */
    if (H5Dwrite(dataset, H5T_STD_I32LE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
		 out)<0) goto error;

    /* Read data with byte order conversion */
    if (H5Dread(dataset, H5T_STD_I32BE, H5S_ALL, H5S_ALL, H5P_DEFAULT, in)<0)
	goto error;

    /* Check */
    for (i = 0; i < 1000000; i++) {
	if (in[4*i+0]!=out[4*i+3] ||
	    in[4*i+1]!=out[4*i+2] ||
	    in[4*i+2]!=out[4*i+1] ||
	    in[4*i+3]!=out[4*i+0]) {
	    FAILED();
	    puts("    Read with byte order conversion failed.");
	    goto error;
	}
    }

    if (H5Dclose(dataset)<0) goto error;
    free (out);
    free (in);
    puts(" PASSED");
    return 0;

 error:
    return -1;
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
    return nbytes;
}


/*-------------------------------------------------------------------------
 * Function:	test_compression
 *
 * Purpose:	Tests dataset compression. If compression is requested when
 *		it hasn't been compiled into the library (such as when
 *		updating an existing compressed dataset) then data is sent to
 *		the file uncompressed but no errors are returned.
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
    int			points[100][200], check[100][200];
    const hsize_t	size[2] = {100, 200};
    const hsize_t	chunk_size[2] = {2, 25};
    const hssize_t	hs_offset[2] = {7, 30};
    const hsize_t	hs_size[2] = {4, 50};
    const char		*not_supported;
    
    hsize_t		i, j, n;
    void		*tconv_buf = NULL;

    not_supported = "    Deflate compression is not supported.\n"
		    "    The zlib was not found when hdf5 was configured.";
    
    TESTING("compression (setup)");
    
    /* Create the data space */
    if ((space = H5Screate_simple(2, size, NULL))<0) goto error;

    /*
     * Create a small conversion buffer to test strip mining. We
     * might as well test all we can!
     */
    if ((xfer = H5Pcreate (H5P_DATASET_XFER))<0) goto error;
    tconv_buf = malloc (1000);
    if (H5Pset_buffer (xfer, 1000, tconv_buf, NULL)<0) goto error;

    /* Use chunked storage with compression */
    if ((dc = H5Pcreate (H5P_DATASET_CREATE))<0) goto error;
    if (H5Pset_chunk (dc, 2, chunk_size)<0) goto error;
    if (H5Pset_deflate (dc, 6)<0) goto error;

    /* Create the dataset */
    if ((dataset = H5Dcreate(file, DSET_COMPRESS_NAME, H5T_NATIVE_INT, space,
			     dc))<0) goto error;
#ifdef HAVE_COMPRESS2
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 1: Read uninitialized data.  It should be zero.
     *---------------------------------------------------------------------- 
     */
    TESTING("compression (uninitialized read)");

    if (H5Dread (dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer, check)<0)
	goto error;
    
    for (i=0; i<size[0]; i++) {
	for (j=0; j<size[1]; j++) {
	    if (0!=check[i][j]) {
		FAILED();
		printf("    Read a non-zero value.\n");
		printf("    At index %lu,%lu\n",
		       (unsigned long)i, (unsigned long)j);
		goto error;
	    }
	}
    }
#ifdef HAVE_COMPRESS2
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 2: Test compression by setting up a chunked dataset and writing
     * to it.
     *---------------------------------------------------------------------- 
     */
    TESTING("compression (write)");
    
    for (i=n=0; i<size[0]; i++) {
	for (j=0; j<size[1]; j++) {
	    points[i][j] = n++;
	}
    }

    if (H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer, points)<0)
	goto error;
#ifdef HAVE_COMPRESS2
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 3: Try to read the data we just wrote.
     *---------------------------------------------------------------------- 
     */
    TESTING("compression (read)");

    /* Read the dataset back */
    if (H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer, check)<0)
	goto error;

    /* Check that the values read are the same as the values written */
    for (i=0; i<size[0]; i++) {
	for (j=0; j<size[1]; j++) {
	    if (points[i][j] != check[i][j]) {
		FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %lu,%lu\n",
		       (unsigned long)i, (unsigned long)j);
		goto error;
	    }
	}
    }
#ifdef HAVE_COMPRESS2
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 4: Write new data over the top of the old data.  The new data is
     * random thus not very compressible, and will cause the chunks to move
     * around as they grow.  We only change values for the left half of the
     * dataset although we rewrite the whole thing.
     *---------------------------------------------------------------------- 
     */
    TESTING("compression (modify)");
    
    for (i=0; i<size[0]; i++) {
	for (j=0; j<size[1]/2; j++) {
	    points[i][j] = rand ();
	}
    }
    if (H5Dwrite (dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer, points)<0)
	goto error;

    /* Read the dataset back and check it */
    if (H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer, check)<0)
	goto error;

    /* Check that the values read are the same as the values written */
    for (i=0; i<size[0]; i++) {
	for (j=0; j<size[1]; j++) {
	    if (points[i][j] != check[i][j]) {
		FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %lu,%lu\n",
		       (unsigned long)i, (unsigned long)j);
		goto error;
	    }
	}
    }
#ifdef HAVE_COMPRESS2
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 5: Close the dataset and then open it and read it again.  This
     * insures that the compression message is picked up properly from the
     * object header.
     *---------------------------------------------------------------------- 
     */
    TESTING("compression (re-open)");
    
    if (H5Dclose (dataset)<0) goto error;
    if ((dataset = H5Dopen (file, DSET_COMPRESS_NAME))<0) goto error;
    if (H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer, check)<0)
	goto error;

    /* Check that the values read are the same as the values written */
    for (i=0; i<size[0]; i++) {
	for (j=0; j<size[1]; j++) {
	    if (points[i][j] != check[i][j]) {
		FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %lu,%lu\n",
		       (unsigned long)i, (unsigned long)j);
		goto error;
	    }
	}
    }
#ifdef HAVE_COMPRESS2
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif
    

    /*----------------------------------------------------------------------
     * STEP 6: Test partial I/O by writing to and then reading from a
     * hyperslab of the dataset.  The hyperslab does not line up on chunk
     * boundaries (we know that case already works from above tests).
     *---------------------------------------------------------------------- 
     */
    TESTING("compression (partial I/O)");

    for (i=0; i<hs_size[0]; i++) {
	for (j=0; j<hs_size[1]; j++) {
	    points[hs_offset[0]+i][hs_offset[1]+j] = rand ();
	}
    }
    if (H5Sselect_hyperslab(space, H5S_SELECT_SET, hs_offset, NULL, hs_size,
			    NULL)<0) goto error;
    if (H5Dwrite (dataset, H5T_NATIVE_INT, space, space, xfer, points)<0)
	goto error;
    if (H5Dread (dataset, H5T_NATIVE_INT, space, space, xfer, check)<0)
	goto error;
    
    /* Check that the values read are the same as the values written */
    for (i=0; i<hs_size[0]; i++) {
	for (j=0; j<hs_size[1]; j++) {
	    if (points[hs_offset[0]+i][hs_offset[1]+j] !=
		check[hs_offset[0]+i][hs_offset[1]+j]) {
		FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %lu,%lu\n",
		       (unsigned long)(hs_offset[0]+i),
		       (unsigned long)(hs_offset[1]+j));
		printf("    At original: %d\n",
		       (int)points[hs_offset[0]+i][hs_offset[1]+j]);
		printf("    At returned: %d\n",
		       (int)check[hs_offset[0]+i][hs_offset[1]+j]);
		goto error;
	    }
	}
    }
#ifdef HAVE_COMPRESS2
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 7: Register an application-defined compression method and use it
     * to write and then read the dataset.
     *---------------------------------------------------------------------- 
     */
    TESTING("compression (app-defined method)");

    if (H5Zregister (H5Z_BOGUS, "bogus", bogus)<0) goto error;
    if (H5Pset_filter (dc, H5Z_BOGUS, 0, 0, NULL)<0) goto error;
    if (H5Dclose (dataset)<0) goto error;
    if (H5Sclose (space)<0) goto error;
    if ((space = H5Screate_simple (2, size, NULL))<0) goto error;
    if ((dataset=H5Dcreate (file, DSET_BOGUS_NAME, H5T_NATIVE_INT, space,
			    dc))<0) goto error;

    if (H5Dwrite (dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer, points)<0)
	goto error;
    if (H5Dread (dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer, check)<0)
	goto error;
    
    for (i=0; i<size[0]; i++) {
	for (j=0; j<size[1]; j++) {
	    if (points[i][j] != check[i][j]) {
		FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %lu,%lu\n",
		       (unsigned long)i, (unsigned long)j);
		goto error;
	    }
	}
    }
    PASSED();
    
			 
  
    /*----------------------------------------------------------------------
     * Cleanup
     *---------------------------------------------------------------------- 
     */
    if (H5Pclose (xfer)<0) goto error;
    if (H5Pclose (dc)<0) goto error;
    if (H5Dclose(dataset)<0) goto error;
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

    TESTING("multi-open with extending");

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
	FAILED();
	printf ("    Got %d instead of %d!\n",
		(int)tmp_size[0], (int)cur_size[0]);
	goto error;
    }
    
    if (H5Dclose (dset1)<0) goto error;
    if (H5Dclose (dset2)<0) goto error;
    if (H5Sclose (space)<0) goto error;
    if (H5Pclose (dcpl)<0) goto error;
    PASSED();
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
    int			nerrors=0, mdc_nelmts;
    char		filename[1024];

    h5_reset();
    fapl = h5_fileaccess();
    
#if 0
    /* Turn off raw data cache */
    if (H5Pget_cache(fapl, &mdc_nelmts, NULL, NULL, NULL)<0) goto error;
    if (H5Pset_cache(fapl, mdc_nelmts, 0, 0, 0.0)<0) goto error;
#endif
    
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC|H5F_ACC_DEBUG,
			H5P_DEFAULT, fapl))<0) goto error;

    /* Cause the library to emit initial messages */
    if ((grp = H5Gcreate (file, "emit diagnostics", 0))<0) goto error;
    if (H5Gset_comment(grp, ".", "Causes diagnostic messages to be emitted")<0)
	goto error;
    if (H5Gclose (grp)<0) goto error;

    nerrors += test_create(file)<0 	?1:0;
    nerrors += test_simple_io(file)<0	?1:0;
    nerrors += test_tconv(file)<0	?1:0;
    nerrors += test_compression(file)<0	?1:0;
    nerrors += test_multiopen (file)<0	?1:0;

    if (H5Fclose(file)<0) goto error;
    if (nerrors) goto error;
    printf("All dataset tests passed.\n");
    h5_cleanup(fapl);
    return 0;

 error:
    nerrors = MAX(1, nerrors);
    printf("***** %d DATASET TEST%s FAILED! *****\n",
	   nerrors, 1 == nerrors ? "" : "S");
    return 1;
}
