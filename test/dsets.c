/*
 * Copyright (C) 1997 NCSA
 *		      All rights reserved.
 *
 * Programmer:	Robb Matzke <matzke@llnl.gov>
 *		Tuesday, December  9, 1997
 *
 * Purpose:	Tests the dataset interface (H5D)
 */

#include "h5test.h"
#include "H5Zprivate.h"         /* Filter functions */

const char *FILENAME[] = {
    "dataset",
    "compact_dataset",
    NULL
};

#define FILE_DEFLATE_NAME       "deflate.h5"

#define DSET_DEFAULT_NAME	"default"
#define DSET_CHUNKED_NAME	"chunked"
#define DSET_COMPACT_NAME       "compact"
#define DSET_SIMPLE_IO_NAME	"simple_io"
#define DSET_COMPACT_IO_NAME    "compact_io"
#define DSET_TCONV_NAME		"tconv"
#define DSET_DEFLATE_NAME	"deflate"
#define DSET_SHUFFLE_NAME	"shuffle"
#define DSET_SHUFFLE_DEFLATE_NAME	"shuffle+deflate"
#define DSET_BOGUS_NAME		"bogus"
#define DSET_MISSING_NAME	"missing"
#define DSET_ONEBYTE_SHUF_NAME   "onebyte_shuffle"

#define H5Z_BOGUS		305

/* Shared global arrays */
int	points[100][200], check[100][200];


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
 *              Added test for compact dataset creation.
 *              Raymond Lu
 *              August 8, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_create(hid_t file)
{
    hid_t	dataset, space, small_space, create_parms;
    hsize_t	dims[2], small_dims[2];
    herr_t	status;
    hsize_t	csize[2];

    TESTING("create, open, close");

    /* Create the data space */
    dims[0] = 256;
    dims[1] = 512;
    space = H5Screate_simple(2, dims, NULL);
    assert(space>=0);

    /* Create a small data space for compact dataset */
    small_dims[0] = 16;
    small_dims[1] = 8;
    small_space = H5Screate_simple(2, small_dims, NULL);
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
	H5_FAILED();
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
	H5_FAILED();
	puts("    Opened a non-existent dataset.");
	goto error;
    }

    /*
     * Create a new dataset that uses chunked storage instead of the default
     * layout.
     */
    create_parms = H5Pcreate(H5P_DATASET_CREATE);
    assert(create_parms >= 0);

    /* Attempt to create a dataset with invalid chunk sizes */
    csize[0] = dims[0]*2;
    csize[1] = dims[1]*2;
    status = H5Pset_chunk(create_parms, 2, csize);
    assert(status >= 0);
    H5E_BEGIN_TRY {
        dataset = H5Dcreate(file, DSET_CHUNKED_NAME, H5T_NATIVE_DOUBLE, space,
			create_parms);
    } H5E_END_TRY;
    if (dataset >= 0) {
	H5_FAILED();
	puts("    Opened a dataset with incorrect chunking parameters.");
	goto error;
    }

    csize[0] = 5;
    csize[1] = 100;
    status = H5Pset_chunk(create_parms, 2, csize);
    assert(status >= 0);

    dataset = H5Dcreate(file, DSET_CHUNKED_NAME, H5T_NATIVE_DOUBLE, space,
			create_parms);
    if (dataset < 0) goto error;
    H5Pclose (create_parms);

    /* Test dataset address.  Should be undefined. */
    if(H5Dget_offset(dataset)!=HADDR_UNDEF) goto error;
    
    /*
     * Close the chunked dataset.
     */
    if (H5Dclose(dataset) < 0) goto error;

    /*
     * Create a compact dataset, then close it. 
     */
    create_parms = H5Pcreate(H5P_DATASET_CREATE);
    assert(create_parms >= 0);
    status = H5Pset_layout(create_parms, H5D_COMPACT);
    assert(status >= 0);
    status = H5Pset_alloc_time(create_parms, H5D_ALLOC_TIME_EARLY);
    assert(status >= 0); 

    dataset = H5Dcreate(file, DSET_COMPACT_NAME, H5T_NATIVE_DOUBLE, 
                        small_space, create_parms);
    if(dataset < 0) goto error;
    H5Pclose(create_parms);
    if(H5Dclose(dataset) <0) goto error;

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
    int			i, j, n;
    hsize_t		dims[2];
    void		*tconv_buf = NULL;

    TESTING("simple I/O");

    /* Initialize the dataset */
    for (i = n = 0; i < 100; i++) {
	for (j = 0; j < 200; j++) {
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
    if (H5Pset_buffer (xfer, 1000, tconv_buf, NULL)<0) goto error;

    /* Create the dataset */
    if ((dataset = H5Dcreate(file, DSET_SIMPLE_IO_NAME, H5T_NATIVE_INT, space,
			     H5P_DEFAULT))<0) goto error;

    /* Test dataset address.  Should be undefined. */
    if(H5Dget_offset(dataset)!=HADDR_UNDEF) goto error;
    
    /* Write the data to the dataset */
    if (H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer, points)<0)
	goto error;

    /* Test dataset address.  Should be valid. */
    if(H5Dget_offset(dataset)==HADDR_UNDEF) goto error;
    
    /* Read the dataset back */
    if (H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer, check)<0)
	goto error;

    /* Check that the values read are the same as the values written */
    for (i = 0; i < 100; i++) {
	for (j = 0; j < 200; j++) {
	    if (points[i][j] != check[i][j]) {
		H5_FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %d,%d\n", i, j);
		goto error;
	    }
	}
    }

    i=H5Pclose (xfer);
    H5Dclose(dataset);
    free (tconv_buf);
    PASSED();
    return 0;

  error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_compact_io
 *
 * Purpose:     Tests compact dataset I/O.  That is, reading and writing a 
 *              complete multi-dimensional array without data type or data 
 *              space conversions, without compression, and store in 
 *              compact dataset.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu 
 *              August 8, 2002 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_compact_io(hid_t fapl)
{
    hid_t       file, dataset, space, plist;
    hsize_t     dims[2];
    herr_t      status;
    int         wbuf[16][8], rbuf[16][8];
    char	filename[1024];
    int         i, j, n;

    TESTING("compact dataset I/O");

    /* Initialize data */
    n=0;
    for(i=0; i<16; i++) {
        for(j=0; j<8; j++) {
            wbuf[i][j] = n++;
        }
    }
    
    /* Create a small data space for compact dataset */
    dims[0] = 16;
    dims[1] = 8;
    space = H5Screate_simple(2, dims, NULL);
    assert(space>=0);

    /* Create a file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        goto error;

    /* Create property list for compact dataset creation */
    plist = H5Pcreate(H5P_DATASET_CREATE);
    assert(plist >= 0);
    status = H5Pset_layout(plist, H5D_COMPACT);
    assert(status >= 0);
    status = H5Pset_alloc_time(plist, H5D_ALLOC_TIME_EARLY); 
    assert(status >= 0);

    /* Create and write to a compact dataset */
    if((dataset = H5Dcreate(file, DSET_COMPACT_IO_NAME, H5T_NATIVE_INT, space, 
                        plist))<0)
        goto error;

    /* Test dataset address.  Should be undefined. */
    if(H5Dget_offset(dataset)!=HADDR_UNDEF) goto error;
    
    if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf)<0)
        goto error;

    /* Test dataset address.  Should be undefined. */
    if(H5Dget_offset(dataset)!=HADDR_UNDEF) goto error;
    
    /* Close file */
    H5Sclose(space);
    H5Pclose(plist);
    H5Dclose(dataset);
    H5Fclose(file);

    /*
     * Open the file and check data 
     */
    if((file=H5Fopen(filename, H5F_ACC_RDONLY, fapl))<0)
        goto error;
    if((dataset = H5Dopen(file, DSET_COMPACT_IO_NAME))<0)
        goto error;
    if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf)<0)
        goto error;

     /* Check that the values read are the same as the values written */
     for (i = 0; i < 16; i++) {
         for (j = 0; j < 8; j++) {
             if (rbuf[i][j] != wbuf[i][j]) {
                 H5_FAILED();
                 printf("    Read different values than written.\n");
                 printf("    At index %d,%d\n", i, j);
                 goto error;
             }
         }
     }

     H5Dclose(dataset);
     H5Fclose(file);
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
	    H5_FAILED();
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
bogus(unsigned int UNUSED flags, size_t UNUSED cd_nelmts,
      const unsigned int UNUSED *cd_values, size_t nbytes,
      size_t UNUSED *buf_size, void UNUSED **buf)
{
    return nbytes;
}


/*-------------------------------------------------------------------------
 * Function:	test_compression_internal
 *
 * Purpose:	Tests dataset compression. If compression is requested when
 *		it hasn't been compiled into the library (such as when
 *		updating an existing compressed dataset) then data is sent to
 *		the file uncompressed but no errors are returned.
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 15, 1998
 *
 * Modifications:
 *              Moved out of main test_compression routine
 *              Quincey Koziol, November 14, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_compression_internal(hid_t fid, const char *name, hid_t dcpl, hsize_t *dset_size)
{
    hid_t		dataset;        /* Dataset ID */
    hid_t		dxpl;           /* Dataset xfer property list ID */
    hid_t		sid;            /* Dataspace ID */
    const hsize_t	size[2] = {100, 200};           /* Dataspace dimensions */
    const hssize_t	hs_offset[2] = {7, 30}; /* Hyperslab offset */
    const hsize_t	hs_size[2] = {4, 50};   /* Hyperslab size */
    void		*tconv_buf = NULL;      /* Temporary conversion buffer */
    hsize_t		i, j, n;        /* Local index variables */

    /* Create the data space */
    if ((sid = H5Screate_simple(2, size, NULL))<0) goto error;

    /*
     * Create a small conversion buffer to test strip mining. We
     * might as well test all we can!
     */
    if ((dxpl = H5Pcreate (H5P_DATASET_XFER))<0) goto error;
    tconv_buf = malloc (1000);
    if (H5Pset_buffer (dxpl, 1000, tconv_buf, NULL)<0) goto error;

    TESTING("compression (setup)");
    
    /* Create the dataset */
    if ((dataset = H5Dcreate(fid, name, H5T_NATIVE_INT, sid,
			     dcpl))<0) goto error;
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 1: Read uninitialized data.  It should be zero.
     *---------------------------------------------------------------------- 
     */
    TESTING("compression (uninitialized read)");

    if (H5Dread (dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check)<0)
	goto error;
    
    for (i=0; i<size[0]; i++) {
	for (j=0; j<size[1]; j++) {
	    if (0!=check[i][j]) {
		H5_FAILED();
		printf("    Read a non-zero value.\n");
		printf("    At index %lu,%lu\n",
		       (unsigned long)i, (unsigned long)j);
		goto error;
	    }
	}
    }
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 2: Test compression by setting up a chunked dataset and writing
     * to it.
     *---------------------------------------------------------------------- 
     */
    TESTING("compression (write)");
    
    for (i=n=0; i<size[0]; i++) {
	for (j=0; j<size[1]; j++) {
	    points[i][j] = (int)(n++);
	}
    }

    if (H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, points)<0)
	goto error;
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 3: Try to read the data we just wrote.
     *---------------------------------------------------------------------- 
     */
    TESTING("compression (read)");

    /* Read the dataset back */
    if (H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check)<0)
	goto error;

    /* Check that the values read are the same as the values written */
    for (i=0; i<size[0]; i++) {
	for (j=0; j<size[1]; j++) {
	    if (points[i][j] != check[i][j]) {
		H5_FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %lu,%lu\n",
		       (unsigned long)i, (unsigned long)j);
		goto error;
	    }
	}
    }
    PASSED();

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
    if (H5Dwrite (dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, points)<0)
	goto error;

    /* Read the dataset back and check it */
    if (H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check)<0)
	goto error;

    /* Check that the values read are the same as the values written */
    for (i=0; i<size[0]; i++) {
	for (j=0; j<size[1]; j++) {
	    if (points[i][j] != check[i][j]) {
		H5_FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %lu,%lu\n",
		       (unsigned long)i, (unsigned long)j);
		goto error;
	    }
	}
    }
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 5: Close the dataset and then open it and read it again.  This
     * insures that the compression message is picked up properly from the
     * object header.
     *---------------------------------------------------------------------- 
     */
    TESTING("compression (re-open)");
    
    if (H5Dclose (dataset)<0) goto error;
    if ((dataset = H5Dopen (fid, name))<0) goto error;
    if (H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check)<0)
	goto error;

    /* Check that the values read are the same as the values written */
    for (i=0; i<size[0]; i++) {
	for (j=0; j<size[1]; j++) {
	    if (points[i][j] != check[i][j]) {
		H5_FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %lu,%lu\n",
		       (unsigned long)i, (unsigned long)j);
		goto error;
	    }
	}
    }
    PASSED();
    

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
    if (H5Sselect_hyperslab(sid, H5S_SELECT_SET, hs_offset, NULL, hs_size,
			    NULL)<0) goto error;
    if (H5Dwrite (dataset, H5T_NATIVE_INT, sid, sid, dxpl, points)<0)
	goto error;
    if (H5Dread (dataset, H5T_NATIVE_INT, sid, sid, dxpl, check)<0)
	goto error;
    
    /* Check that the values read are the same as the values written */
    for (i=0; i<hs_size[0]; i++) {
	for (j=0; j<hs_size[1]; j++) {
	    if (points[hs_offset[0]+i][hs_offset[1]+j] !=
		check[hs_offset[0]+i][hs_offset[1]+j]) {
		H5_FAILED();
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
    PASSED();

    /* Get the storage size of the dataset */
    if((*dset_size=H5Dget_storage_size(dataset))==0) goto error;

    /* Clean up objects used for this test */
    if (H5Dclose (dataset)<0) goto error;
    if (H5Sclose (sid)<0) goto error;
    if (H5Pclose (dxpl)<0) goto error;
    free (tconv_buf);

    return(0);

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_compression
 *
 * Purpose:	Tests dataset compression.
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 15, 1998
 *
 * Modifications:
 *              Moved guts of compression testing out of main routine.
 *              Also added tests for shuffle filter.
 *              Quincey Koziol, November 14, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_compression(hid_t file)
{
    hid_t	dc;                 /* Dataset creation property list ID */
    const hsize_t chunk_size[2] = {2, 25};  /* Chunk dimensions */
    hsize_t     null_size;          /* Size of dataset with null filter */
#ifdef H5_HAVE_FILTER_DEFLATE
    hsize_t     deflate_size;       /* Size of dataset with deflate filter */
#endif /* H5_HAVE_FILTER_DEFLATE */
#ifdef H5_HAVE_FILTER_SHUFFLE
    hsize_t     shuffle_size;       /* Size of dataset with shuffle filter */
#endif /* H5_HAVE_FILTER_SHUFFLE */
#if defined H5_HAVE_FILTER_DEFLATE && defined H5_HAVE_FILTER_SHUFFLE
    hsize_t     shuff_def_size;     /* Size of dataset with shuffle+deflate filter */
#endif /* H5_HAVE_FILTER_DEFLATE && H5_HAVE_FILTER_SHUFFLE */
    
    /* Test null I/O filter (by itself) */
    puts("Testing 'null' filter");
    if((dc = H5Pcreate(H5P_DATASET_CREATE))<0) goto error;
    if (H5Pset_chunk (dc, 2, chunk_size)<0) goto error;
    if (H5Zregister (H5Z_BOGUS, "bogus", bogus)<0) goto error;
    if (H5Pset_filter (dc, H5Z_BOGUS, 0, 0, NULL)<0) goto error;

    if(test_compression_internal(file,DSET_BOGUS_NAME,dc,&null_size)<0) goto error;

    /* Clean up objects used for this test */
    if (H5Pclose (dc)<0) goto error;

#ifdef H5_HAVE_FILTER_DEFLATE
    /* Test deflate I/O filter (by itself) */
    puts("Testing deflate filter");
    if((dc = H5Pcreate(H5P_DATASET_CREATE))<0) goto error;
    if (H5Pset_chunk (dc, 2, chunk_size)<0) goto error;
    if (H5Pset_deflate (dc, 6)<0) goto error;

    if(test_compression_internal(file,DSET_DEFLATE_NAME,dc,&deflate_size)<0) goto error;
    if(deflate_size>=null_size) {
        H5_FAILED();
        puts("    Deflated size greater than uncompressed size.");
        goto error;
    } /* end if */

    /* Clean up objects used for this test */
    if (H5Pclose (dc)<0) goto error;
#else /* H5_HAVE_FILTER_DEFLATE */
    TESTING("deflate filter");
    SKIPPED();
    puts("Deflate filter not enabled");
#endif /* H5_HAVE_FILTER_DEFLATE */

#ifdef H5_HAVE_FILTER_SHUFFLE
    /* Test shuffle I/O filter (by itself) */
    puts("Testing shuffle filter");
    if((dc = H5Pcreate(H5P_DATASET_CREATE))<0) goto error;
    if (H5Pset_chunk (dc, 2, chunk_size)<0) goto error;
    if (H5Pset_shuffle (dc, sizeof(int))<0) goto error;

    if(test_compression_internal(file,DSET_SHUFFLE_NAME,dc,&shuffle_size)<0) goto error;
    if(shuffle_size!=null_size) {
        H5_FAILED();
        puts("    Shuffled size not the same as uncompressed size.");
        goto error;
    } /* end if */

    /* Clean up objects used for this test */
    if (H5Pclose (dc)<0) goto error;
#else /* H5_HAVE_FILTER_SHUFFLE */
    TESTING("shuffle filter");
    SKIPPED();
    puts("Shuffle filter not enabled");
#endif /* H5_HAVE_FILTER_SHUFFLE */

#if defined H5_HAVE_FILTER_DEFLATE && defined H5_HAVE_FILTER_SHUFFLE
    /* Test combination of deflate & shuffle I/O filters */
    puts("Testing shuffle+deflate filters");
    if((dc = H5Pcreate(H5P_DATASET_CREATE))<0) goto error;
    if (H5Pset_chunk (dc, 2, chunk_size)<0) goto error;
    if (H5Pset_shuffle (dc, sizeof(int))<0) goto error;
    if (H5Pset_deflate (dc, 6)<0) goto error;

    if(test_compression_internal(file,DSET_SHUFFLE_DEFLATE_NAME,dc,&shuff_def_size)<0) goto error;
    if(shuff_def_size>=deflate_size) {
        H5_FAILED();
        puts("    Shuffle+deflate size greater than plain deflated size.");
        goto error;
    } /* end if */

    /* Clean up objects used for this test */
    if (H5Pclose (dc)<0) goto error;
#else /* H5_HAVE_FILTER_DEFLATE && H5_HAVE_FILTER_SHUFFLE */
    TESTING("shuffle+deflate filters");
    SKIPPED();
    puts("Deflate or shuffle filter not enabled");
#endif /* H5_HAVE_FILTER_DEFLATE && H5_HAVE_FILTER_SHUFFLE */

    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_missing_filter
 *
 * Purpose:	Tests library behavior when filter is missing
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, November 14, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_missing_filter(hid_t file)
{
    hid_t       fid;            /* File ID */
    hid_t       dsid;           /* Dataset ID */
    hid_t       sid;            /* Dataspace ID */
    hid_t       dcpl;           /* Dataspace creation property list ID */
    const hsize_t dims[2] = {100, 200};         /* Dataspace dimensions */
    const hsize_t chunk_dims[2] = {2, 25};      /* Chunk dimensions */
    hsize_t     dset_size;      /* Dataset size */
    hsize_t     i,j;            /* Local index variables */
    herr_t      ret;            /* Generic return value */
    char testfile[512]="";      /* Buffer to hold name of existing test file */
    char *srcdir = HDgetenv("srcdir");    /* The source directory, if we are using the --srcdir configure option */

    TESTING("dataset access with missing filter");

    /* Unregister the deflate filter */
#ifdef H5_HAVE_FILTER_DEFLATE
        /* Verify deflate filter is registered currently */
        if(H5Zfilter_avail(H5Z_FILTER_DEFLATE)!=TRUE) {
            H5_FAILED();
            printf("    Line %d: Deflate filter not available\n",__LINE__);
            goto error;
        } /* end if */

        /* Unregister deflate filter (use internal function) */
        if (H5Z_unregister(H5Z_FILTER_DEFLATE)<0) {
            H5_FAILED();
            printf("    Line %d: Can't unregister deflate filter\n",__LINE__);
            goto error;
        } /* end if */
#endif /* H5_HAVE_FILTER_DEFLATE */
        /* Verify deflate filter is not registered currently */
        if(H5Zfilter_avail(H5Z_FILTER_DEFLATE)!=FALSE) {
            H5_FAILED();
            printf("    Line %d: Deflate filter available\n",__LINE__);
            goto error;
        } /* end if */

    /* Create dcpl with deflate filter */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0) {
        H5_FAILED();
        printf("    Line %d: Can't create dcpl\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Pset_chunk(dcpl, 2, chunk_dims)<0) {
        H5_FAILED();
        printf("    Line %d: Can't set chunk sizes\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Pset_deflate(dcpl, 9)<0) {
        H5_FAILED();
        printf("    Line %d: Can't set deflate filter\n",__LINE__);
        goto error;
    } /* end if */

    /* Create the data space */
    if ((sid = H5Screate_simple(2, dims, NULL))<0) {
        H5_FAILED();
        printf("    Line %d: Can't open dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Create new dataset */
    if ((dsid = H5Dcreate(file, DSET_MISSING_NAME, H5T_NATIVE_INT, sid, dcpl))<0) {
        H5_FAILED();
        printf("    Line %d: Can't create dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Write data */
    if (H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points)<0) {
        H5_FAILED();
        printf("    Line %d: Error writing dataset data\n",__LINE__);
        goto error;
    } /* end if */

    /* Flush the file (to clear the cache) */
    if (H5Fflush(file, H5F_SCOPE_GLOBAL)<0) {
        H5_FAILED();
        printf("    Line %d: Error flushing file\n",__LINE__);
        goto error;
    } /* end if */

    /* Query the dataset's size on disk */
    if((dset_size=H5Dget_storage_size(dsid))==0) {
        H5_FAILED();
        printf("    Line %d: Error querying dataset size\n",__LINE__);
        goto error;
    } /* end if */

    /* Verify that the size indicates data is uncompressed */
    /* (i.e. the deflation filter we asked for was silently ignored) */
    if((H5Tget_size(H5T_NATIVE_INT)*100*200)!=dset_size) {
        H5_FAILED();
        printf("    Line %d: Incorrect dataset size: %lu\n",__LINE__,(unsigned long)dset_size);
        goto error;
    } /* end if */

    /* Read data */
    if (H5Dread(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check)<0) {
        H5_FAILED();
        printf("    Line %d: Error reading dataset data\n",__LINE__);
        goto error;
    } /* end if */

    /* Compare data */
    /* Check that the values read are the same as the values written */
    for (i=0; i<dims[0]; i++) {
	for (j=0; j<dims[1]; j++) {
	    if (points[i][j] != check[i][j]) {
		H5_FAILED();
		printf("    Line %d: Read different values than written.\n",__LINE__);
		printf("    At index %lu,%lu\n", (unsigned long)(i), (unsigned long)(j));
		printf("    At original: %d\n",points[i][j]);
		printf("    At returned: %d\n",check[i][j]);
		goto error;
	    } /* end if */
	} /* end for */
    } /* end for */

    /* Close dataset */
    if(H5Dclose(dsid)<0) {
        H5_FAILED();
        printf("    Line %d: Can't close dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataspace */
    if(H5Sclose(sid)<0) {
        H5_FAILED();
        printf("    Line %d: Can't close dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataset creation property list */
    if(H5Pclose(dcpl)<0) {
        H5_FAILED();
        printf("    Line %d: Can't close dcpl\n",__LINE__);
        goto error;
    } /* end if */


    /* Try reading existing dataset with deflate filter */

    /* Compose the name of the file to open, using the srcdir, if appropriate */
    if (srcdir && ((HDstrlen(srcdir) + HDstrlen(FILE_DEFLATE_NAME) + 1) < sizeof(testfile))){
	HDstrcpy(testfile, srcdir);
	HDstrcat(testfile, "/");
    }
    HDstrcat(testfile, FILE_DEFLATE_NAME);

    /* Open existing file */
    if((fid=H5Fopen(testfile, H5F_ACC_RDONLY, H5P_DEFAULT))<0) {
        H5_FAILED();
        printf("    Line %d: Can't open existing deflated file\n",__LINE__);
        goto error;
    } /* end if */

    /* Open dataset */
    if ((dsid = H5Dopen(fid, "Dataset1"))<0) {
        H5_FAILED();
        printf("    Line %d: Can't open dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Read data (should fail, since deflate filter is missing) */
    H5E_BEGIN_TRY {
        ret=H5Dread(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check);
    } H5E_END_TRY;
    if (ret>=0) {
        H5_FAILED();
        printf("    Line %d: Error reading dataset data\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataset */
    if(H5Dclose(dsid)<0) {
        H5_FAILED();
        printf("    Line %d: Can't close dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Close existing file */
    if(H5Fclose(fid)<0) {
        H5_FAILED();
        printf("    Line %d: Can't close file\n",__LINE__);
        goto error;
    } /* end if */

    /* Re-register the deflate filter */
        /* Verify deflate filter is not registered currently */
        if(H5Zfilter_avail(H5Z_FILTER_DEFLATE)!=FALSE) {
            H5_FAILED();
            printf("    Line %d: Deflate filter available\n",__LINE__);
            goto error;
        } /* end if */
#ifdef H5_HAVE_FILTER_DEFLATE
        /* Register deflate filter (use internal function) */
        if(H5Z_register(H5Z_FILTER_DEFLATE, "deflate", H5Z_filter_deflate)<0) {
            H5_FAILED();
            printf("    Line %d: Can't unregister deflate filter\n",__LINE__);
            goto error;
        } /* end if */

        /* Verify deflate filter is registered currently */
        if(H5Zfilter_avail(H5Z_FILTER_DEFLATE)!=TRUE) {
            H5_FAILED();
            printf("    Line %d: Deflate filter not available\n",__LINE__);
            goto error;
        } /* end if */
#endif /* H5_HAVE_FILTER_DEFLATE */

    PASSED();
    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_onebyte_shuffle
 *
 * Purpose:	Tests the 8-bit array with shuffling algorithm.
 *              The shuffled array  should be the same result as 
 *              that before the shuffling.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Kent Yang
 *              Wednesday, , 2002 Nov. 13th
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_onebyte_shuffle(hid_t file)
{
    hid_t		dataset, space,dc;
    const hsize_t	size[2] = {10, 20};
    const hsize_t       chunk_size[2] = {10, 20};
    unsigned char       orig_data[10][20];
    unsigned char       new_data[10][20];
    unsigned            level;
#ifndef H5_HAVE_FILTER_SHUFFLE
    const char		*not_supported;
#endif
    
    hsize_t		i, j;

#ifndef H5_HAVE_FILTER_SHUFFLE
    not_supported = "    Data shuffling is not supported.\n"
		    "    The  shuffling flag was not found when hdf5 was configured.";
#endif
    
    TESTING("8-bit shuffling (setup)");
    
    /* Create the data space */
    if ((space = H5Screate_simple(2, size, NULL))<0) goto error;

    /* Use shuffling algorithm with 8-bit  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE))<0) goto error;
    if (H5Pset_chunk (dc, 2, chunk_size)<0) goto error;
    level = sizeof(unsigned char);
    if (level != 1) goto error;
    if (H5Pset_shuffle (dc, level)<0) goto error;

    /* Create the dataset */
    if ((dataset = H5Dcreate(file, DSET_ONEBYTE_SHUF_NAME, H5T_NATIVE_UCHAR, 
			     space,dc))<0) goto error;

    for (i= 0;i< 10; i++)
      for (j = 0; j < 20; j++)
	orig_data[i][j] = rand();

#ifdef H5_HAVE_FILTER_SHUFFLE
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 1: Test shuffling by setting up a chunked dataset and writing
     * to it.
     *---------------------------------------------------------------------- 
     */
    TESTING("8-bit shuffling (write)");

    if (H5Dwrite(dataset, H5T_NATIVE_UCHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT,
		 orig_data)<0)
	goto error;
#ifdef H5_HAVE_FILTER_SHUFFLE
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *---------------------------------------------------------------------- 
     */
    TESTING("8-bit shuffling (read)");

    /* Read the dataset back */
    if (H5Dread(dataset, H5T_NATIVE_UCHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT,
		new_data)<0)
	goto error;

    /* Check that the values read are the same as the values written */
    for (i=0; i<size[0]; i++) {
	for (j=0; j<size[1]; j++) {
	    if (new_data[i][j] != orig_data[i][j]) {
		H5_FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %lu,%lu\n",
		       (unsigned long)i, (unsigned long)j);
		goto error;
	    }
	}
    }
#ifdef H5_HAVE_FILTER_SHUFFLE
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * Cleanup
     *---------------------------------------------------------------------- 
     */
    if (H5Pclose (dc)<0) goto error;
    if (H5Dclose(dataset)<0) goto error;
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
    if((dcpl=H5Pcreate(H5P_DATASET_CREATE))<0) goto error;
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
	H5_FAILED();
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
 * Function:	test_types
 *
 * Purpose:	Make some datasets with various types so we can test h5ls.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Monday, June  7, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_types(hid_t file)
{
    hid_t		grp=-1, type=-1, space=-1, dset=-1;
    size_t		i;
    hsize_t		nelmts;
    unsigned char	buf[32];

    TESTING("various datatypes");
    if ((grp=H5Gcreate(file, "typetests", 0))<0) goto error;

    /* bitfield_1 */
    nelmts = sizeof(buf);
    if ((type=H5Tcopy(H5T_STD_B8LE))<0 ||
	(space=H5Screate_simple(1, &nelmts, NULL))<0 ||
	(dset=H5Dcreate(grp, "bitfield_1", type, space, H5P_DEFAULT))<0)
	goto error;
    for (i=0; i<sizeof buf; i++) buf[i] = (unsigned char)0xff ^ (unsigned char)i;
    if (H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf)<0)
	goto error;

    if (H5Sclose(space)<0) goto error;
    if (H5Tclose(type)<0) goto error;
    if (H5Dclose(dset)<0) goto error;

    /* bitfield_2 */
    nelmts = sizeof(buf)/2;
    if ((type=H5Tcopy(H5T_STD_B16LE))<0 ||
	(space=H5Screate_simple(1, &nelmts, NULL))<0 ||
	(dset=H5Dcreate(grp, "bitfield_2", type, space, H5P_DEFAULT))<0)
	goto error;
    for (i=0; i<sizeof buf; i++) buf[i] = (unsigned char)0xff ^ (unsigned char)i;
    if (H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf)<0)
	goto error;
    if (H5Sclose(space)<0) goto error;
    if (H5Tclose(type)<0) goto error;
    if (H5Dclose(dset)<0) goto error;

    /* opaque_1 */
    nelmts = sizeof(buf);
    if ((type=H5Tcreate(H5T_OPAQUE, 1))<0 ||
	H5Tset_tag(type, "testing 1-byte opaque type")<0 ||
	(space=H5Screate_simple(1, &nelmts, NULL))<0 ||
	(dset=H5Dcreate(grp, "opaque_1", type, space, H5P_DEFAULT))<0)
	goto error;
    for (i=0; i<sizeof buf; i++) buf[i] = (unsigned char)0xff ^ (unsigned char)i;
    if (H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf)<0)
	goto error;
    if (H5Sclose(space)<0) goto error;
    if (H5Tclose(type)<0) goto error;
    if (H5Dclose(dset)<0) goto error;
    
    /* opaque_2 */
    nelmts = sizeof(buf)/4;
    if ((type=H5Tcreate(H5T_OPAQUE, 4))<0 ||
	H5Tset_tag(type, "testing 4-byte opaque type")<0 ||
	(space=H5Screate_simple(1, &nelmts, NULL))<0 ||
	(dset=H5Dcreate(grp, "opaque_2", type, space, H5P_DEFAULT))<0)
	goto error;
    for (i=0; i<sizeof buf; i++) buf[i] = (unsigned char)0xff ^ (unsigned char)i;
    if (H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf)<0)
	goto error;
    if (H5Sclose(space)<0) goto error;
    if (H5Tclose(type)<0) goto error;
    if (H5Dclose(dset)<0) goto error;
    
    /* Cleanup */
    if (H5Gclose(grp)<0) goto error;
    PASSED();
    return 0;
    
 error:
    H5E_BEGIN_TRY {
	H5Gclose(grp);
	H5Tclose(type);
	H5Sclose(space);
	H5Dclose(dset);
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
    int			nerrors=0;
    char		filename[1024];

    h5_reset();
    fapl = h5_fileaccess();
    
#if 0
    {
	/* Turn off raw data cache */
	int mdc_nelmts;
	if (H5Pget_cache(fapl, &mdc_nelmts, NULL, NULL, NULL)<0) goto error;
	if (H5Pset_cache(fapl, mdc_nelmts, 0, 0, 0.0)<0) goto error;
    }
#endif
    
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) {
	goto error;
    }
    
    /* Cause the library to emit initial messages */
    if ((grp = H5Gcreate (file, "emit diagnostics", 0))<0) goto error;
    if (H5Gset_comment(grp, ".", "Causes diagnostic messages to be emitted")<0)
	goto error;
    if (H5Gclose (grp)<0) goto error;

    nerrors += test_create(file)<0 	?1:0;
    nerrors += test_simple_io(file)<0	?1:0;
    nerrors += test_compact_io(fapl)<0  ?1:0;
    nerrors += test_tconv(file)<0	?1:0;
    nerrors += test_compression(file)<0	?1:0;
    nerrors += test_missing_filter(file)<0	?1:0;
    nerrors += test_onebyte_shuffle(file)<0 ?1:0;
    nerrors += test_multiopen (file)<0	?1:0;
    nerrors += test_types(file)<0       ?1:0;

    if (H5Fclose(file)<0) goto error;
    if (nerrors) goto error;
    printf("All dataset tests passed.\n");
    h5_cleanup(FILENAME, fapl);
    return 0;

 error:
    nerrors = MAX(1, nerrors);
    printf("***** %d DATASET TEST%s FAILED! *****\n",
	   nerrors, 1 == nerrors ? "" : "S");
    return 1;
}
