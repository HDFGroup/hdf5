/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5. The full HDF5 copyright notice, including      *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic document set and is     *
 * linked from the top-level documents page.  It can also be found at        *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have access   *
 * to either file, you may request a copy from help@hdfgroup.org.            *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*
 * Programmer:	Raymond Lu
 *		13 February 2013
 *
 * Purpose:	Tests the plugin module (H5PL)
 */
#include <stdlib.h>
#include <time.h>

#include "h5test.h"
#include "H5srcdir.h"

/*
 * This file needs to access private datatypes from the H5Z package.
 */
#define H5Z_PACKAGE
#include "H5Zpkg.h"

/* Filters for HDF5 internal test */
#define H5Z_FILTER_DYNLIB1      257
#define H5Z_FILTER_DYNLIB2      258 
#define H5Z_FILTER_DYNLIB3      259
#define H5Z_FILTER_DYNLIB4      260

const char *FILENAME[] = {
    "plugin",
    NULL
};
#define FILENAME_BUF_SIZE       1024

/* Dataset names for testing filters */
#define DSET_DEFLATE_NAME	"deflate"
#define DSET_DYNLIB1_NAME	"dynlib1"
#define DSET_DYNLIB2_NAME       "dynlib2"
#define DSET_DYNLIB4_NAME   "dynlib4"

/* Parameters for internal filter test */
#define FILTER_CHUNK_DIM1       2
#define FILTER_CHUNK_DIM2       25
#define FILTER_HS_OFFSET1       7
#define FILTER_HS_OFFSET2       30
#define FILTER_HS_SIZE1         4
#define FILTER_HS_SIZE2         50

/* Shared global arrays */
#define DSET_DIM1       100
#define DSET_DIM2       200

/* Limit random number within 20000 */
#define RANDOM_LIMIT    20000

#define GROUP_ITERATION 1000

int	points_deflate[DSET_DIM1][DSET_DIM2], 
        points_dynlib1[DSET_DIM1][DSET_DIM2],
        points_dynlib2[DSET_DIM1][DSET_DIM2],
        points_dynlib4[DSET_DIM1][DSET_DIM2],
        points_bzip2[DSET_DIM1][DSET_DIM2];


/*-------------------------------------------------------------------------
 * Function:	test_filter_internal
 *
 * Purpose:	Tests writing entire data and partial data with filters  
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *              27 February 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_filter_internal(hid_t fid, const char *name, hid_t dcpl)
{
    hid_t		dataset;        /* Dataset ID */
    hid_t		dxpl;           /* Dataset xfer property list ID */
    hid_t		write_dxpl;     /* Dataset xfer property list ID for writing */
    hid_t		sid;            /* Dataspace ID */
    const hsize_t	size[2] = {DSET_DIM1, DSET_DIM2};           /* Dataspace dimensions */
    const hsize_t	hs_offset[2] = {FILTER_HS_OFFSET1, FILTER_HS_OFFSET2}; /* Hyperslab offset */
    const hsize_t	hs_size[2] = {FILTER_HS_SIZE1, FILTER_HS_SIZE2};   /* Hyperslab size */
    void		*tconv_buf = NULL;      /* Temporary conversion buffer */
    int	                points[DSET_DIM1][DSET_DIM2], check[DSET_DIM1][DSET_DIM2];
    size_t		i, j;        /* Local index variables */
    int                 n = 0;

    /* Create the data space */
    if((sid = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /*
     * Create a small conversion buffer to test strip mining. We
     * might as well test all we can!
     */
    if((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0) goto error;
    tconv_buf = HDmalloc((size_t)1000);
    if(H5Pset_buffer(dxpl, (size_t)1000, tconv_buf, NULL) < 0) goto error;
    if((write_dxpl = H5Pcopy(dxpl)) < 0) TEST_ERROR;

    TESTING("    filters (setup)");

    /* Check if all the filters are available */
    if(H5Pall_filters_avail(dcpl)!=TRUE) {
        H5_FAILED();
        printf("    Line %d: Incorrect filter availability\n",__LINE__);
        goto error;
    } /* end if */

    /* Create the dataset */
    if((dataset = H5Dcreate2(fid, name, H5T_NATIVE_INT, sid, H5P_DEFAULT,
			     dcpl, H5P_DEFAULT)) < 0) goto error;

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 1: Read uninitialized data.  It should be zero.
     *----------------------------------------------------------------------
     */
    TESTING("    filters (uninitialized read)");

    if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
	TEST_ERROR;

    for(i=0; i<(size_t)size[0]; i++) {
	for(j=0; j<(size_t)size[1]; j++) {
	    if(0!=check[i][j]) {
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
     * STEP 2: Test filters by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    filters (write)");

    n = 0;
    for(i=0; i<size[0]; i++) {
	for(j=0; j<size[1]; j++) {
	    points[i][j] = (int)(n++);
	}
    }

    if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, points) < 0)
	TEST_ERROR;

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 3: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    filters (read)");

    /* Read the dataset back */
        if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
	   TEST_ERROR;

        /* Check that the values read are the same as the values written */
        for(i=0; i<size[0]; i++) {
	   for(j=0; j<size[1]; j++) {
	       if(points[i][j] != check[i][j]) {
		  H5_FAILED();
		  fprintf(stderr,"    Read different values than written.\n");
		  fprintf(stderr,"    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
		  fprintf(stderr,"    At original: %d\n", (int)points[i][j]);
		  fprintf(stderr,"    At returned: %d\n", (int)check[i][j]);
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
    TESTING("    filters (modify)");

    for(i=0; i<size[0]; i++) {
	for(j=0; j<size[1]/2; j++) {
	    points[i][j] = (int)HDrandom () % RANDOM_LIMIT;
	}
    }
    if(H5Dwrite (dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, points) < 0)
	TEST_ERROR;

        /* Read the dataset back and check it */
        if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
	   TEST_ERROR;

        /* Check that the values read are the same as the values written */
        for(i=0; i<size[0]; i++) {
	   for(j=0; j<size[1]; j++) {
	       if(points[i][j] != check[i][j]) {
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
     * insures that the filters message is picked up properly from the
     * object header.
     *----------------------------------------------------------------------
     */
    TESTING("    filters (re-open)");

    if(H5Dclose(dataset) < 0) TEST_ERROR;
    if((dataset = H5Dopen2(fid, name, H5P_DEFAULT)) < 0) TEST_ERROR;

        if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
            TEST_ERROR;

        /* Check that the values read are the same as the values written */
        for(i = 0; i < size[0]; i++)
	   for(j = 0; j < size[1]; j++)
	       if(points[i][j] != check[i][j]) {
		  H5_FAILED();
		  printf("    Read different values than written.\n");
		  printf("    At index %lu,%lu\n",
		        (unsigned long)i, (unsigned long)j);
		  goto error;
	       } /* end if */

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 6: Test partial I/O by writing to and then reading from a
     * hyperslab of the dataset.  The hyperslab does not line up on chunk
     * boundaries (we know that case already works from above tests).
     *----------------------------------------------------------------------
     */
    TESTING("    filters (partial I/O)");

    for(i=0; i<(size_t)hs_size[0]; i++) {
	for(j=0; j<(size_t)hs_size[1]; j++) {
	    points[(size_t)hs_offset[0]+i][(size_t)hs_offset[1]+j] = (int)HDrandom() % RANDOM_LIMIT;
	}
    }
    if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, hs_offset, NULL, hs_size,
			    NULL) < 0) TEST_ERROR;
    /* (Use the "read" DXPL because partial I/O on corrupted data test needs to ignore errors during writing) */
    if(H5Dwrite (dataset, H5T_NATIVE_INT, sid, sid, dxpl, points) < 0)
	TEST_ERROR;

        if(H5Dread (dataset, H5T_NATIVE_INT, sid, sid, dxpl, check) < 0)
	   TEST_ERROR;

        /* Check that the values read are the same as the values written */
        for(i=0; i<(size_t)hs_size[0]; i++) {
	   for(j=0; j<(size_t)hs_size[1]; j++) {
	       if(points[(size_t)hs_offset[0]+i][(size_t)hs_offset[1]+j] !=
                      check[(size_t)hs_offset[0]+i][(size_t)hs_offset[1]+j]) {
		  H5_FAILED();
		  fprintf(stderr,"    Read different values than written.\n");
		  fprintf(stderr,"    At index %lu,%lu\n",
		         (unsigned long)((size_t)hs_offset[0]+i),
		         (unsigned long)((size_t)hs_offset[1]+j));
		  fprintf(stderr,"    At original: %d\n",
		         (int)points[(size_t)hs_offset[0]+i][(size_t)hs_offset[1]+j]);
		  fprintf(stderr,"    At returned: %d\n",
		         (int)check[(size_t)hs_offset[0]+i][(size_t)hs_offset[1]+j]);
		  goto error;
	       }
	   }
        }

    PASSED();

    /* Save the data written to the file for later comparison when the file 
     * is reopened for read test */
    for(i=0; i<size[0]; i++) {
        for(j=0; j<size[1]; j++) {
            if(!HDstrcmp(name, DSET_DEFLATE_NAME)) {
	        points_deflate[i][j] = points[i][j];
            } else if(!HDstrcmp(name, DSET_DYNLIB1_NAME)) {
	        points_dynlib1[i][j] = points[i][j];
            } else if(!HDstrcmp(name, DSET_DYNLIB2_NAME)) {
	        points_dynlib2[i][j] = points[i][j];
            } else if(!HDstrcmp(name, DSET_DYNLIB4_NAME)) {
            points_dynlib4[i][j] = points[i][j];
            }
	}
    }

    /* Clean up objects used for this test */
    if(H5Dclose (dataset) < 0) goto error;
    if(H5Sclose (sid) < 0) goto error;
    if(H5Pclose (dxpl) < 0) goto error;
    free (tconv_buf);

    return(0);

error:
    if(tconv_buf)
        free (tconv_buf);
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:	test_filters_for_datasets
 *
 * Purpose:	Tests creating datasets and writing data with dynamically
 *              loaded filters
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *              14 March 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_filters_for_datasets(hid_t file)
{
    hid_t	dc;                 /* Dataset creation property list ID */
    const hsize_t chunk_size[2] = {FILTER_CHUNK_DIM1, FILTER_CHUNK_DIM2};  /* Chunk dimensions */
    unsigned int         compress_level = 9;
    unsigned int         dynlib4_values[4];

    /*----------------------------------------------------------
     * STEP 1: Test deflation by itself.
     *----------------------------------------------------------
     */
#ifdef H5_HAVE_FILTER_DEFLATE
    puts("Testing deflate filter");
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_deflate (dc, 6) < 0) goto error;

    if(test_filter_internal(file,DSET_DEFLATE_NAME,dc) < 0) goto error;
    /* Clean up objects used for this test */
    if(H5Pclose (dc) < 0) goto error;
#else /* H5_HAVE_FILTER_DEFLATE */
    TESTING("deflate filter");
    SKIPPED();
    puts("    Deflate filter not enabled");
#endif /* H5_HAVE_FILTER_DEFLATE */

    /*----------------------------------------------------------
     * STEP 2: Test DYNLIB1 by itself.
     *----------------------------------------------------------
     */
    puts("Testing DYNLIB1 filter");
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_filter (dc, H5Z_FILTER_DYNLIB1, H5Z_FLAG_MANDATORY, (size_t)1, &compress_level) < 0) goto error;

    if(test_filter_internal(file,DSET_DYNLIB1_NAME,dc) < 0) goto error;

    /* Clean up objects used for this test */
    if(H5Pclose (dc) < 0) goto error;

    /* Unregister the dynamic filter DYNLIB1 for testing purpose. The next time when this test is run for 
     * the new file format, the library's H5PL code has to search in the table of loaded plugin libraries
     * for this filter. */
    if(H5Zunregister(H5Z_FILTER_DYNLIB1) < 0) goto error;

    /*----------------------------------------------------------
     * STEP 3: Test DYNLIB2 by itself.
     *----------------------------------------------------------
     */
    puts("Testing DYNLIB2 filter");
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_filter (dc, H5Z_FILTER_DYNLIB2, H5Z_FLAG_MANDATORY, 0, NULL) < 0) goto error;

    if(test_filter_internal(file,DSET_DYNLIB2_NAME,dc) < 0) goto error;

    /* Clean up objects used for this test */
    if(H5Pclose (dc) < 0) goto error;

    /* Unregister the dynamic filter DYNLIB2 for testing purpose. The next time when this test is run for 
     * the new file format, the library's H5PL code has to search in the table of loaded plugin libraries
     * for this filter. */
    if(H5Zunregister(H5Z_FILTER_DYNLIB2) < 0) goto error;

    /*----------------------------------------------------------
     * STEP 4: Test DYNLIB4 by itself.
     *----------------------------------------------------------
     */
    puts("Testing DYNLIB4 filter");
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
    dynlib4_values[0] = 9;
    if(H5get_libversion(&dynlib4_values[1], &dynlib4_values[2], &dynlib4_values[3]) < 0) goto error;
    if(H5Pset_filter (dc, H5Z_FILTER_DYNLIB4, H5Z_FLAG_MANDATORY, (size_t)4, dynlib4_values) < 0) goto error;

    if(test_filter_internal(file,DSET_DYNLIB4_NAME,dc) < 0) goto error;

    /* Clean up objects used for this test */
    if(H5Pclose (dc) < 0) goto error;

    /* Unregister the dynamic filter DYNLIB4 for testing purpose. The next time when this test is run for
     * the new file format, the library's H5PL code has to search in the table of loaded plugin libraries
     * for this filter. */
    if(H5Zunregister(H5Z_FILTER_DYNLIB4) < 0) goto error;

    return 0;

error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:	test_read_data
 *
 * Purpose:	Tests reading data and compares values
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *              14 March 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_read_data(hid_t dataset, int *origin_data)
{
    int	                check[DSET_DIM1][DSET_DIM2];
    const hsize_t	size[2] = {DSET_DIM1, DSET_DIM2};           /* Dataspace dimensions */
    int                 *data_p = origin_data;
    size_t		i, j;        /* Local index variables */

    /* Read the dataset back */
    if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    for(i=0; i<size[0]; i++) {
        for(j=0; j<size[1]; j++) {
	   if(*data_p != check[i][j]) {
	       H5_FAILED();
	       fprintf(stderr,"    Read different values than written.\n");
	       fprintf(stderr,"    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
	       fprintf(stderr,"    At original: %d\n", *data_p);
	       fprintf(stderr,"    At returned: %d\n", (int)check[i][j]);
	       goto error;
	   }
           data_p++;
	}
    }

    PASSED();
    return 0;

error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    test_read_with_filters
 *
 * Purpose: Tests reading dataset created with dynamically loaded filters
 *
 * Return:  Success:    0
 *      Failure:    -1
 *
 * Programmer:  Raymond Lu
 *              14 March 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_read_with_filters(hid_t file)
{
    hid_t   dset;                 /* Dataset ID */

    /*----------------------------------------------------------
     * STEP 1: Test deflation by itself.
     *----------------------------------------------------------
     */
#ifdef H5_HAVE_FILTER_DEFLATE
    TESTING("Testing deflate filter");

    if(H5Zfilter_avail(H5Z_FILTER_DEFLATE) != TRUE) TEST_ERROR

    if((dset = H5Dopen2(file,DSET_DEFLATE_NAME,H5P_DEFAULT)) < 0) TEST_ERROR

    if(test_read_data(dset, (int *)points_deflate) < 0) TEST_ERROR

    if(H5Dclose(dset) < 0) TEST_ERROR

    /* Clean up objects used for this test */
#else /* H5_HAVE_FILTER_DEFLATE */
    TESTING("deflate filter");
    SKIPPED();
    puts("    Deflate filter not enabled");
#endif /* H5_HAVE_FILTER_DEFLATE */

    /*----------------------------------------------------------
     * STEP 2: Test DYNLIB1 by itself.
     *----------------------------------------------------------
     */
    TESTING("Testing DYNLIB1 filter");

    if((dset = H5Dopen2(file,DSET_DYNLIB1_NAME,H5P_DEFAULT)) < 0) TEST_ERROR

    if(test_read_data(dset, (int *)points_dynlib1) < 0) TEST_ERROR

    if(H5Dclose(dset) < 0) TEST_ERROR

    /*----------------------------------------------------------
     * STEP 3: Test Bogus2 by itself.
     *----------------------------------------------------------
     */
    TESTING("Testing DYNLIB2 filter");

    if((dset = H5Dopen2(file,DSET_DYNLIB2_NAME,H5P_DEFAULT)) < 0) TEST_ERROR

    if(test_read_data(dset, (int *)points_dynlib2) < 0) TEST_ERROR

    if(H5Dclose(dset) < 0) TEST_ERROR

    /*----------------------------------------------------------
     * STEP 4: Test DYNLIB4 by itself.
     *----------------------------------------------------------
     */
    TESTING("Testing DYNLIB4 filter");

    if((dset = H5Dopen2(file,DSET_DYNLIB4_NAME,H5P_DEFAULT)) < 0) TEST_ERROR

    if(test_read_data(dset, (int *)points_dynlib4) < 0) TEST_ERROR

    if(H5Dclose(dset) < 0) TEST_ERROR

    return 0;

error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    test_noread_data
 *
 * Purpose: Tests not reading data
 *
 * Return:  Success:    0
 *      Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_noread_data(hid_t dataset)
{
    int        check[DSET_DIM1][DSET_DIM2];
    herr_t     ret;

    /* Read the dataset back */
    H5E_BEGIN_TRY {
        ret = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check);
    } H5E_END_TRY
    if(ret >= 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:	test_noread_with_filters
 *
 * Purpose:	Tests reading dataset created with dynamically loaded filters disabled
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_noread_with_filters(hid_t file)
{
    hid_t	dset;                 /* Dataset ID */
    unsigned     plugin_state;         /* status of plugins */
    TESTING("Testing DYNLIB1 filter with plugins disabled");

    /* disable filter plugin */
    if(H5PLget_loading_state(&plugin_state) < 0) TEST_ERROR
    plugin_state = plugin_state & ~H5PL_FILTER_PLUGIN;
    if(H5PLset_loading_state(plugin_state) < 0) TEST_ERROR

    if((dset = H5Dopen2(file,DSET_DYNLIB1_NAME,H5P_DEFAULT)) < 0) TEST_ERROR

    if(test_noread_data(dset) < 0) TEST_ERROR

    if(H5Dclose(dset) < 0) TEST_ERROR

    /* re-enable filter plugin */
    plugin_state = plugin_state | H5PL_FILTER_PLUGIN;
    if(H5PLset_loading_state(plugin_state) < 0) TEST_ERROR

    return 0;

error:
    /* re-enable filter plugin */
    plugin_state = plugin_state | H5PL_FILTER_PLUGIN;
    if(H5PLset_loading_state(plugin_state) < 0) TEST_ERROR
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:	test_filters_for_groups
 *
 * Purpose:	Tests creating group with dynamically loaded filters
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *              1 April 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_filters_for_groups(hid_t file)
{
    hid_t	gcpl, gid, group;
    int         i;
    char        gname[256];

    TESTING("Testing DYNLIB3 filter for group");

    if((gcpl = H5Pcreate(H5P_GROUP_CREATE)) < 0) goto error;
  
    /* Use DYNLIB3 for creating groups */ 
    if(H5Pset_filter (gcpl, H5Z_FILTER_DYNLIB3, H5Z_FLAG_MANDATORY, (size_t)0, NULL) < 0) goto error;

    /* Create a group using this filter */
    if((gid = H5Gcreate2(file, "group1", H5P_DEFAULT, gcpl, H5P_DEFAULT)) < 0) goto error;

    /* Create multiple groups under "group1" */
    for (i=0; i < GROUP_ITERATION; i++) {
        sprintf(gname, "group_%d", i);
        if((group = H5Gcreate2(gid, gname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;
        if(H5Gclose(group) < 0) goto error; 
    }

    /* Close the group */
    if(H5Gclose(gid) < 0) goto error;

    /* Clean up objects used for this test */
    if(H5Pclose (gcpl) < 0) goto error;

    PASSED();

    return 0;

error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:	test_groups_with_filters
 *
 * Purpose:	Tests opening group with dynamically loaded filters
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *              1 April 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_groups_with_filters(hid_t file)
{
    hid_t	gid, group;
    int         i;
    char        gname[256];

    TESTING("Testing opening groups with DYNLIB3 filter");

    /* Open the top group */
    if((gid = H5Gopen2(file, "group1", H5P_DEFAULT)) < 0) goto error;

    /* Create multiple groups under "group1" */
    for (i=0; i < GROUP_ITERATION; i++) {
        sprintf(gname, "group_%d", i);
        if((group = H5Gopen2(gid, gname, H5P_DEFAULT)) < 0) goto error;
        if(H5Gclose(group) < 0) goto error; 
    }

    /* Close the group */
    if(H5Gclose(gid) < 0) goto error;

    PASSED();

    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Tests the plugin module (H5PL)
 *
 * Return:	Success:	exit(0)
 *
 *		Failure:	exit(1)
 *
 * Programmer:	Raymond Lu
 *		14 March 2013
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    char		filename[FILENAME_BUF_SIZE];
    hid_t		file, fapl, fapl2;
    hbool_t new_format;
    int mdc_nelmts;
    size_t rdcc_nelmts;
    size_t rdcc_nbytes;
    double rdcc_w0;
    int	nerrors = 0;

    /* Testing setup */
    h5_reset();
    fapl = h5_fileaccess();

    /* Turn off the chunk cache, so all the chunks are immediately written to disk */
    if(H5Pget_cache(fapl, &mdc_nelmts, &rdcc_nelmts, &rdcc_nbytes, &rdcc_w0) < 0)
        TEST_ERROR 
    rdcc_nbytes = 0;
    if(H5Pset_cache(fapl, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0) < 0)
        TEST_ERROR 

    /* Copy the file access property list */
    if((fapl2 = H5Pcopy(fapl)) < 0) TEST_ERROR

    /* Set the "use the latest version of the format" bounds for creating objects in the file */
    if(H5Pset_libver_bounds(fapl2, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) TEST_ERROR

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Test with old & new format groups */
    for(new_format = FALSE; new_format <= TRUE; new_format++) {
        hid_t my_fapl;

        /* Set the FAPL for the type of format */
        if(new_format) {
            puts("\nTesting with new file format:");
            my_fapl = fapl2;
        } /* end if */
        else {
            puts("Testing with old file format:");
            my_fapl = fapl;
        } /* end else */

        /* Create the file for this test */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
            TEST_ERROR 

        /* Test dynamically loaded filters for chunked dataset */
        nerrors += (test_filters_for_datasets(file) < 0	? 1 : 0);

        /* Test dynamically loaded filters for groups */
        nerrors += (test_filters_for_groups(file) < 0 ? 1 : 0);

        if(H5Fclose(file) < 0)
            TEST_ERROR 
    } /* end for */

    /* Close FAPL */
    if(H5Pclose(fapl2) < 0) TEST_ERROR
    if(H5Pclose(fapl) < 0) TEST_ERROR
   
    puts("\nTesting reading data with with dynamic plugin filters:");

    /* Close the library so that all loaded plugin libraries are unloaded */
    h5_reset();
    fapl = h5_fileaccess();

    /* Reopen the file for testing data reading */
    if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR 

    /* Read the data with filters */
    nerrors += (test_read_with_filters(file) < 0		? 1 : 0);

    /* Open the groups with filters */
    nerrors += (test_groups_with_filters(file) < 0	? 1 : 0);

    /* Close the library so that all loaded plugin libraries are unloaded */
    h5_reset();
    fapl = h5_fileaccess();

    /* Reopen the file for testing data reading */
    if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR

    /* Read the data with disabled filters */
    nerrors += (test_noread_with_filters(file) < 0  ? 1 : 0);

    if(H5Fclose(file) < 0)
        TEST_ERROR 

    if(nerrors)
        TEST_ERROR 
    printf("All plugin tests passed.\n");
    h5_cleanup(FILENAME, fapl);

    return 0;

error:
    nerrors = MAX(1, nerrors);
    printf("***** %d PLUGIN TEST%s FAILED! *****\n",
            nerrors, 1 == nerrors ? "" : "S");
    return 1;
}

