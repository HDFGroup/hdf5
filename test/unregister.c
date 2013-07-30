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
 *              24 April 2013
 *
 * Purpose:	Tests H5Zunregister function
 */
#include "h5test.h"

const char *FILENAME[] = {
    "unregister_filter_1",
    "unregister_filter_2",
    NULL
};

#define GROUP_NAME              "group"
#define DSET_NAME               "dataset"
#define FILENAME_BUF_SIZE       1024
#define DSET_DIM1               100
#define DSET_DIM2               200
#define FILTER_CHUNK_DIM1       2
#define FILTER_CHUNK_DIM2       25
#define GROUP_ITERATION         1000

#define H5Z_FILTER_DUMMY        312

static size_t filter_dummy(unsigned int flags, size_t cd_nelmts,
    const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);

/* Dummy filter for test_unregister_filters only */
const H5Z_class2_t H5Z_DUMMY[1] = {{
    H5Z_CLASS_T_VERS,           /* H5Z_class_t version */
    H5Z_FILTER_DUMMY,		/* Filter id number		*/
    1, 1,                       /* Encoding and decoding enabled */
    "dummy",			/* Filter name for debugging	*/
    NULL,                       /* The "can apply" callback     */
    NULL,                       /* The "set local" callback     */
    filter_dummy,		/* The actual filter function	*/
}};


/*-------------------------------------------------------------------------
 * Function:	filter_dummy
 *
 * Purpose:	A dummy compression method that doesn't do anything.  This
 *              filter is only for test_unregister_filters.  Please don't 
 *              use it for other tests because it may mess up this test.
 *
 * Return:	Success:	Data chunk size
 *
 *		Failure:	0
 *
 * Programmer:	Raymond Lu
 *              April 24, 2013
 *
 *-------------------------------------------------------------------------
 */
static size_t
filter_dummy(unsigned int UNUSED flags, size_t UNUSED cd_nelmts,
      const unsigned int UNUSED *cd_values, size_t nbytes,
      size_t UNUSED *buf_size, void UNUSED **buf)
{
    return nbytes;
}

/*-------------------------------------------------------------------------
 * Function:	test_unregister_filters
 *
 * Purpose:	Tests unregistering filter before closing the file
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *              11 April 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_unregister_filters(hid_t my_fapl)
{
    hid_t	file1, file2;
    hid_t	dc;
    hid_t	gcpl, gid, group;
    hid_t       dataset, space;
    int		i, j, n;
    char        gname[256];
    char        filename[FILENAME_BUF_SIZE];
    const hsize_t chunk_size[2] = {FILTER_CHUNK_DIM1, FILTER_CHUNK_DIM2};  /* Chunk dimensions */
    hsize_t	dims[2];
    int	        points[DSET_DIM1][DSET_DIM2];
    herr_t      ret;

    TESTING("Unregistering filter");

    /* Create first file */
    h5_fixname(FILENAME[0], my_fapl, filename, sizeof filename);
    if((file1 = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0) goto error;

    /* Create second file */
    h5_fixname(FILENAME[1], my_fapl, filename, sizeof filename);
    if((file2 = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0) goto error;

    /* Register DUMMY filter */
    if(H5Zregister(H5Z_DUMMY) < 0) goto error;

    if(H5Zfilter_avail(H5Z_FILTER_DUMMY)!=TRUE) goto error;

    if((gcpl = H5Pcreate(H5P_GROUP_CREATE)) < 0) goto error;
  
    /* Use DUMMY filter for creating groups */ 
    if(H5Pset_filter (gcpl, H5Z_FILTER_DUMMY, H5Z_FLAG_MANDATORY, (size_t)0, NULL) < 0) goto error;

    /* Create a group using this filter */
    if((gid = H5Gcreate2(file1, GROUP_NAME, H5P_DEFAULT, gcpl, H5P_DEFAULT)) < 0) goto error;

    /* Create multiple groups under the main group */
    for (i=0; i < GROUP_ITERATION; i++) {
        sprintf(gname, "group_%d", i);
        if((group = H5Gcreate2(gid, gname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;
        if(H5Gclose(group) < 0) goto error; 
    }

    if(H5Fflush(file1, H5F_SCOPE_GLOBAL) < 0) goto error;

    /* Unregister the filter before closing the group.  It should fail */
    H5E_BEGIN_TRY {
        ret = H5Zunregister(H5Z_FILTER_DUMMY);
    } H5E_END_TRY;
    if(ret>=0) {
        H5_FAILED();
        printf("    Line %d: Should not be able to unregister filter\n", __LINE__);
        goto error;
    } /* end if */

    /* Close the group */
    if(H5Gclose(gid) < 0) goto error;

    /* Clean up objects used for this test */
    if(H5Pclose (gcpl) < 0) goto error;

    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;

    if(H5Pset_filter(dc, H5Z_FILTER_DUMMY, 0, (size_t)0, NULL) < 0) goto error;

    /* Initialize the dataset */
    for(i = n = 0; i < DSET_DIM1; i++)
        for(j = 0; j < DSET_DIM2; j++)
            points[i][j] = n++;

    /* Create the data space */
    dims[0] = DSET_DIM1;
    dims[1] = DSET_DIM2;
    if((space = H5Screate_simple(2, dims, NULL)) < 0) goto error;

    /* Create a dataset in the first file */
    if((dataset = H5Dcreate2(file1, DSET_NAME, H5T_NATIVE_INT, space,
                             H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Write the data to the dataset */
    if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points) < 0)
        goto error;

    /* Unregister the filter before closing the dataset.  It should fail */
    H5E_BEGIN_TRY {
        ret = H5Zunregister(H5Z_FILTER_DUMMY);
    } H5E_END_TRY;
    if(ret>=0) {
        H5_FAILED();
        printf("    Line %d: Should not be able to unregister filter\n", __LINE__);
        goto error;
    } /* end if */

    if(H5Dclose(dataset) < 0) goto error;

    /* Create a dataset in the second file */
    if((dataset = H5Dcreate2(file2, DSET_NAME, H5T_NATIVE_INT, space,
                             H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Write the data to the dataset */
    if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points) < 0)
        goto error;

    if(H5Dclose(dataset) < 0) goto error;

    /* Unregister the filter after closing all objects but before closing files. 
     * It should flush all files. */
    if(H5Zunregister(H5Z_FILTER_DUMMY) < 0) goto error;

    /* Clean up objects used for this test */
    if(H5Pclose (dc) < 0) goto error;
    if(H5Fclose(file1) < 0) goto error;
    if(H5Fclose(file2) < 0) goto error;

    PASSED();
    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Tests unregistering filter with H5Zunregister
 *
 * Return:	Success:	exit(0)
 *
 *		Failure:	exit(1)
 *
 * Programmer:	Raymond Lu
 *              11 April 2013
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t		fapl;
    int	nerrors = 0;

    /* Testing setup */
    h5_reset();
    fapl = h5_fileaccess();

    /* Test unregistering filter in its own file */
    nerrors += (test_unregister_filters(fapl) < 0           ? 1 : 0);

    if(nerrors)
        goto error;
    printf("All filter unregistration tests passed.\n");
    h5_cleanup(FILENAME, fapl);

    return 0;

error:
    nerrors = MAX(1, nerrors);
    printf("***** %d FILTER UNREGISTRATION TEST%s FAILED! *****\n",
            nerrors, 1 == nerrors ? "" : "S");
    return 1;
}

