/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, April  2, 1998
 *
 * Purpose:	Tests datasets with shared data types.
 */
#include <assert.h>
#include <hdf5.h>

#include <H5config.h>
#ifndef HAVE_ATTRIBUTE
#   undef __attribute__
#   define __attribute__(X) /*void*/
#   define __unused__ /*void*/
#else
#   define __unused__ __attribute__((unused))
#endif

#define TEST_FILE_NAME0		"shtype0.h5"
#define TEST_FILE_NAME1		"shtype1.h5"
#define TEST_FILE_NAME2A	"shtype2a.h5"
#define TEST_FILE_NAME2B	"shtype2b.h5"
#define TEST_FILE_NAME3		"shtype3.h5"


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
 * Function:	test_1
 *
 * Purpose:	Create a shared data type and use it to create a dataset.
 *		Then query that dataset's type and use it to create a second
 *		dataset.  Both datasets should share a single data type.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, April  2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_1 (void)
{
    hid_t	f, d1, d2, s1, t1, t2;
    hsize_t	size[1] = {1};

    printf ("%-70s", "...creating/quering datasets with shared type");
    fflush (stdout);

    f = H5Fcreate (TEST_FILE_NAME1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (f<0) goto error;
    if ((s1 = H5Screate_simple (1, size, size))<0) goto error;
    if ((t1 = H5Tcopy (H5T_NATIVE_INT))<0) goto error;
    if (H5Tshare (f, t1)<0) goto error;
    
    /* Create the first dataset */
    if ((d1 = H5Dcreate (f, "d1", t1, s1, H5P_DEFAULT))<0) goto error;
    if (H5Dclose (d1)<0) goto error;

    /* Get the type of the first dataset to use for the second */
    if ((d1 = H5Dopen (f, "d1"))<0) goto error;
    if ((t2 = H5Dget_type (d1))<0) goto error;
    if (H5Dclose (d1)<0) goto error;

    /* Create the second datatype */
    if ((d2 = H5Dcreate (f, "d2", t2, s1, H5P_DEFAULT))<0) goto error;
    if (H5Dclose (d2)<0) goto error;

    if (H5Sclose (s1)<0) goto error;
    if (H5Fclose (f)<0) goto error;
    
    puts (" PASSED");
    return 0;

 error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_2
 *
 * Purpose:	Create lots of datasets that have their own types, then
 *		create another file with lots of datasets that share a type
 *		and compare the file sizes.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, April  2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_2 (void)
{
    hid_t	f1, f2, t1, t2, s1, d1, d2;
    hsize_t	size[1] = {1};
    char	buf[32];
    int		i;

    printf ("%-70s", "...compare shared and unshared types");
    fflush (stdout);

    f1 = H5Fcreate (TEST_FILE_NAME2A, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (f1<0) goto error;
    f2 = H5Fcreate (TEST_FILE_NAME2B, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (f2<0) goto error;
    s1 = H5Screate_simple (1, size, size);
    if (s1<0) goto error;

    /* Create a large compound type */
    if ((t1 = H5Tcreate (H5T_COMPOUND, 100))<0) goto error;
    for (i=0; i<10; i++) {
	sprintf (buf, "member-%d", i);
	if (H5Tinsert (t1, buf, i*sizeof(int), H5T_NATIVE_INT)<0) goto error;
    }

    /* Create a shared version of that type for file #2 */
    if ((t2 = H5Tcopy (t1))<0) goto error;
    if (H5Tshare (f2, t2)<0) goto error;

    /* Create the datasets */
    for (i=0; i<1000; i++) {
	sprintf (buf, "dset%04d", i);
	d1 = H5Dcreate (f1, buf, t1, s1, H5P_DEFAULT);
	if (d1<0) goto error;
	if (H5Dclose (d1)<0) goto error;
	d2 = H5Dcreate (f2, buf, t2, s1, H5P_DEFAULT);
	if (d2<0) goto error;
	if (H5Dclose (d2)<0) goto error;
    }

    /* Close things */
    if (H5Sclose (s1)<0) goto error;
    if (H5Tclose (t1)<0) goto error;
    if (H5Tclose (t2)<0) goto error;
    if (H5Fclose (f1)<0) goto error;
    if (H5Fclose (f2)<0) goto error;

    puts (" PASSED");
    return 0;

 error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_3
 *
 * Purpose:	Tries to create a shared type in a read-only file.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Friday, April  3, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_3 (void)
{
    hid_t	f1, s1, t1, d1;
    hsize_t	size[1] = {1};
    herr_t	status, (*ef)(void*)=NULL;
    void	*ed = NULL;

    printf ("%-70s", "...shared types and read-only files");
    fflush (stdout);

    /*
     * Create the file first since we can't create a file for read-only
     * access.  Add a dataset with a shared type. Then close it and open it
     * for read-only.  The shared type causes a global heap to be allocated.
     */
    f1 = H5Fcreate (TEST_FILE_NAME3, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (f1<0) goto error;
    if ((s1 = H5Screate_simple (1, size, size))<0) goto error;
    if ((t1 = H5Tcopy (H5T_NATIVE_INT))<0) goto error;
    if (H5Tshare (f1, t1)<0) goto error;
    if ((d1 = H5Dcreate (f1, "d1", t1, s1, H5P_DEFAULT))<0) goto error;
    if (H5Sclose (s1)<0) goto error;
    if (H5Dclose (d1)<0) goto error;
    if (H5Fclose (f1)<0) goto error;
    f1 = H5Fopen (TEST_FILE_NAME3, H5F_ACC_RDONLY, H5P_DEFAULT);
    if (f1<0) goto error;
    assert (0==H5Tis_shared (f1, t1));

    /*
     * Create a shared data type.  We know that H5MF_alloc() will fail on a
     * read-only file so there's not much point in trying to create a global
     * heap.  But what happens if we modify a global heap in the cache? So
     * lets try to get a collection loaded into memory by reading the shared
     * type.  Then creating another shared type will just upate memory unless
     * an error is correctly detected.
     */
    if ((d1 = H5Dopen (f1, "d1"))<0) goto error;
    if (H5Dclose (d1)<0) goto error;
    if (H5Eget_auto (&ef, &ed)<0) goto error;
    if (H5Eset_auto (NULL, NULL)<0) goto error;
    status = H5Tshare (f1, t1);
    if (H5Eset_auto (ef, ed)<0) goto error;
    if (status>=0) {
	puts ("*FAILED*");
	puts ("   H5Tshare() should have failed but didn't.");
	goto error;
    }

    /* Close the file */
    if (H5Fclose (f1)<0) goto error;

    puts (" PASSED");
    return 0;

 error:
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
    remove(TEST_FILE_NAME0);
    remove(TEST_FILE_NAME1);
    remove(TEST_FILE_NAME2A);
    remove(TEST_FILE_NAME2B);
    remove(TEST_FILE_NAME3);
}

/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	
 *
 * Return:	Success:	
 *
 *		Failure:	
 *
 * Programmer:	Robb Matzke
 *              Thursday, April  2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main (void)
{
    int		nerrors = 0;
    hid_t	f1, g1;

    /* Make sure diagnostics get emitted before we start doing real stuff */
    f1 = H5Fcreate (TEST_FILE_NAME0, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    g1 = H5Gcreate (f1, "howdy", 0);
    H5Gclose (g1);
    H5Fclose (f1);

    /* Set the error handler */
    H5Eset_auto (display_error_cb, NULL);

    /* Run the tests */
    nerrors += test_1()<0 ? 1 : 0;
    nerrors += test_2()<0 ? 1 : 0;
    nerrors += test_3()<0 ? 1 : 0;

    /* Report results */
    if (nerrors) {
	puts ("*** Shared data type test(s) failed ***");
    } else {
	puts ("All shared data type tests passed.");
	cleanup();
    }
    return nerrors ? 1 : 0;
}
